(in-package :cl-kubernetes)

(defgeneric load-config (source)
  (:documentation
   "Load a YAML configuration file from a file or a list of files."))

;; https://kubernetes.io/docs/concepts/configuration/organize-cluster-access-kubeconfig/#merging-kubeconfig-files
(defun merge-configurations (&optional current new)
  "Destructively merge two k8s configurations.

Adds in CURRENT all the entries (K,V) from NEW for which K is not
currently associated to a value in CURRENT.

When called with zero arguments, produce an empty HASH-TABLE (this is
to satisfy REDUCE and easily build empty configurations)."
  (cond
    ((and current new)
     (maphash (lambda (key new-value)
                (multiple-value-bind (current-value exists-p)
                    (gethash key current)
                  (declare (ignore current-value))
                  (unless exists-p
                    (setf (gethash key current) new-value))))
              new)
     current)
    (t (make-hash-table :test #'equal))))

(defmethod load-config ((sequence sequence))
  "Load and merge a sequence of configuration files."
  ;; merge according to priority rules.
  (reduce #'merge-configurations
          ;; may error during deserializeing
          (mapcar #'load-config
                  ;; ignore non-existing files
                  (delete nil (map 'list #'probe-file sequence)))))

(defmethod load-config ((path pathname))
  "Load a single configuration file."
  (cl-yy:yaml-load-file path))

(defmethod load-config :around ((path pathname))
  "Add an IGNORE restart around LOAD-CONFIG for pathnames."
  (restart-case (call-next-method)
    (ignore ()
      :report "Ignore this configuration file."
      (return-from load-config (merge-configurations)))))

(defun default-config ()
  "Load the default configuration in this environment.

First, try to load configuration from one or more files listed in the
KUBECONFIG environment variable, if that variable is set.

Otherwise, try to read the configuration file from the home user
directory (~/.kube/config).

If this file does not exist, return a default configuration."
  (let ((source (or (split (inter-directory-separator)
                           (getenvp "KUBECONFIG"))
                    (probe-file (merge-pathnames #P".kube/config"
                                                 (user-homedir-pathname))))))
    (if source
        (load-config source)
        (alist-hash-table
         '(("apiVersion" "v1")
           ("kind" "Config"))
         :test #'equal))))

(defmacro define-accessors (name &optional (key (string-downcase name)))
  "Define both NAME and (SETF NAME) to access slot KEY in objects.

KEY is a string that defaults to the NAME symbol (downcased), and
represents the name of the key to access in an object to retrieve its
value."
  (check-type name symbol)
  (check-type key string)
  (let ((object (copy-symbol :object))
        (value (copy-symbol :value)))
    `(progn
       ;; Inline accessors: they are not going to change at runtime.
       (declaim (inline ,name (setf ,name)))

       ;; Reader: allow NIL object.
       (defun ,name (,object)
         ,(format
           nil
           "Get the value associated with ~S in OBJECT, or NIL. ~%~%~

            If OBJECT is NIL, returns NIL."
           key)
         (and ,object (gethash ,key ,object)))

       ;; Writer .
       (defun (setf ,name) (,value ,object)
         ,(format
           nil
           "Set the value associated with ~S in OBJECT to VALUE."
           key)
         (setf (gethash ,key ,object) ,value)))))

;; values
(define-accessors name)
(define-accessors server)
(define-accessors client-key)
(define-accessors client-certificate)
(define-accessors certificate-authority)

;; lists
(define-accessors contexts)
(define-accessors users)
(define-accessors clusters)

;; keys that are references, by names, to objects.
(define-accessors context-name "context")
(define-accessors user-name "user")
(define-accessors cluster-name "cluster")
(define-accessors current-context-name "current-context")

(defun find-object-in-config-list (config list-key name)
    (let ((objects (gethash list-key config)))
        (find-object objects name)))

(defun get-context (config context)
    (find-object-in-config-list config "contexts" context))

(defun get-user (config user)
    (find-object-in-config-list config "users" user))
(declaim (inline find-by-name))
(defun find-by-name (name sequence)
  (find name sequence :test #'equal :key #'name))

(defun get-cluster (config cluster)
    (find-object-in-config-list config "clusters" cluster))

(defun current-context (config)
    (let ((context (get-context config (gethash "current-context" config))))
        (if (not (eq nil context))
            (gethash "context" context)
            nil)))

(defun current-user (config)
    (let ((context (current-context config)))
       (if context
           (gethash "user" (get-user config (gethash "user" context)))
           nil)))

(defun current-cluster (config)
    (let ((context (current-context config)))
       (if context
           (gethash "cluster" (get-cluster config (gethash "cluster" context)))
           nil)))

(defun call-api (path
                 &key
                 (method :GET)
                 (host "http://localhost:8080")
                 (body nil)
                 (content-type "application/json")
                 (user-agent "cl-k8s 0.0.1")
                 (insecure-tls-no-verify nil)
                 (ca-file nil)
                 (client-certificate nil)
                 (client-key nil))
    (let ((uri (concatenate 'string host path)))
        (print body)
        (multiple-value-bind (stream code)
            (drakma:http-request
                uri
                :want-stream t
                :method method
                :content (if body (json:encode-json-to-string body) nil)
                :content-type content-type
                :verify (if insecure-tls-no-verify nil :required)
                :ca-file ca-file
                :certificate client-certificate
                :key client-key
                :user-agent user-agent)
            (values (json:decode-json stream) code))))

(defun call-api-with-config (path
                             config 
                             &key
                             (method :GET)
                             (body nil)
                             (content-type "application/json")
                             (user-agent "cl-k8s 0.0.1"))
    (let ((cluster (current-cluster config))
          (user (current-user config)))
        (call-api
            path
            :method method
            :body body
            :content-type content-type
            :user-agent user-agent
            :host (gethash "server" cluster)
            :ca-file (gethash "certificate-authority" cluster)
            :insecure-tls-no-verify nil ; (get-insecure-tls-no-verify config)
            :client-certificate (gethash "client-certificate" user)
            :client-key (gethash "client-key" user))))


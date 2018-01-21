(in-package :cl-kubernetes)

(defun read-file (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun load-config (path)
    (cl-yy:yaml-simple-load (read-file path)))

; from http://cl-cookbook.sourceforge.net/os.html
(defun getenv (name &optional default)
    #+CMU
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-CMU
    (or
     #+Allegro (sys:getenv name)
     #+CLISP (ext:getenv name)
     #+ECL (si:getenv name)
     #+SBCL (sb-unix::posix-getenv name)
     #+LISPWORKS (lispworks:environment-variable name)
     default))

(defun default-config ()
    (let ((kubeconfig (getenv "KUBECONFIG" nil))
          ; todo make this work on Windows?
          (homekube (concatenate 'string (getenv "HOME") "/.kube/config")))
        (if kubeconfig
            (load-config kubeconfig)
            (if (probe-file homekube)
                (load-config homekube)
                nil ; todo default to localhost:8080 
            )
        )
    )
)

(defun find-object (list name)
    (if list
        (let ((obj (car list)))
            (if (string= name (gethash "name" obj)) obj (find-object (cdr list) name)))
        nil))

(defun find-object-in-config-list (config list-key name)
    (let ((objects (gethash list-key config)))
        (find-object objects name)))

(defun get-context (config context)
    (find-object-in-config-list config "contexts" context))

(defun get-user (config user)
    (find-object-in-config-list config "users" user))

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


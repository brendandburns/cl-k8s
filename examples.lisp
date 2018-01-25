(in-package :cl-kubernetes-examples)

(defun simple-example ()
  (call-api-with-config "/api/v1/namespaces/default" (default-config)))

(defun create-namespace (name)
  (call-api-with-config
   "/api/v1/namespaces"
   (default-config)
   :method :POST
   :body `((:KIND . "Namespace")
           (:API-VERSION . "v1")
           (:METADATA (:NAME . ,name)))))

(defun delete-namespace (name)
  (call-api-with-config
   (concatenate 'string "/api/v1/namespaces/" name)
   (default-config)
   :method :DELETE))

(defun create-pod ()
  (call-api-with-config
   "/api/v1/namespaces/default/pods"
   (default-config)
   :method :POST
   :body '((:KIND . "Pod")
           (:API-VERSION . "v1")
           (:METADATA (:NAME . "nginx"))
           (:SPEC (:CONTAINERS
                   ((:NAME . "nginx")
                    (:IMAGE . "nginx")))
            (:RESTART-POLICY . "Always")
            (:TERMINATION-GRACE-PERIOD-SECONDS . 30)
            (:DNS-POLICY . "ClusterFirst")
            (:SERVICE-ACCOUNT-NAME . "default")))))

(defun delete-pod (name &key (namespace "default"))
  (call-api-with-config
   (concatenate 'string "/api/v1/namespaces/" namespace "/pods/" name)
   (default-config)
   :method :DELETE))


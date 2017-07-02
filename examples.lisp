(load "api.lisp")

(defun simple-example ()
    (call-api-with-config "/api/v1/namespaces/default" (default-config)))

(defun create-namespace (name)
    (let ((ns '((:KIND . "Namespace")
                (:API-VERSION . "v1")
                (:METADATA (:NAME . name)))))
        (call-api-with-config
            "/api/v1/namespaces"
            (default-config)
            :method :POST
            :body ns)))

(defun delete-namespace (name)
    (call-api-with-config
            (concatenate 'string "/api/v1/namespaces/" name)
            (default-config)
            :method :DELETE))

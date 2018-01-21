(defsystem "cl-k8s"
  :depends-on ("drakma"
               "cl-json"
               "cl-yaclyaml"
               "cl-ppcre"
               "alexandria")
  :description "Kubernetes client"
  :author "brendanburns" ;; please adjust
  :version "0.0.1" ;; please adjust
  :license "Apache 2" ;; for example, please adjust
  :components ((:file "packages")
               (:file "api")))

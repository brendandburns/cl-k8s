# cl-k8s

A simple Common Lisp client for Kubernetes.

Very much a work in progress.

Tested in [SBCL](http://www.sbcl.org).
# pre-requisites

Requires that [quicklisp](https://www.quicklisp.org) is installed.

You should install the `cl-k8s` directory in 
`${HOME}/quicklisp/local-projects/cl-k8s`

# usage

```lisp
(load "quicklisp/setup.lisp")
(ql:quickload :cl-k8s)

(defun simple-example ()
    (k8s:call-api-with-config "/api/v1/namespaces/default" (k8s:default-config)))
```

Note that not all `kubeconfig` are supported, specifically, inline certificates,
keys and ca-authorities are not supported currently.

See [examples.lisp](examples.lisp) for more examples.

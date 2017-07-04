# cl-k8s

A simple Common Lisp client for Kubernetes.

Very much a work in progress.

Tested in [SBCL](http://www.sbcl.org).
# pre-requisites

Requires that [quicklisp](https://www.quicklisp.org) is installed.

# usage

```lisp
(load "api.lisp")

(defun simple-example ()
    (call-api-with-config "/api/v1/namespaces/default" (default-config)))
```

See [examples.lisp](examples.lisp) for more examples.

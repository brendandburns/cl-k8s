# cl-k8s

A simple Common Lisp client for Kubernetes.

Very much a work in progress.

Tested in [SBCL](https://www.sbcl.org).

# usage

```lisp
(load "api.lisp")

(defun simple-example ()
    (call-api-with-config "/api/v1/namespaces/default" (default-config)))
```

See [examples.lisp](examples.lisp) for more examples.

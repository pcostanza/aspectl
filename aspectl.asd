#+scl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "AspectL is currently not supported in Scieneer Common Lisp."))

(asdf:defsystem #:aspectl
  :name "AspectL"
  :description "AspectL is a library that provides some experimental aspect-oriented extensions for Common Lisp / CLOS. AspectL is deprecated, because the pointcut functionality of AspectL does not make a lot of sense in Common Lisp, and the support for dynamically scoped generic functions has been replaced with much better mechanisms in ContextL."
  :author "Pascal Costanza"
  :version "0.75.0"
  :licence "MIT-style license"
  :depends-on ("closer-mop" "contextl" #-lispworks "lw-compat")
  :components ((:file "al-packages")
               (:file "al-clos-mop"         :depends-on ("al-packages"))
               (:file "al-pointcuts"        :depends-on ("al-packages"))
               (:file "al-mixins"           :depends-on ("al-packages" "al-clos-mop" "al-pointcuts"))
               (:file "al-special-class"    :depends-on ("al-packages" "al-mixins"))
	       #-(or cmu mcl)
               (:file "al-special-function" :depends-on ("al-packages" "al-clos-mop"))))

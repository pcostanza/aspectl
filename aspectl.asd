#+scl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "AspectL is currently not supported in Scieneer Common Lisp."))

(asdf:defsystem #:aspectl
  :name "AspectL"
  :description "AspectL is a library that provides some experimental aspect-oriented extensions for Common Lisp / CLOS. AspectL is deprecated, because the pointcut functionality of AspectL does not make a lot of sense in Common Lisp, and the support for dynamically scoped generic functions has been replaced with much better mechanisms in ContextL."
  :author "Pascal Costanza"
  :version "0.75.0"
  :licence "
Copyright (c) 2005 - 2013 Pascal Costanza

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
"
  :depends-on ("closer-mop" "contextl" #-lispworks "lw-compat")
  :components ((:file "al-packages")
               (:file "al-clos-mop"         :depends-on ("al-packages"))
               (:file "al-pointcuts"        :depends-on ("al-packages"))
               (:file "al-mixins"           :depends-on ("al-packages" "al-clos-mop" "al-pointcuts"))
               (:file "al-special-class"    :depends-on ("al-packages" "al-mixins"))
	       #-(or cmu mcl)
               (:file "al-special-function" :depends-on ("al-packages" "al-clos-mop"))))

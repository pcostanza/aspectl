;;;;
;;;; AspectL
;;;;
;;;; Copyright (c) 2005, 2006 Pascal Costanza
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;;; sell copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
;;;;

(in-package :cl-user)

(defpackage #:aspectl
  (:documentation "All AspectL definitions in one package.")
  (:nicknames #:al)
  (:use #:closer-common-lisp #:contextl #:lispworks)
  (:export

   ;; A few MOP-related utility functions. The wrappers for
   ;; the CLOS MOP from previous versions of AspectL are removed.
   ;; Use Closer to MOP instead.
   
   #:the-class #:the-direct-slot-definition #:the-effective-slot-definition
   #:defmethod* #:*generic-function*

   ;; Provides generic pointcuts that are containers for join points
   ;; and aspect weavers. Generic pointcuts are a generalization of pointcuts in
   ;; other AOP languages, similar to generic functions being a generalization of
   ;; methods in other OOP languages. This means that join points can be defined
   ;; in a distributed fashion if needed, contrary to the requirement to define
   ;; pointcuts in a single place, as is the case in other AOP approaches.

   ;; This package provides the classes pointcut, join-point and aspect-weaver,
   ;; together with the accessors point-cut-name, point-cut-join-points,
   ;; point-cut-aspect-weavers, join-point-name, join-point-args, aspect-weaver-name,
   ;; and aspect-weaver-function. The functional interface consists of ensure-pointcut,
   ;; and add-..., remove-... and find-... functions. The user-level macros are
   ;; define-pointcut, define-join-point and define-aspect-weaver.
   
   #:pointcut #:find-pointcut
   #:ensure-pointcut #:define-pointcut
   #:point-cut-name #:point-cut-join-points #:point-cut-aspect-weavers
   #:join-point #:aspect-weaver
   #:add-join-point #:find-join-point
   #:remove-join-point #:define-join-point
   #:join-point-name #:join-point-args
   #:add-aspect-weaver #:find-aspect-weaver
   #:remove-aspect-weaver #:define-aspect-weaver
   #:aspect-weaver-name #:aspect-weaver-function

   ;; Provides destructive mixins, i.e. functions that allow for incremental
   ;; modification of existing classes. The functions class-add, class-remove and class-set
   ;; enable setting class options. Likewise, slot-add, slot-remove and slot-set enable
   ;; setting slot options. The class/slot for which these changes should take effect are
   ;; either bound to the special variables *the-class* / *the-slot*, or they are passed
   ;; as parameters to those functions. The special variables are bound via with-class
   ;; and with-slot macros that collect all the requested changes and perform them at
   ;; once as soon as the control flow exits those macros.

   #:*the-class* #:*the-slot* 
   #:class-options #:slot-options
   #:get-class-option #:finalize-class-option
   #:get-slot-option #:default-slot-option #:finalize-slot-option
   #:class-add #:class-remove #:class-set
   #:slot-add #:slot-remove #:slot-set
   #:with-class #:with-slot

   ;; Provides a framework for dealing with generalized places that can
   ;;  be dynamically rebound (i.e., whose bindings have dynamic extent).

   ;; The main macros to make use of special places are dletf and dletf*.
   ;; Any place / accessor can be used in conjunction with dletf/dletf*, provided they
   ;; adhere to the following simple protocol: Instead of storing values directly, they
   ;; have to store special symbols, as created by make-special-symbol, and bind the
   ;; values to those symbols via (setf symbol-value). When *symbol-access* is bound
   ;; to a non-nil value, they have to write or read the symbol as such instead of
   ;; the actual symbol value. The special symbols stored in the special places can
   ;; be created lazily on demand, but should remain the same after initialization.

   #:make-special-symbol #:special-symbol-p
   #:*symbol-access* #:with-symbol-access #:without-symbol-access
   #:checked-progv #:dletf #:dletf*
   
   ;; The metaobject class special-function allows for definition of methods with
   ;; dynamic scope (dynamic extent). A new dynamic scope for a special function
   ;; is created with function call-with-special-function-scope or with the macro
   ;; with-special-function-scope. Use aspectl.clos-mop:defmethod* as a replacement
   ;; for defmethod. (Currently, special functions are not supported in CMUCL and MCL.)

   #:special-class ;; provided for backwards compatibility
   .
   #+(or cmu mcl) ()
   #-(or cmu mcl)
   (#:special-function
    #:special-method 
    #:special-method-combination #:dynamic
    #:define-special-function #:special-function-caller
    #:call-with-special-function-scope
    #:with-special-function-scope)))

(defpackage #:aspectl-user
  (:documentation "A default user environment for AspectL.")
  (:nicknames #:al-user)
  (:use #:closer-common-lisp #:aspectl))

#+lispworks
(progn
  (editor:setup-indent "define-join-point" 1 2 19)
  (editor:setup-indent "defmethod*" 2 2 12))

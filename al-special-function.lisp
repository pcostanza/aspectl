;;;;
;;;; AspectL
;;;;
;;;; Copyright (c) 2005, 2006, 2009 Pascal Costanza
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

(in-package #:aspectl)

#+(or cmu mcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Sorry, this Common Lisp implementation doesn't support special functions."))

;; Dynamic scoping for special functions is achieved by adding
;; another dummy parameter to its argument list that is specialized on
;; classes that represent the current dynamic scope. dynamic is both a
;; placeholder class for the current dynamic scope and the class that
;; actually represents global scope.

(defclass dynamic ()
  ()
  (:documentation
   "A placeholder class for the current dynamic scope. Use it only for
    declaration purposes, as described in special-function.
    Otherwise, the consequences are undefined."))

(ensure-finalized (find-class 'dynamic))

(defun next-dynamic-scope (dynamic-scope)
  (ensure-finalized
   (make-instance 'standard-class :direct-superclasses (list dynamic-scope))))

(defun unlink-dynamic-scope (dynamic-scope)
  (let ((superclass (car (class-direct-superclasses dynamic-scope))))
    (remove-direct-subclass superclass dynamic-scope)))

(defclass special-method (standard-method)
  ())

;; a few helper methods for special-method-combination

(defun check-qualifiers (qualifiers kind allowed)
  (let ((counts))
    (dolist (qualifier qualifiers)
      (incf (getf counts qualifier 0)))
    (when kind
      (unless (eql (getf counts kind) 1)
        (return-from check-qualifiers nil)))
    (loop for (key value) on counts by #'cddr
          always (and (member key allowed)
		      (<= value 1)))))

(defun around/override-p (qualifiers)
  (check-qualifiers qualifiers :around '(:around :override)))

(defun before/override-p (qualifiers)
  (check-qualifiers qualifiers :before '(:before :override)))

(defun after/override-p (qualifiers)
  (check-qualifiers qualifiers :after '(:after :override)))

(defun primary/override-p (qualifiers)
  (check-qualifiers qualifiers nil '(:override)))

(define-method-combination special-method-combination ()
  ((around  around/override-p)
   (before  before/override-p)
   (after   after/override-p)
   (primary primary/override-p :required t))
  "Extends the standard method combination with another qualifier :override. By default,
   call-next-method refers to methods of the surrounding scope, even when their specializers
   are the same as those of the current method. The qualifier :override hides those methods
   for the current dynamic scope."
  (flet ((collapse-methods (method-list)
           (loop for current-method in method-list
                 unless (some (lambda (lower-method)
                                (and (member :override (method-qualifiers lower-method))
                                     (mapcar #'equal
                                             (cdr (method-specializers lower-method))
                                             (cdr (method-specializers current-method)))))
                              result-methods)
                 collect current-method into result-methods
                 finally (return result-methods)))
         (call-methods (methods)
           (mapcar (lambda (method) `(call-method ,method)) methods)))
    (setf around  (collapse-methods around)
          before  (collapse-methods before)
          after   (collapse-methods after)
          primary (collapse-methods primary))
    (let ((form (if (or before after (rest primary))
                  `(multiple-value-prog1
                       (progn ,@(call-methods before)
                         (call-method ,(first primary) ,(rest primary)))
                     ,@(call-methods (reverse after)))
                  `(call-method ,(first primary)))))
      (if around `(call-method ,(first around)
                               (,@(rest around)
                                (make-method ,form)))
        form))))

(defclass special-function (standard-generic-function)
  ((dynamic-scope :initform (let ((special-symbol (make-special-symbol)))
                              (setf (dynamic-symbol-value special-symbol)
                                    (find-class 'dynamic))
                              special-symbol)))
  (:documentation
   "Allows for definition of methods with dynamic scope (dynamic extent). By default,
    :method-class is set to special-method and :method-combination is set to
    special-method-combination. 

    Global scope and dynamic scope is distinguished via the specializer of the first
    parameter of a method. If it is t it refers to the global scope; if it is dynamic
    it refers to the current dynamic scope. The actual object passed as that first parameter
    should not be stored or passed anywhere else, and should not be used in any other way
    except for declaring the scope of the special method. The class dynamic, or the actual class
    of the first parameter passed to a special method, should not be used for creating instances,
    and should not be used in any other way except for declaring the scope of the special method.
    Otherwise, the consequences are undefined.

    There exists only one exception: If call-next-method is used to pass a modified parameter
    list to the next method, one must pass as a first parameter what the method has received as
    its first parameter.

    The define-special-function form for a special function does not mention the implicit scope
    parameter. The implicit scope parameter is also not mentioned when calling a function.
    The methods in a define-special-function form can only be defined for the global scope, and
    must mention the scope parameter.

    If the first parameter of a special method is specialized on dynamic, that method must
    currently be defined through defmethod* instead of defmethod. (See package aspectl.clos-mop.)
    This circumvents the problem that some defmethod implementations do not correctly use
    make-method-lambda as specified in the MOP. (The method initialization protocol of special
    methods must know about the generic function to which the method is about to be added.)

    Example:

    (define-special-function some-function (x y z)
      (:method ((scope t) (x number) (y number) (z number))
       (+ x y z)))

    (defmethod some-function ((scope t) x y z)
      (+ x y z))

    (defmethod* some-function :before ((scope dynamic) x y z)
      (print \"I am about to perform some-function.\"))

    (defmethod* some-function :override :around
      ((scope dynamic) x y z)
      (call-next-method scope (1+ x) (1+ y) (1+ z)))

    (some-function 1 2 3)")
  (:default-initargs
   :method-class (find-class 'special-method)
   :method-combination (find-method-combination
                        (class-prototype (find-class 'special-function))
                        'special-method-combination '()))
  (:metaclass funcallable-standard-class))

(defun dynamic-scope (function)
  "Determines the current dynamic scope of a special funcion."
  (if *symbol-access* (slot-value function 'dynamic-scope)
    (dynamic-symbol-value (slot-value function 'dynamic-scope))))

(defmethod initialize-instance :around
    ((gf special-function)
     &rest initargs
     &key (lambda-list '() lambda-list-p)
     (argument-precedence-order (required-args lambda-list)))
  (assert lambda-list-p (lambda-list lambda-list-p)
	  "The special function ~S must be initialized with a specified lambda list." gf)
  (apply #'call-next-method
	 gf
	 :lambda-list
	 `(scope ,@lambda-list) ; the dynamic scope is the first in the lambda list...
	 :argument-precedence-order
	 `(,@argument-precedence-order scope) ; ...but last in the precedence order
	 initargs))

(defmethod reinitialize-instance :around
  ((gf special-function)
   &rest initargs
   &key (lambda-list '() lambda-list-p)
   (argument-precedence-order (when lambda-list-p (required-args lambda-list))))
  (if lambda-list-p
    (apply #'call-next-method
           gf
           :lambda-list `(scope ,@lambda-list)
           :argument-precedence-order `(,@argument-precedence-order scope)
           initargs)
    (call-next-method)))

#-(or allegro clisp)
(defmethod make-method-lambda
           ((gf special-function)
            (method special-method)
            lambda-expression environment)
  (declare (ignore lambda-expression environment))
  (multiple-value-bind
      (expression initargs)
      (call-next-method)
    (values expression (list* :gf gf initargs))))

(defmethod initialize-instance :around
  ((method special-method) &rest initargs
   &key specializers (gf *generic-function*))
  (let ((dynamic-scope-p (member (find-class 'dynamic) specializers)))
    (when dynamic-scope-p
      (assert gf (gf)
        "Generic function for initialization of special method ~S not specified." method))
    (apply #'call-next-method
           method
           :specializers (if dynamic-scope-p
                           (substitute (dynamic-scope gf)
                                       (find-class 'dynamic)
                                       specializers)
                           specializers)
           initargs)))

(defmethod find-method ((gf special-function)
                        qualifiers specializers &optional (errorp t))
  "Pass (find-class 'dynamic) as the first specializer if you want to
   find a special method in the current dynamic environment. Otherwise,
   pass (find-class 't)."
  (call-next-method gf qualifiers
                    (substitute (dynamic-scope gf)
                                (find-class 'dynamic)
                                specializers)
                    errorp))

(defmacro define-special-function (function-name lambda-list &body options)
  (let ((definer (assoc :definer options))
        (generic-function-class (assoc :generic-function-class options)))
    (unless definer
      (error "A ~S form must mention the :definer option." 'define-special-function))
    (when generic-function-class
      (unless (subtypep (find-class (cadr generic-function-class))
                        (find-class 'special-function))
        (error "The class named by the :generic-function-class option in a ~S form must be a subclass of ~S."
               'define-special-function 'special-function)))
    `(progn
       (defgeneric ,(cadr definer) ,lambda-list
         (:generic-function-class ,(if generic-function-class
                                     (cadr generic-function-class)
                                     'special-function))
         #+(or clozure clisp)
         (:method-class ,(or (cadr (assoc :method-class options))
                             'special-method))
         #+(or clozure clisp)
         (:method-combination ,(or (cadr (assoc :method-combination options))
                                   'special-method-combination))
         ,@(remove-if (lambda (option)
                        (member (car option) '(:definer :generic-function-class
                                               #+(or clozure clisp) :method-class
                                               #+(or clozure clisp) :method-combination)))
                      options))
       (defun ,function-name (&rest args)
         (apply (function ,(cadr definer)) 
                (class-prototype (dynamic-scope (function ,(cadr definer))))
                args))
       (declaim (inline ,function-name))
       (function ,(cadr definer)))))

(defun special-function-caller (special-function)
  (lambda (&rest args)
    (apply special-function
           (class-prototype (dynamic-scope special-function))
           args)))

(defun call-with-special-function-scope (functions lambda)
  "Execute the function lambda in a new dynamic environment for
   the special functions."
  (let ((dynamic-scope-symbols
         (with-symbol-access (mapcar #'dynamic-scope functions)))
        (next-dynamic-scopes
         (mapcar (lambda (function) (next-dynamic-scope (dynamic-scope function)))
                 functions)))
    (unwind-protect
        (special-symbol-progv
            dynamic-scope-symbols
            next-dynamic-scopes
          (funcall lambda))
      (loop for function in functions
            for dynamic-scope in next-dynamic-scopes
            do (loop for method in (generic-function-methods function)
                     when (eq (first (method-specializers method))
                              dynamic-scope)
                     do (remove-method function method)))
      (mapc #'unlink-dynamic-scope next-dynamic-scopes))))

(defmacro with-special-function-scope (functions &body body)
  "Execute body in a new dynamic environment for the special functions."
  `(call-with-special-function-scope
    (mapcar #'fdefinition ',functions)
    (lambda () ,@body)))

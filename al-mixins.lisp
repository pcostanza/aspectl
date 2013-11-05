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

(in-package #:aspectl)

(defgeneric class-options (class)
  (:method ((class standard-class))
   '(:direct-slots :direct-superclasses
     #+(or lispworks4 lispworks5 lispworks6.0) :default-initargs
     #-(or lispworks4 lispworks5 lispworks6.0) :direct-default-initargs
     :documentation :metaclass)))

(defgeneric slot-options (class)
  (:method ((class standard-class))
   '(:name :initform :initfunction :initargs
     :readers :writers
     :documentation :allocation :type)))

(defgeneric get-slot-option (class slot option)
  (:documentation "Determine the value for some slot option.")
  (:method ((class standard-class)
            (slot standard-slot-definition)
            (option symbol))  
   (ccase option
     (:name          (slot-definition-name slot))
     (:initform      (slot-definition-initform slot))
     (:initfunction  (slot-definition-initfunction slot))
     (:initargs      (slot-definition-initargs slot))
     (:readers       (slot-definition-readers slot))
     (:writers       (slot-definition-writers slot))
     (:documentation (documentation slot t) nil)
     (:allocation    (slot-definition-allocation slot))
     (:type          (slot-definition-type slot)))))

(defgeneric default-slot-option (class option &optional name)
  (:documentation "Determine a default value for some slot option.")
  (:method ((class standard-class) (option symbol) &optional name)
   (ccase option
     (:name name)
     ((:initform :initfunction :initargs
       :readers :writers :documentation) nil)
     (:allocation :instance)
     (:type t))))

(defgeneric finalize-slot-option (class option value)
  (:documentation "Process a slot option before it is passed to the MOP.")
  (:method ((class t) (option symbol) value) value))

;; helper functions

(defun make-slot-spec-alist (class)
  (loop for slot in (class-direct-slots class)
        collect (cons (slot-definition-name slot)
                      (loop for option in (slot-options class)
                            collect (cons option (get-slot-option class slot option))))))

(defun finalize-slot-spec-alist-plist (class alist)
  (loop for (nil . slot-options) in (remove-duplicates alist :key #'car :test #'eq :from-end t)
        collect (loop for (option . value) in slot-options
                      collect option
                      collect (finalize-slot-option class option value))))

(defun make-default-slot-spec (class name)
  (loop for option in (slot-options class)
        collect (cons option (default-slot-option class option name))))

(defgeneric get-class-option (class option)
  (:documentation "Determine the value for some class option.")
  (:method ((class standard-class) (option symbol))
   (ccase option
     (:name                    (class-name class))
     (:direct-slots            (make-slot-spec-alist class))
     (:direct-superclasses     (mapcar #'class-name (class-direct-superclasses class)))
     (#+(or lispworks4 lispworks5 lispworks6.0)
      :default-initargs
      #-(or lispworks4 lispworks5 lispworks6.0)
      :direct-default-initargs (class-direct-default-initargs class))
     (:documentation           (documentation class t))
     (:metaclass               (class-name (class-of class))))))

(defgeneric finalize-class-option (class option value)
  (:documentation "Process a class option before it is passed to the MOP.")
  (:method ((class t) (option symbol) value) value))

(defmethod finalize-class-option
           ((class standard-class)
            (option (eql :direct-slots))
            value)
  (finalize-slot-spec-alist-plist class value))

;; helper functions

(defun make-class-spec-alist (class)
  (loop for option in (class-options class)
        collect (cons option (get-class-option class option))))

(defun finalize-class-spec-alist-plist (class alist)
  (loop for (option . value) in alist
        collect option
        collect (finalize-class-option class option value)))

(defvar *the-class* nil
  "Holds the current class metaobject for inspection purposes.")
(defvar *class-spec*) ; the current class specification alist

(defvar *the-slot* nil
  "Holds the current slot definition metaobject for inspection purposes.")
(defvar *slot-name* nil) ; the current slot's name
(defvar *slot-spec*)     ; the current slot specification alist

(defmacro with-class (class &body body)
  "Creates an environment for modifying a class.
   Accepts symbols and class metaobjects."
  (with-unique-names (new-class)
    (rebinding (class)
      `(let* ((*the-class*  (the-class ,class))
	      (*class-spec* (make-class-spec-alist *the-class*))
	      (*the-slot* nil))
         ,@body
         (handler-bind
             ((warning #'muffle-warning))
           (let ((,new-class (apply #'ensure-class
                                    (class-name *the-class*)
                                    (finalize-class-spec-alist-plist *the-class* *class-spec*))))
             #+(or cmu clozure mcl) (finalize-inheritance ,new-class)
             #-(or cmu clozure mcl) (ensure-finalized ,new-class)
             ,new-class))))))

(defmacro with-slot (slot &body body)
  "Creates an environment for modifying a slot.
   Must be embedded inside a with-class environment,
   and accepts symbols and slot definition metaobjects."
  (rebinding (slot)
    `(progn
       (assert (not (null *the-class*)))
       (assert (not (null *class-spec*)))
       (let* ((*the-slot* (the-direct-slot-definition *the-class* ,slot))
              (*slot-name* (if (symbolp ,slot) ,slot (slot-definition-name *the-slot*)))
              (*slot-spec* (or (cdr (assoc *slot-name* (cdr (assoc :direct-slots *class-spec*))))
                               (make-default-slot-spec *the-class* *slot-name*))))
         ,@body
         (push (cons *slot-name* *slot-spec*)
               (cdr (assoc :direct-slots *class-spec*)))))))

(defgeneric class-add (option value &rest args)
  (:method (option value &rest args)
   (apply #'class-add-using-class *the-class* option value args)))

(defgeneric class-add-using-class (class option value &key &allow-other-keys)
  (:documentation "Pushes information to the value of a class option.")
  (:method ((class standard-class)
            (option symbol) value
            &key (test #'eql) (key #'identity) &allow-other-keys)
   (push (cons option (adjoin value (cdr (assoc option *class-spec*)) :test test :key key))
         *class-spec*)))

(defmethod class-add-using-class
           ((class standard-class)                                  
            (option (eql :direct-slots))
            (value symbol)
            &key &allow-other-keys)
  "Ensures that a slot of this name exists.
   If it doesn't exist, it is created with default slot option values."
  (with-slot value))

(defmethod class-add-using-class
           ((class standard-class)
            (option (eql :direct-slots))
            (value list)
            &key &allow-other-keys)
  "Modifies or creates a slot with additional/changed slot options.
   Slot options are defined as, for example, in DEFCLASS.
   Caution: :initform and :initfunction are not synced!"
  (with-slot (pop value)
    (loop for (key val) on value by #'cddr
          do (case key
               (:initarg  (slot-add :initargs val))
               (:reader   (slot-add :readers val))
               (:writer   (slot-add :writers val))
               (:accessor (slot-add :accessors val))
               (otherwise (slot-set key val))))))

(defgeneric class-remove (option value &rest args)
  (:method (option value &rest args)
   (apply #'class-remove-using-class *the-class* option value args)))

(defgeneric class-remove-using-class (class option value &key &allow-other-keys)
  (:documentation "Removes information from the value of a class option.")
  (:method ((class standard-class)
            (option symbol)
            value
            &key (test #'eql) (key #'identity) &allow-other-keys)
   (when-let (cons (assoc option *class-spec*))
     (removef (cdr cons) value :test test :key key))))

(defmethod class-remove-using-class
           ((class standard-class)
            (option (eql :direct-slots))
            (value symbol)
            &key &allow-other-keys)
  "Removes a slot from a class."
  (when-let (cons (assoc :direct-slots *class-spec*))
    (removef (cdr cons) value :key #'car)))

(defmethod class-remove-using-class
           ((class standard-class)
            (option (eql :direct-slots))
            (value list)
            &key &allow-other-keys)
  "Modifies or creates (!) a slot with slot options to be removed.
   Slot options are defined as, for example, in DEFCLASS.
   Caution: :initform and :initfunction are not synced!"
  (with-slot (pop value)
    (loop for (key val) on value by #'cddr
          do (case key
               (:initarg   (slot-remove :initargs val))
               (:reader    (slot-remove :readers val))
               (:writer    (slot-remove :writers val))
               (:accessors (slot-remove :accessors val))
               (otherwise  (slot-set key (default-slot-option class key val)))))))

(defgeneric class-set (option value &rest args)
  (:method (option value &rest args)
   (apply #'class-set-using-class *the-class* option value args)))

(defgeneric class-set-using-class (class option value &key &allow-other-keys)
  (:documentation "Sets the value of a class option.")
  (:method ((class standard-class)
            (option symbol)
            value
            &key &allow-other-keys)
   (push (cons option value) *class-spec*)))

(defgeneric slot-add (option value &rest args)
  (:method (option value &rest args)
   (apply #'slot-add-using-class *the-class* option value args)))

(defgeneric slot-add-using-class (class option value &key &allow-other-keys)
  (:documentation "Pushes information to the value of a slot option.")
  (:method ((class standard-class)
            (option symbol)
            value
            &key (test #'eql) (key #'identity) &allow-other-keys)
   (push (cons option (adjoin value (cdr (assoc option *slot-spec*)) :test test :key key))
         *slot-spec*)))

(defmethod slot-add-using-class
           ((class standard-class)
            (option (eql :accessors))
            value
            &key &allow-other-keys)
  "Adds an accessor to a slot."
  (slot-add :readers value)
  (slot-add :writers `(setf ,value) :test #'equal))

(defgeneric slot-remove (option value &rest args)
  (:method (option value &rest args)
   (apply #'slot-remove-using-class *the-class* option value args)))

(defgeneric slot-remove-using-class (class option value &key &allow-other-keys)
  (:documentation "Removes information from the value of a slot option.")
  (:method ((class standard-class)
            (option symbol)
            value
            &key (test #'eql) (key #'identity) &allow-other-keys)
   (when-let (cons (assoc option *slot-spec*))
     (removef (cdr cons) value :test test :key key))))

(defmethod slot-remove-using-class
           ((class standard-class)
            (option (eql :accessors))
            value
            &key &allow-other-keys)
  "Removes an accessor from a slot."
  (slot-remove :readers value)
  (slot-remove :writers `(setf ,value) :test #'equal))

(defgeneric slot-set (option value &rest args)
  (:method (option value &rest args)
   (apply #'slot-set-using-class *the-class* option value args)))

(defgeneric slot-set-using-class (class option value &key &allow-other-keys)
  (:documentation "Sets the value of a slot option.")
  (:method ((class standard-class)
            (option symbol)
            value
            &key &allow-other-keys)
   (push (cons option value) *slot-spec*)))

(defmethod slot-set-using-class
           ((class standard-class)
            (option (eql :accessors))
            value
            &key &allow-other-keys)
  "Sets the accessors of a slot."
  (slot-set :readers value)
  (slot-set :writers (mapcar (lambda (val) `(setf ,val)) value)))

;; All add/remove/set methods defined above accept additional keyword arguments
;; to set the class and the slot they work on. They are expanded by the following
;; aspects to appropriate uses of with-class/with-slot. This serves also as an
;; additional illustration of how AspectL aspects work.

(define-join-point with-class class-add 'to-class)
(define-join-point with-class class-remove 'from-class)
(define-join-point with-class class-set 'for-class)

(define-aspect-weaver with-class accept-class-arg
    (aspect-weaver join-point class-arg)
  (declare (ignore aspect-weaver))
  (ensure-method (fdefinition (join-point-name join-point))
                 `(lambda (option value &key (,class-arg *the-class*) &allow-other-keys)
                    (declare (ignorable option value))
                    (assert (not (null ,class-arg)))
                    (let ((class (the-class ,class-arg)))
                      (if (not (eq class *the-class*))
                        (with-class class (call-next-method))
                        (call-next-method))))
                 :qualifiers '(:around)))

(define-join-point with-slot slot-add 'for-class 'to-slot)
(define-join-point with-slot slot-remove 'for-class 'from-slot)
(define-join-point with-slot slot-set 'for-class 'for-slot)

(define-aspect-weaver with-slot accept-class/slot-arg
    (aspect-weaver join-point class-arg slot-arg)
  (declare (ignore aspect-weaver))
  (ensure-method (fdefinition (join-point-name join-point))
                 `(lambda (option value &key (,class-arg *the-class*) (,slot-arg *slot-name*) &allow-other-keys)
                    (declare (ignorable option value))
                    (assert (not (null ,class-arg)))
                    (let* ((class (the-class ,class-arg))
                           (slot (the-direct-slot-definition class ,slot-arg))
                           (slot-name (if (symbolp ,slot-arg) ,slot-arg
                                        (slot-definition-name slot))))
                      (cond ((not (eq class *the-class*))
                             (with-class class
                               (with-slot ,slot-arg
                                 (call-next-method))))
                            ((not (eq slot-name *slot-name*))
                             (with-slot ,slot-arg (call-next-method)))
                            (t (call-next-method)))))
                 :qualifiers '(:around)))

(asdf:oos 'asdf:load-op :aspectl :force t)

(in-package :al-user)

(setf (find-class 'person) nil)

(defclass person ()
  ((name :accessor name
         :initarg :name))
  (:metaclass special-class))

(defparameter *p* (make-instance 'person :name "Dr. Jekyll"))

(assert (equal (name *p*) "Dr. Jekyll"))

(handler-bind
    ((error (lambda (error)
              (slot-set :special t :for-class 'person :for-slot 'name)
              (continue error))))
  (dletf (((name *p*) "Mr. Hide"))
    (assert (equal (name *p*) "Mr. Hide"))))

(assert (equal (name *p*) "Dr. Jekyll"))

#-(or cmu mcl)
(progn
  (define-special-function f (x)
    (:definer f*))
  (defmethod f* ((scope t) x)
    x)
  (assert (eql (f 5) 5))
  (with-special-function-scope (f*)
    (defmethod* f* ((scope dynamic) x)
      (1+ (call-next-method)))
    (assert (eql (f 10) 11)))
  (assert (eql (f 100) 100)))

(print :done)

#+allegro (excl:exit)
#+clozure (ccl:quit)
#+cmu (ext:quit)
#+ecl (si:quit)
#+sbcl (sb-ext:quit)

(asdf:oos 'asdf:load-op :aspectl)

(in-package :al-user)

(define-special-function f (x)
  (:definer f*))

(defmethod f* ((scope t) x)
  (if (zerop x)
      0
    (+ (* x x)
       (f (1- x)))))

(assert (eql (f 5) 55))

(with-special-function-scope (f*)
  (defmethod* f* :around ((scope dynamic) x)
    (1+ (with-special-function-scope (f*)
          (defmethod* f* :override :around ((scope dynamic) x)
            (call-next-method))
          (call-next-method))))
  (assert (eql (f 5) 56)))

(assert (eql (f 6) 91))

(print :done)

#+allegro (excl:exit)
#+clozure (ccl:quit)
#+cmu (ext:quit)
#+ecl (si:quit)
#+sbcl (sb-ext:quit)

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

(defun the-class (class)
  "If class is a class, return it. If it is a symbol, find the class."
  (ctypecase class
    (class class)
    (symbol (find-class class))))

(defun the-direct-slot-definition (class slot)
  "If slot is a direct-slot-definition, return it.
   If it is a symbol, find it in (the-class class)."
  (ctypecase slot
    (direct-slot-definition slot)
    (symbol (find slot (class-direct-slots (the-class class))
                  :key #'slot-definition-name))))

(defun the-effective-slot-definition (class slot)
  "If slot is an effective-slot-definition, return it.
   If it is a symbol, find it in (the-class class)."
  (ctypecase slot
    (slot-definition slot)
    (symbol (find slot (class-slots (the-class class))
                  :key #'slot-definition-name))))

(defvar *generic-function* nil
  "Some method objects must know about the generic functions
   to which they are about to be added. Usually, this can be
   achieved by specializing make-method-lambda, but many MOP
   implementations don't handle this correctly. Therefore,
   defmethod* is provided as an alternative to defmethod. It
   binds *generic-function* accordingly.")

(defmacro defmethod* (name &rest args)
  "Some method objects must know about the generic functions
   to which they are about to be added. Usually, this can be
   achieved by specializing make-method-lambda, but many MOP
   implementations don't handle this correctly. Therefore,
   defmethod* is provided as an alternative to defmethod. It
   binds *generic-function* accordingly."
  `(let ((*generic-function* (if (fboundp ',name)
                                 (fdefinition ',name)
                               (ensure-generic-function ',name))))
     (defmethod ,name ,@args)))

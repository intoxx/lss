(in-package #:cl-user)

(uiop:define-package #:lss
  (:use #:cl)
  (:export #:style
           #:defproperty
           #:defproperties
           #:*property-aliases*
           #:*associations*))

(in-package #:lss)

(defmacro defproperty (alias &optional (value "auto"))
  "Return a new LIST containing an association between an ALIAS and its VALUE (defaults to 'auto')."
  `(list ,alias ,value))

(defmacro defproperties (&rest associations)
  "Return a new ALIST of ALIAS and its VALUE."
  (let ((results (list)))
    (loop :for association in associations
          :do (pushnew (funcall (macro-function 'defproperty) ; FIXME: Cache macro-function result
                                `(defproperty ,(first association) ,(second association)) nil)
                       results)
          :finally (pushnew 'list results))
    results))

(defvar *property-aliases* (defproperties (:dp "display")
                                          (:m "margin")
                                          (:mt "margin-top")
                                          (:mr "margin-right")
                                          (:mb "margin-bottom")
                                          (:ml "margin-left")
                                          (:p "padding")
                                          (:list-style "list-style"))
  "ALIST of supported CSS PROPERTY-ALIAS where the CAR is the PROPERTY-ALIAS and the CDR its translation.")

(defparameter *associations* '((:flex (:dp "flex")) (:ml-auto (:ml "auto"))
                               (:mx-auto (:ml "auto" :mr "auto")) ; FIXME: sequential let to reference ml-auto ?
                               (:list-none (:list-style "none")))
  "ALIST of supported LSS SUPER-PROPERTY-ALIAS where the CAR is the SUPER-PROPERTY-ALIAS and the CDR its
translation into PROPERTY-ALIAS.")

(defun make-property-list (&rest args)
  "FIXME"
  (let ((property-list (list)))
    (dolist (arg args (nreverse property-list))
      (if (member arg *associations* :key #'car :test #'eql)
          (setf property-list (concatenate 'list (reverse (cadr (assoc arg *associations* :test #'eql))) property-list))
          (pushnew arg property-list)))))

(defun style (&rest args)
  "Return a STRING containing the compiled CSS declarations, args must be a valid PROPERTY-LIST."
  (let ((plist (apply #'make-property-list (first args)))) ; FIXME: I've added (first args) to make it work with the macro character
    (apply #'compile-declarations
           (loop :for arg in plist
                 :when (keywordp arg)
                   :collect (property->declaration arg (getf plist arg))))))

(defun property->declaration (property-alias &optional (value "auto"))
  "Return a new LIST containing the translated PROPERTY-ALIAS and its value (defaults to 'auto')."
  (list (or (cadr (assoc property-alias *property-aliases*))
            (error (format nil "PROPERTY-ALIAS ~A not found" property-alias)))
        value))

(defun compile-declarations (&rest declarations)
  "Return a STRING containing the inlined CSS for all DECLARATIONS."
  (format nil "~{~{~A: ~A~};~^ ~}" declarations))


(defvar *default-readtable* (copy-readtable))

(set-macro-character #\@
                     #'(lambda (stream char)
                         (declare (ignore char))
                         (setf *default-readtable* (copy-readtable))
                         (set-macro-character #\(
                                              #'(lambda (stream char)
                                                  (declare (ignore char))
                                                  ;; Don't keep this macro character after using it
                                                  (setf *readtable* *default-readtable*)
                                                  ;; Generate the style
                                                  ;; FIXME: style has been updated with (first args) to make it work
                                                  (style (read-delimited-list #\) stream)))
                                              nil)
                         ;; FIXME: check we're in an spinneret block
                         ;; FIXME: when spinneret isn't loaded it returns T
                         #+spinneret
                         :style)
                     nil)

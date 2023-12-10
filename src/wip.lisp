(defun read-style (style-list)
  (dolist (item style-list)
    (format t "~A (type : ~A)~&" item (type-of item))))

(defun gen-style (style-list)
  "font-size: large")

(defvar *default-readtable* (copy-readtable))

;; (unread-char #\( stream))) ; or use peek-char removing the need to unread
;; Usage : @(:size "x-large" :p "3px")
;; FIXME: for conditional macro characters, copy the read table and pop it after.
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
                                                  (gen-style (read-delimited-list #\) stream)))
                                              nil)
                         ;; FIXME: check we're in an spinneret block
                         ;; FIXME: when spinneret isn't loaded it returns T
                         #+spinneret
                         :style)
                     nil)

(defun h1 (&rest args)
  (pprint args))

;;Using a style tag and different styles
(:h1 @style(:size "x-large" :p "3px") "Welcome")

;; Using a class tag and different styles.
;; Requires configuration to output the classes CSS.
;; Output could be done in multiple different ways :
;; - Compile/Run time stream generation (both file and style tag). Distribution has to be handled outside.
;; - Compile time file generation. Distribution has to be handled outside.
;; - Compile time <style> tags generation into <head>. Distribution is handled.
(:h1 @class(:size "x-large" :p "3px" "Welcome")

;; Let the system decides what to use.
;; It requires a configuration to output the css (either as <style> tags or files).
(:h1 @(:size "x-large" :p "3px"] "Welcome")

;; Define local styles
(with-style ((title @(:flex :mx-auto :p "3px"))
             (:section @(:mx-auto :m "5px"))
             ((:section :article) @(:text-base)))
  (spinneret:with-html
    (:h1 @(title :text-lg))
    (:h2 @(title :text-md))))

;; Define global styles
(defstyle title @(:flex :mx-auto :p "3px"))
(defstyle :section @(:mx-auto :m "5px"))
(defstyle (:section :article) @(:text-base))

;; Those should be in an html framework, not directly related to css

(defun home-page ()
  (flet ((container () ((or :section :article) @(:flex :m 5)))
         (title () (:h1 @(:flex :mx-auto :p "3px"))))
    (spinneret:with-html
        (container
         (title @(:text-lg))
         (container
          (title @(:text-md)))))))

(defun home-page ()
  (with-fragments ((title () (:h1 @(:flex :mx-auto :p "3px"))))
    (title)))

;; defpage, defragment

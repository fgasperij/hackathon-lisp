;#!/usr/local/bin/sbcl --script

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "cl-who")
(ql:quickload "hunchentoot")
(ql:quickload "parenscript")

(defpackage :vote (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :vote)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))

(defmacro define-url-fn ((name) &body body)
	 `(progn
	    (defun ,name ()
	      ,@body)
	    (push (create-prefix-dispatcher ,(format nil "/~(~a~).htm" name) ',name) *dispatch-table*)))

; VARS
(defclass counter ()
  ((name  :initarg  :name :accessor name)
   (description  :initarg  :description :accessor description)
   (votes :initform 0 :accessor votes)))

(defvar *counter-actions* nil)
(defmacro def-counter-action (name argname &body body)
  `(progn
  (push ,name *counter-actions*)
  (hunchentoot:define-easy-handler (,(intern name) :uri ,(format nil "/~a.htm" name)) (name)
    (let ((,argname (counter-by-name name)))
      ,@body
      (redirect (format nil "/counter.htm?name=~a" (name ,argname)))))))

(defmacro render-counter-actions (counter)
   `(dolist (action *counter-actions*)
     (htm
       (:a :href (format nil "~a.htm?name=~a" action (name ,counter)) (fmt "~a" action))
       (:br))))

(def-counter-action "up vote" counter
  (incf (votes counter)))

(def-counter-action "down vote" counter
  (decf (votes counter)))

(defvar *counters* nil)

(defun add-counter (name description)
  (push (make-instance 'counter :name name :description description) *counters*))

(defun counters ()
  (copy-list *counters*))

(defun counter-by-name (name)
  (find name *counters* :test #'string-equal
                        :key  #'name))

; PAGES
(defmacro standard-page ((&key title) &body body)
   `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
      :xml\:lang "en"
      :lang "en"
      (:head
       (:meta :http-equiv "Content-Type"
        :content    "text/html;charset=utf-8")
       (:title ,title))
      (:body
       (:div :id "header"
       (:h2 :class "strapline"
        "Give your vote"))
       ,@body))))

; HANDLERS
(define-url-fn (counter-create)
	 (let ((name (parameter "name"))
        (description (parameter "description")))
	   (unless (or (null name) (zerop (length name)))
	     (add-counter name description))
	   (redirect "/")))

(define-url-fn (counter)
	 (let ((counter (counter-by-name (parameter "name"))))
     (standard-page (:title "Vote Lisp")
       (:h2 (fmt "~a" (name counter)))
       (:i (fmt "~a" (description counter)))
       (:p (fmt "It currently has ~d votes" (votes counter)))
       (render-counter-actions counter)
       (:br)
       (:a :href "/" "Back"))))

(hunchentoot:define-easy-handler (index :uri "/") ()
  (standard-page (:title "Vote Lisp")
      (:h1 "Vote Lisp")
      (:ol
        (dolist (counter (counters))
          (htm 
            (:li 
              (:a :href (format nil "counter.htm?name=~a" (name counter)) (fmt "~A" (name counter)))))))
      (:a :href "counter-new.htm" "Add new counter")))

(hunchentoot:define-easy-handler (new :uri "/counter-new.htm") ()
  (standard-page (:title "Vote Lisp")
    (:h1 "Available titles")
    (:form :action "/counter-create.htm" :method "post"
           (:p "Title" (:br)
               (:input :type "text"
                       :name "name"
                       :class "txt"))
           (:p "Description" (:br)
               (:input :type "text"
                       :name "description"
                       :class "txt"))
           (:p (:input :type "submit"
                       :value "Add"
                       :class "btn")))))

(add-counter "Lola" "lolita")
(add-counter "Lolame" "la lola")
(add-counter "Lolitas" "las lolitas")

(read-line *query-io*)

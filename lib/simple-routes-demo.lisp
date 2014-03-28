;;;
;;; https://raw.github.com/vancan1ty/simple-routes/master/simpleroutes-demo.lisp
;;;
;;; As I couldn't make Quicklisp and ASDF to load the project once git
;;; cloned in ~/quicklisp/local-projects/, let's just add a copy here.

(defpackage :simpleroutes-demo
  (:use :common-lisp :hunchentoot :cl-who :simple-routes))

(in-package :simpleroutes-demo)

;;this should point to your static files root
(defvar *file-root* (cl-fad:merge-pathnames-as-directory *default-pathname-defaults* "web/"))

(setf simple-routes:*routeslist*
      (compile-routes 
       ;;html content uris
       (:GET    ""                                      'home-handler)
       (:GET    "/"                                     'home-handler)
       (:GET    "/people"                               'home-handler)
       (:GET    "/people/:first/:last"                  'get-people-handler)
       (:GET    "/people/put/:first/:last/:description" 'put-people-handler)
       (:PUT    "/people/:first/:last/:description"     'put-people-handler)
              ))

;all lat/long numbers are rounded to the hundredths place before insertion or checking...
(defvar *people-hash* (make-hash-table :test #'equalp))
;;add a couple of predefined peoples
(setf (gethash (list "Nikola" "Tesla") *people-hash*) "AC induction motor FTW!")
(setf (gethash (list "Thomas" "Edison") *people-hash*) "preferred DC")

(defun home-handler ()
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:html
     (:h1 "Simple-Routes Demo")
     (:p "Look in simpleroutes-demo.lisp to see underlying code")
     (:table :border 3 
	     (:tr (:th "Operation") (:th "Urlspec") (:th "Description"))
	     (:tr (:td "GET") (:td "/people/:first/:last") (:td "retrieves matching person"))
	     (:tr (:td "PUT") (:td "/people/:first/:last/:description") 
		  (:td "PUTs that name and description to the server"))
	     (:tr (:td "GET") (:td "/people/put/:first/:last/:description") 
		  (:td "convenience accessor for PUT functionality using plain browser functionality"))
	     )
     (:h2 "Current Entries")
     (maphash (lambda (k v) 
		    (htm (:p (print k) ": " (fmt v))))
		  *people-hash*)
)))

(defun get-people-handler (first last)
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:html
     (let ((potentialout (gethash (list first last) *people-hash*)))
       (if potentialout
	   (htm (:p (fmt "name: ~a ~a" first last)) 
		(:p (fmt "description: ~a" potentialout)))
	   (progn
	     (setf (return-code*) +HTTP-NOT-FOUND+)
	     (htm (:p "couldn't find that person!"))))))))

(defun put-people-handler (first last description)
  (with-html-output-to-string (*standard-output* nil)
    (:html
     (:p (fmt "put: ~a " (setf (gethash (list first last) *people-hash*) (url-decode description)))))))


(defvar *macceptor* (make-instance 'simple-routes:simpleroutes-acceptor :port 8080
					  :document-root *file-root*
					  :access-log-destination *terminal-io*
					  :message-log-destination *terminal-io*))
(setf *show-lisp-errors-p* t
      *show-lisp-backtraces-p* t)

;;code below restarts the acceptor every time this file is loaded
(if (hunchentoot::acceptor-shutdown-p *macceptor*)
    (hunchentoot:start *macceptor*)
    (progn
      (hunchentoot:stop *macceptor*)
      (hunchentoot:start *macceptor*)))

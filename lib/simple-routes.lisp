;;;
;;; https://raw.github.com/vancan1ty/simple-routes/master/simple-routes.lisp
;;;
;;; As I couldn't make Quicklisp and ASDF to load the project once git
;;; cloned in ~/quicklisp/local-projects/, let's just add a copy here.

(defpackage :simple-routes
  (:use :common-lisp :cl-ppcre :hunchentoot)
  (:export :compile-routes
	   :simpleroutes-acceptor
	   :simpleroutes-ssl-acceptor
	   :bind-alist-values
           :define-simple-handler))

(in-package :simple-routes)


;;Adds simple-router dispatching to the front of hunchentoot's dispatch table!

(defvar *routeslist* ()
 "should contain routes compiled with routespec-compile or manually entered in compiled form
  incoming requests are matched up against each item in *routeslist* successively, until (and if) a
  matching routespec is found.")

(defclass simpleroutes-acceptor (acceptor)
  ((routes :initarg :routes
           :accessor routes
           :documentation "Routes list."))
  (:documentation
   "This first tries to route requests using simple-router, then falls back
    to hunchentoot's default easy-acceptor."))

#-:hunchentoot-no-ssl
(defclass simpleroutes-ssl-acceptor (simpleroutes-acceptor ssl-acceptor)
  ()
  (:documentation "This is an acceptor that mixes the simpleroutes
  acceptor with SSL connections."))

(defun issymbolstring (str)
  (and (> (length str) 1) (eql (elt str 0) #\:)))

(defun removelast (sequence)
  "removes the last item in sequence IF THE SEQUENCE HAS A LAST ITEM"
  (if (> (length sequence) 0)
      (subseq sequence 0 (1- (length sequence)))
      sequence))

(defun routespec-compile (httpmethod urldef fntocall)
  "httpmethod can be one of :GET :HEAD :POST :PUT :DELETE or :ALL
   urldef is a url definition string sharing *basic* syntax with Ruby on Rails
   fntocall is the function to call in case the this is found to be a match for the request

   this macro returns a list which is meant to be processed by cl-simple routehandler
   example call:
   =>(rtreg :GET ``/home/next/:number'' #'nxthandler) returns
   (:GET \"^/home/next/([^/]*)$\" (NUMBER)
     #<CLOSURE (LAMBDA # :IN MACRO-FUNCTION) {1000F213DB}>)
   the output of this macro can in turn be processed by simple-processor"
  (declare (optimize (debug 3)))
  (let* ((thelist (remove "" (cl-ppcre:split "/" urldef) :test #'equalp)) 
	 (startswithslash (and (> (length urldef) 0) (eql (elt urldef 0) #\/)))
	 (endswithslash (and (> (length urldef) 1) (eql (lastitem urldef) #\/)))
	 (colonitems (reverse 
		      (reduce (lambda (accum nxt) 
				(if (issymbolstring nxt) 
				    (cons nxt accum)
				    accum))
			      thelist :initial-value ())))
	 (theregex (concatenate 'string
				"^"
				(when startswithslash "/")
				(removelast
				 (apply #'concatenate 'string 
					(loop for item in thelist collect
					     (if (issymbolstring item)
						 "([^/]*)/"
						 (concatenate 'string item "/")))))
				(when endswithslash "/")
				"$"))
	 (symstobind (mapcar (lambda (item) (intern (string-upcase (subseq item 1)))) colonitems)))
    `(list ,httpmethod ,theregex (quote ,symstobind) ,fntocall)))

(defmacro compile-routes (&rest routespecs)
  `(list ,@(loop for routespec in routespecs collect
       (apply #'routespec-compile routespec))))

(defun simple-router (request-uri request-type)
  "takes in a request uri and type (:GET, :POST, etc...) and loops through all
   compiled routes in *routeslist*.  If it finds a route that matches
   ,it returns the associated handler and returns true.  otherwise returns false"
  (register-groups-bind (processed-uri) ("^([^?]*)\\??.*" request-uri)
    (loop for compiled-route in *routeslist* do 
	 (destructuring-bind (treqtype tregexp tvars tfntocall) compiled-route
	   (declare (ignore tvars))
	   (multiple-value-bind (regexmatch capturedstrings) (cl-ppcre:scan-to-strings tregexp processed-uri)
	     (declare (ignore regexmatch))
	     (if (and (not (eql capturedstrings nil))
		      (eql treqtype request-type))
		 (progn 
		   (return-from simple-router (apply tfntocall (mapcar #'hunchentoot:url-decode (coerce capturedstrings 'list)))))))))))


(defmethod acceptor-dispatch-request ((acceptor simpleroutes-acceptor) request)
  "The simple request dispatcher which tries to complete the request using simple,
   but otherwise falls back to the hunchentoot defaults *dispatch-table* and easy-acceptor"
  (let ((uri (request-uri request))
	(request-type (hunchentoot:request-method request)))
    (let* ((*routeslist*  (routes acceptor))
           (potentialout (simple-router uri request-type)))
      (or potentialout
	  (call-next-method)))))

(defmacro bind-alist-values (lambda-list alist-expression &rest body)
  "this is intended to be used to access get and post parameters.  example usage
   (bind-alist-values (first second) (hunchentoot:get-parameters*)
		         (list first second))"
  `(destructuring-bind ,lambda-list 
       (mapcar (lambda (varname) 
		 (cdr (assoc (string varname) 
			     ,alist-expression
			     :test #'equalp)) )
	       (quote ,lambda-list))
     ,@body))

(defun lastitem (seq)
  (let ((lindex (- (length seq) 1)))
    (when (> lindex 0)
      (elt seq lindex))))

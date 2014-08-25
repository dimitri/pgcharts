;;;
;;; Parse database connection string
;;;

(in-package #:pgcharts.dburi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun getenv-default (name &optional default)
    "Return the value of the NAME variable as found in the environment, or
     DEFAULT if that variable isn't set"
    (or (uiop:getenv name) default)))

(defrule punct (or #\, #\- #\_)
  (:text t))

(defrule namestring (and (alpha-char-p character)
			 (* (or (alpha-char-p character)
				(digit-char-p character)
				punct)))
  (:text t))

(defrule dsn-port (and ":" (* (digit-char-p character)))
  (:destructure (colon digits &aux (port (coerce digits 'string)))
		(declare (ignore colon))
		(list :port (if (null digits) digits
				(parse-integer port)))))

(defrule doubled-at-sign (and "@@") (:constant "@"))
(defrule doubled-colon   (and "::") (:constant ":"))
(defrule password (+ (or (not "@") doubled-at-sign)) (:text t))
(defrule username (+ (or (not (or ":" "@")) doubled-at-sign doubled-colon))
  (:text t))

(defrule dsn-user-password (and username
				(? (and ":" (? password)))
				"@")
  (:lambda (args)
    (destructuring-bind (username &optional password)
	(butlast args)
      ;; password looks like '(":" "password")
      (list :user username :password (cadr password)))))

(defun hexdigit-char-p (character)
  (member character #. (quote (coerce "0123456789abcdefABCDEF" 'list))))

(defrule ipv4-part (and (digit-char-p character)
			(? (digit-char-p character))
			(? (digit-char-p character))))

(defrule ipv4 (and ipv4-part "." ipv4-part "." ipv4-part "." ipv4-part)
  (:lambda (ipv4)
    (list :ipv4 (text ipv4))))

;;; socket directory is unix only, so we can forbid ":" on the parsing
(defun socket-directory-character-p (char)
  (or (member char #.(quote (coerce "/.-_" 'list)))
      (alphanumericp char)))

(defrule socket-directory (and "unix:" (* (socket-directory-character-p character)))
  (:destructure (unix socket-directory)
		(declare (ignore unix))
    (list :unix (when socket-directory (text socket-directory)))))

(defrule network-name (and namestring (* (and "." namestring)))
  (:lambda (name)
    (list :host (text name))))

(defrule hostname (or ipv4 socket-directory network-name)
  (:identity t))

(defrule dsn-hostname (and (? hostname) (? dsn-port))
  (:destructure (hostname &optional port)
		(append (list :host hostname) port)))

(defrule dsn-dbname (and "/" (? namestring))
  (:destructure (slash dbname)
		(declare (ignore slash))
		(list :dbname dbname)))

(defrule dsn-prefix (or "pgsql://" "postgresql://") (:constant nil))

(defrule db-connection-uri (and dsn-prefix
				(? dsn-user-password)
				(? dsn-hostname)
				dsn-dbname)
  (:lambda (uri)
    (destructuring-bind (&key type
			      user
			      password
			      host
			      port
			      dbname)
	(apply #'append uri)

      (declare (ignore type))
      ;;
      ;; Default to environment variables as described in
      ;;  http://www.postgresql.org/docs/9.3/static/app-psql.html
      ;;  http://dev.mysql.com/doc/refman/5.0/en/environment-variables.html
      ;;
      (list (or dbname (getenv-default "PGDATABASE" user))

            (or user (getenv-default "PGUSER" (getenv-default "USER")))

            (or password (getenv-default "PGPASSWORD"))

            (or (when host
                  (destructuring-bind (type &optional name) host
                    (ecase type
                      (:unix  (if name (cons :unix name) :unix))
                      (:ipv4  name)
                      (:host  name))))
                (getenv-default "PGHOST"
                                #+unix :unix
                                #-unix "localhost"))

            :port (or port
                      (parse-integer (getenv-default "PGPORT" "5432")))))))


;;;
;;; Parsing a connection string
;;;
(defun parse-pgsql-connection-string (connection-string)
  "Parse given CONNECTION-STRING and return a Postmodern suitable connection
   spec.

   Examples:
      IN: postgresql://dim@localhost:54393/pginstall
     OUT: '(\"pginstall\" \"dim\" nil \"localhost\" :port 54393)

   The default port, when omitted, comes from the environment variable PGPORT."
  (parse 'db-connection-uri connection-string))

(defmacro with-pgsql-connection ((connection-string) &body body)
  "Runs BODY within an established PostgreSQL connection."
  `(with-connection (parse-pgsql-connection-string ,connection-string)
     ,@body))

;;;
;;; Validating a connection string
;;;
(defun validate-dburi (connection-string)
  "Signal an error when CONNECTION-STRING either can't be parsed or if we
   can't connect to a PostgreSQL server when using it."
  (with-pgsql-connection (connection-string)
    (query "SELECT 1" :single))
  ;; make sure to return the valid connection-string
  connection-string)

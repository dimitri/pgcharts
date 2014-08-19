(in-package #:pgcharts)

;;;
;;; Tools to install our model.sql objects into the database
;;;

(defparameter *model*
  (read-queries
   (asdf:system-relative-pathname :pgcharts "src/model.sql"))
  "The SQL model as a list of queries.")

(defparameter *model-table-list*
  (sort
   (remove-if #'null
              (mapcar (lambda (sql) (cl-ppcre:register-groups-bind (table-name)
                                        ("create table ([A-Za-z_.]+)" sql)
                                      table-name))
                      *model*))
   #'string<)
  "List of table names expected to be created by *model*, to allow for
   checking if the setup has been made.")

(defun ensure-model-is-installed (&optional (dburi *dburi*))
  "Check that the given database connection DBURI contains the SQL data
  model as defined in *model*."
  (with-pgsql-connection (dburi)
    (let ((table-list (query "select nspname || '.' || relname as relname
                                   from      pg_class c
                                        join pg_namespace n
                                          on c.relnamespace = n.oid
                                  where n.nspname = 'pginstall'
                                        and c.relkind = 'r'
                               order by relname"
                                :column)))
      (unless (equalp *model-table-list* table-list)
        (loop :for sql :in *model* :do (query sql))))))


;;;
;;; Data Access Objects
;;;
;;; Allow to easily manage CRUD operations
;;;
(defclass db ()
    ((dbname      :col-type integer :accessor dbname :initarg :dbname)
     (description :col-type string  :accessor description :initarg :description)
     (dbhost      :col-type string  :accessor dbhost :initarg :dbhost)
     (dbport      :col-type integer :accessor dbport :initarg :dbport)
     (dbuser      :col-type string  :accessor dbuser :initarg :dbuser)
     (dbpass      :col-type string  :accessor dbpass :initarg :dbpass))
  (:documentation
   "a database connection string, where to run queries.")
  (:metaclass dao-class)
  (:keys dbname))

(defmethod db-uri ((db db) &optional stream)
  "Print the pgsql:// URI of DB into STREAM."
  (with-slots (dbname dbhost dbport dbuser) db
    (format stream "pgsql://~a@~a:~d/~a" dbuser dbhost dbport dbname)))

(defmethod print-object ((db db) stream)
  (print-unreadable-object (db stream :type t :identity t)
    (db-uri db stream)))

(defun make-db (name user pass host &key (port 5432))
  "Create a db instance given the same parameters as Postmodern connect."
  (make-instance 'db
                 :dbname name
                 :dbhost host
                 :dbport port
                 :dbuser user
                 :dbpass pass))

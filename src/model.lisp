(in-package #:pgcharts)

;;;
;;; Tools to install our model.sql objects into the database
;;;
(defparameter *model*
  (read-queries
   (asdf:system-relative-pathname :pgcharts "src/model.sql"))
  "The SQL model as a list of queries.")

(defparameter *catversion* "20140828"
  "Version number for the catalog.")

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

(defun model-version (&optional (dburi *dburi*))
  "Check that we find all our table definitions."
  (with-pgsql-connection (dburi)
    (let ((table-list (query "select nspname || '.' || relname as relname
                                   from      pg_class c
                                        join pg_namespace n
                                          on c.relnamespace = n.oid
                                  where n.nspname = 'pgcharts'
                                        and c.relkind = 'r'
                               order by relname"
                             :column)))
      (if (member "pgcharts.catalog" table-list :test #'string=)
          (query "select version from pgcharts.catalog" :single)
          (when (equalp table-list '("pgcharts.db" "pgcharts.query"))
              "20140823")))))

(defun install-model-from-scratch (&optional (dburi *dburi*))
  "Check that the given database connection DBURI contains the SQL data
  model as defined in *model*."
  (with-pgsql-connection (dburi)
    (with-transaction ()
      (loop :for sql :in *model* :do (query sql))

      ;; and an extra SQL statement is needed here
      (destructuring-bind (dbname &rest ignore)
          (parse-pgsql-connection-string dburi)
        (declare (ignore ignore))
        (execute "insert into pgcharts.catalog(version) values($1)" *catversion*)
        (execute (format nil "alter database ~a set search_path to pgcharts"
                         dbname))))))

(defun upgrade-model (current-version &optional (dburi *dburi*))
  "Upgrade the database model by rolling out SQL upgrade scripts."
  (let ((script-name-list (find-update-path current-version *catversion*)))
    (loop :for script-name :in script-name-list
       :for queries := (cdr (assoc script-name *upgrade-scripts* :test #'string=))
       :do (with-pgsql-connection (dburi)
             (format t "Rolling out upgrade script ~a~%" script-name)
             (with-transaction ()
               (loop :for sql :in queries :do (query sql)))))))

(defun ensure-model-is-current (&optional (dburi *dburi*))
  "Check the current model's version and upgrade it if needed."
  (let ((version (model-version dburi)))
    (cond ((null version)
           (format t "Installing pgcharts database model.~%")
           (install-model-from-scratch dburi))

          ((string/= version *catversion*)
           (format t "Upgrading pgcharts database model.~%")
           (upgrade-model version)))))


;;;
;;; Data Access Objects
;;;
;;; Allow to easily manage CRUD operations
;;;
(defclass db ()
    ((dbname      :col-type integer :accessor dbname :initarg :dbname)
     (dburi       :col-type string  :accessor db-uri :initarg :dburi))
  (:documentation
   "a database connection string, where to run queries.")
  (:metaclass dao-class)
  (:keys dbname))

(defmethod print-object ((db db) stream)
  (print-unreadable-object (db stream :type t :identity t)
    (db-uri db stream)))


;;;
;;; Save the queries!
;;;
(defclass query ()
    ((id          :col-type integer :reader qid       :initarg :id)
     (dbname      :accessor dbname  :initarg :dbname
                  :col-type string  :col-name db)
     (qname       :col-type string  :accessor qname   :initarg :qname)
     (description :col-type string  :accessor qdesc   :initarg :description)
     (sql         :col-type integer :accessor qsql    :initarg :sql)
     (cats        :col-type string  :accessor qcats   :initarg :cats)
     (series      :col-type string  :accessor qseries :initarg :series)
     (xtitle      :col-type string  :col-name x_title
                  :accessor xtitle  :initarg :xtitle)
     (ytitle      :col-type string  :col-name y_title
                  :accessor ytitle  :initarg :ytitle)
     (chart-type  :col-type string  :col-name chart_type
                  :accessor chart-type :initarg :chart-type))
  (:documentation
   "a pgchart query")
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((query query) stream)
  (print-unreadable-object (query stream :type t :identity t)
    (let ((qid (when (slot-boundp query 'id) (qid query))))
      (with-slots (qname) query
        (format stream "/q/~@[/~36r~] [~a]" qid qname)))))

(defmethod q/url ((query query))
  "Return the HREF where to display and edit the query."
  (format nil "/q/~36r" (qid query)))

(defmethod q/raw/url ((query query))
  "Return the HREF where to display and edit the query."
  (format nil "/q/raw/~36r" (qid query)))

(defmethod q/del/url ((query query))
  "Return the HREF where to display and edit the query."
  (format nil "/q/del/~36r" (qid query)))

(defmethod c/url ((query query))
  "Return the HREF where to admire the query chart."
  (format nil "/c/~36r" (qid query)))

(in-package #:pgcharts)

(defun register-db (dburi)
  "Register a new database server."
  (when (validate-dburi dburi)
    (destructuring-bind (name user pass host &key (port 5432))
        (parse-pgsql-connection-string dburi)
      (with-pgsql-connection (*dburi*)
        (make-dao 'db
                  :dbname name
                  :dbhost host
                  :dbport port
                  :dbuser user
                  :dbpass pass)))))

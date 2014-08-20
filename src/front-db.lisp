(in-package #:pgcharts)

;;;
;;; Database connection strings management
;;;
(defun front-add-database ()
  "Add a new database connection string."
  (let* ((dburi (hunchentoot:post-parameter "dburi")))
    (when (validate-dburi dburi)
      (destructuring-bind (name user pass host &key (port 5432))
          (parse-pgsql-connection-string dburi)
        (with-pgsql-connection (*dburi*)
          (make-dao 'db
                    :dbname name
                    :dbhost host
                    :dbport port
                    :dbuser user
                    :dbpass pass)))
      (front-manage-databases))))

(defun front-edit-db-desc (db)
  "Output db's description within its own form"
  (with-html-output-to-string (s)
    (htm
     (:form :role "desc" :method "post" :action "/db/desc"
            (:input :type "hidden"
                    :id "dbname"
                    :value (dbname db))
            (:div :class "input-group"
                  (:input :type "text"
                          :width "10"
                          :class "form-control"
                          :id "desc"
                          :value (let ((desc (description db)))
                                   (if (or (null desc) (eq desc :null)) ""
                                       desc)))
                  (:span :class "input-group-btn"
                         (:button :class "btn btn-primary"
                                  :type "submit" "set")))))))

(defun front-set-database-description ()
  "Set the description and get back to the database listing."
  (let ((name (hunchentoot:post-parameter "dbname"))
        (desc (hunchentoot:post-parameter "desc")))
    (with-pgsql-connection (*dburi*)
      (query "update db set desc = ? where dbname = ?" desc name))
    (front-manage-databases)))

(defun front-manage-databases ()
  "Allow user to enter a new query."
  (serve-page
   (with-html-output-to-string (s)
     (htm
      (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
            (:form :role "dburi"
                   :method "get"
                   :action "/db/add"
                   (:div :class "form-group"
                         (:label :for "dburi"
                                 :class "col-sm-3 control-label"
                                 "Add a new database")
                         (:div :class "input-group"
                               (:input :type "text" :class "form-control"
                                       :id "dburi"
                                       :name "dburi"
                                       :placeholder "pgsql://localhost/dbname")
                               (:span :class "input-group-btn"
                                      (:button :class "btn btn-primary"
                                               :type "submit"
                                               "Add database"))))))
      (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
            (:h1 :class "page-header" "Database Connection Strings")
            (:p "You will be able to run queries against those databases.
              Queries will be registered in the pgchart database that you
              had to setup, the databases listed here are only going to be
              used to run your queries.")
            (:div :class "table-responsive"
                  (:table :class "table table-stripped"
                          (:thead
                           (:tr (:th "Short Name")
                                (:th "Connection string")
                                (:th "Description")))
                          (:tbody
                           (loop :for db
                              :in (with-pgsql-connection (*dburi*)
                                    (select-dao 'db t 'dbname))
                              :do (htm
                                   (:tr
                                    (:td (str (dbname db)))
                                    (:td (str (db-uri db)))
                                    (:td :style "width: 15em;"
                                         (str (front-edit-db-desc db))))))))))))))

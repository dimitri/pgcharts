(in-package #:pgcharts)

;;;
;;; Frontend for query editing and result display (text or graph)
;;;

(defun front-pick-db ()
  "Pick a database"
  (let ((db-list (with-pgsql-connection (*dburi*)
                   (select-dao 'db t 'dbname))))
    (serve-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "First, you need to pick a database")
              (:p "The query you're going to run and edit next are to be
              made against the database you pick now.")
              (:div :class "row"
                    (loop :for db :in db-list
                       :do (htm
                            (:div :class "col-sm-6 col-md-4"
                                  (:div :class "thumbnail"
                                        (:a :href (format nil "/q/~a" (dbname db))
                                            (:img :src "/images/database_2_128.png"))
                                        (:div :class "caption"
                                              (:h3
                                               (:a :href (format nil "/q/~a" (dbname db))
                                                   (str (dbname db)))
                                               )
                                              (:p  (str (description db)))))))))))))))

(defun front-new-query (db)
  "Allow user to enter a new query."
  (let ((target-db (with-pgsql-connection (*dburi*) (get-dao 'db db))))
    (serve-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "SQL Query")
              (:form :role "query"
                     :id "run-query"
                     :method "post"
                     :action "/run"
                     :class "form-horizontal"
                     (:input :type "hidden"
                             :id "dburi"
                             :name "dburi"
                             :value (db-uri target-db))
                     (:div :class "form-group"
                           (:label :for "qname" :class "col-sm-3 control-label"
                                   "Query name")
                           (:div :class "col-sm-9"
                                 (:input :type "text" :name "qname" :id "qname"
                                         :placeholder "Enter query name"
                                         :class "form-control")))
                     (:div :class "form-group"
                           (:label :for "qdesc" :class "col-sm-3 control-label"
                                   "Query description")
                           (:div :class "col-sm-9"
                                 (:input :type "text" :name "qdesc" :id "qdesc"
                                         :placeholder "Enter query description"
                                         :class "form-control")))

                     (:div :class "form-group"
                           (:label :for "query" :class "col-sm-3 control-label"
                                   "Query SQL")
                           (:div :class "col-sm-9"
                                 (:textarea :id "query" :name "query" :rows "25"
                                            "with drb_stats as (
    select min(drb) as min,
           max(drb) as max
      from team_stats
)
   select width_bucket(drb, min, max, 9) as bucket,
          int4range(min(drb), max(drb), '[]') as range,
          count(*) as freq
     from team_stats, drb_stats
 group by bucket
 order by bucket")))
                     (:div :class "form-group"
                           (:div :class "col-sm-offset-3 col-sm-9"
                                 (:button :class "btn btn-primary"
                                          :type "submit"
                                          "Run Query")))))
        (:script "
            var myCodeMirror = CodeMirror.fromTextArea(query, {
              lineWrapping: true,
              lineNumbers: true,
              styleActiveLine: true,
              matchBrackets: true,
              mode:  \"text/x-plsql\",
              theme: \"elegant\"
            });"))
       (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
             (:h1 :class "page-header" "Query Results")
             (:ul :class "nav nav-tabs"
                  (:li :class "active"
                       (:a :id "raw" :href "#raw"
                           (:span :class "glyphicon glyphicon-th"
                                  " Raw Results")))
                  (:li (:a :id "col" :href "#col"
                           (:span :class "glyphicon glyphicon-stats")
                           " Column Chart"))
                  (:li (:a :id "bar" :href "#bar"
                           (:span :class "glyphicon glyphicon-align-left")
                           " Bar Chart"))
                  (:li (:a :id "pie" :href "#pie"
                           (:span :class "glyphicon glyphicon-dashboard")
                           " Pie Chart"))
                  (:li (:a :id "donut" :href "#donut"
                           (:span :class "glyphicon glyphicon-record")
                           " Donut Chart")))
             (:div :id "qresult")))))))

;;; REVIEW THAT PARAMETER
;;;
;;; It should be possible to compute that directly from the chart
;;; "templates" that we have.
(defvar *chart-kinds* '(("/chart/area"  . "Area Chart")
                        ("/chart/pie"   . "Pie Chart")
                        ("/chart/bar"   . "Bar Chart")
                        ("/chart/donut" . "Donut Chart")
                        ("/chart/lines" . "Line Series Chart")))

(defun list-chart-options (dburi query)
  "Return an HTML string for displaying a choice of chart."
  (with-html-output-to-string (s)
    (htm
     (:div :class "row"
           (loop :for (url . label) :in *chart-kinds*
              :do (htm
                   (:div :class "col-sm-2"
                         (:form :role "chart"
                                :method "post"
                                :action (str url)
                                (:div :class "form-group"
                                      (:div :class "input-group"
                                            (:button :class "btn btn-primary"
                                                     :type "submit"
                                                     (str label))))))))))))

(defun front-run-query ()
  "Get a connection string and a query and run the bloody query already."
  (let* ((dburi  (hunchentoot:post-parameter "dburi"))
         (query  (hunchentoot:post-parameter "query"))
         ;; HACK ALERT
         ;; we should probably register our own row-reader instead...
         (coldef (with-pgsql-connection (dburi)
                   (query query :alist)))
         (result (with-pgsql-connection (dburi)
                   (query query))))
    (serve-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "Query Result")
              (str (list-chart-options dburi query))
              (:table :class "table table-stripped"
                      (:thead
                       (:tr
                        (loop :for (name . value) :in coldef
                           :do (htm (:th (str name))))))
                      (:tbody
                       (loop :for row :in result
                          :do (htm
                               (:tr
                                (loop :for col :in row
                                   :do (htm
                                        (:td (str col)))))))))))))))

(defun front-fetch-csv-data ()
  "Given an SQL query and a connection string given as POST parameters,
   return the query result-set as CSV data."
  (setf (hunchentoot:content-type*) "text/plain")
  (let* ((dburi  (hunchentoot:post-parameter "dburi"))
         (query  (hunchentoot:post-parameter "query"))
         (data   (with-pgsql-connection (dburi)
                   (query query))))
    (with-output-to-string (s)
     (loop :for row :in data
        :do (format s "~&~{\"~a\"~^,~}" row)))))

(defun front-fetch-json-data ()
  "Given an SQL query and a connection string given as POST parameters,
   return the query result-set as CSV data."
  (setf (hunchentoot:content-type*) "text/plain")
  (let* ((dburi  (hunchentoot:post-parameter "dburi"))
         (query  (hunchentoot:post-parameter "query"))
         (data   (with-pgsql-connection (dburi)
                   (query query :alists))))
    (format nil "[~{~a~^, ~}]"
            (loop :for row :in data
               :collect (with-output-to-string (s)
                          (yason:encode-alist row s))))))

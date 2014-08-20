(in-package #:pgcharts)

;;;
;;; Frontend for query editing and result display (text or graph)
;;;
(defun front-new-query ()
  "Allow user to enter a new query."
  (serve-page
   (with-html-output-to-string (s)
     (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "SQL Query")
              (:form :role "query"
                     :method "post"
                     :action "/run"
                     (:div :class "form-group"
                           (:div :class "input-group"
                                 (:input :type "text" :name "dburi" :id "dburi"
                                         :class "form-control"
                                         :value "pgsql:///nba"
                                         :placeholder "pgsql:///nba"
                                         (:span :class "input-group-btn"
                                                (:button :class "btn btn-primary"
                                                         :type "submit"
                                                         "Run Query"))))
                           (:div :class "form-group"
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
 order by bucket"))))
              (:script "
            var myCodeMirror = CodeMirror.fromTextArea(query, {
              lineWrapping: true,
              lineNumbers: true,
              styleActiveLine: true,
              matchBrackets: true,
              mode:  \"text/x-plsql\",
              theme: \"elegant\"
            });"
                       ))
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
              (:div :id "qresult"))))))

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

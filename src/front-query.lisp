(in-package #:pgcharts)

;;;
;;; Frontend for query editing and result display (text or graph)
;;;
(defvar *chart-types* '("Column" "Bar" "Pie" "Donut")
  "Known chart types.")

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

(defun front-edit-query (db &optional qid)
  "Return the HTML to display a query form."
  (let ((q (if qid (with-pgsql-connection (*dburi*)
                     (get-dao 'query (parse-integer qid :radix 36)))
               (make-instance 'query
                              :dbname db
                              :qname "drb range"
                              :description "Defensive rebounds, by range"
                              :sql "with drb_stats as (
    select min(drb) as min,
           max(drb) as max
      from team_stats
)
   select width_bucket(drb, min, max, 9) as bucket,
          int4range(min(drb), max(drb), '[]') as range,
          count(*) as freq
     from team_stats, drb_stats
 group by bucket
 order by bucket"
                              :cats "range"
                              :series "freq"
                              :xtitle "Ranges"
                              :ytitle "Defensive rebounds"))))
    (with-html-output-to-string (s)
      (htm
       (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
             (:h1 :class "page-header" "SQL Query")
             (:form :role "query"
                    :id "run-query"
                    :method "post"
                    :action "/q/save"
                    :class "form-horizontal"
                    (:input :type "hidden" :id "qid" :name "qid" :value qid)
                    (:input :type "hidden" :id "dbname" :name "dbname" :value db)
                    (:input :type "hidden" :id "dburi" :name "dburi"
                            :value (db-uri (with-pgsql-connection (*dburi*)
                                             (get-dao 'db db))))
                    (:div :class "form-group"
                          (:label :for "qname" :class "col-sm-3 control-label"
                                  "Query name")
                          (:div :class "col-sm-9"
                                (:input :type "text" :name "qname" :id "qname"
                                        :placeholder "Enter query name"
                                        :class "form-control"
                                        :value (qname q))))
                    (:div :class "form-group"
                          (:label :for "qdesc" :class "col-sm-3 control-label"
                                  "Query description")
                          (:div :class "col-sm-9"
                                (:input :type "text" :name "qdesc" :id "qdesc"
                                        :placeholder "Enter query description"
                                        :class "form-control"
                                        :value (qdesc q))))
                    (:div :class "form-group"
                          (:label :for "cats" :class "col-sm-3 control-label"
                                  "Categories (x axis)")
                          (:div :class "col-sm-3"
                                (:input :type "text" :name "cats" :id "cats"
                                        :placeholder "Enter categories column name"
                                        :class "form-control"
                                        :value (qcats q)))
                          (:label :for "xtitle" :class "col-sm-2 control-label"
                                  "X Legend")
                          (:div :class "col-sm-4"
                                (:input :type "text" :name "xtitle" :id "xtitle"
                                        :placeholder "Enter categories legend"
                                        :class "form-control"
                                        :value (xtitle q))))
                    (:div :class "form-group"
                          (:label :for "series" :class "col-sm-3 control-label"
                                  "Data series")
                          (:div :class "col-sm-3"
                                (:input :type "text" :name "series" :id "series"
                                        :placeholder "Enter data series column name"
                                        :class "form-control"
                                        :value (qseries q)))
                          (:label :for "ytitle" :class "col-sm-2 control-label"
                                  "Y Legend")
                          (:div :class "col-sm-4"
                                (:input :type "text" :name "ytitle" :id "ytitle"
                                        :placeholder "Enter series legend"
                                        :class "form-control"
                                        :value (ytitle q))))

                    (:div :class "form-group"
                          (:label :for "query" :class "col-sm-3 control-label"
                                  "Query SQL")
                          (:div :class "col-sm-9"
                                (:textarea :id "query" :name "query" :rows "25"
                                           (str (qsql q)))))

                    (:div :class "form-group"
                          (:div :class "col-sm-offset-3 col-sm-2"
                          (:button :id "btn-run-query"
                                   :class "btn btn-success"
                                   :type "button" "Run Query"))

                          (:label :for "chart-type" :class "col-sm-3 control-label"
                                  "Default Chart Type")
                          (:div :class "col-sm-2"
                                (:select :id "chart-type"
                                         :name "chart-type"
                                         :class "form-control"
                                         (loop :for type :in *chart-types*
                                            :for on := (string= type (chart-type q))
                                            :do (htm (:option :selected on
                                                              (str type))))))

                          (:div :class "col-sm-2"
                                (:button :id "btn-save-query"
                                         :class "btn btn-primary"
                                         :type "submit" "Save Query"))))

             (:script "
            var myCodeMirror = CodeMirror.fromTextArea(query, {
              lineWrapping: true,
              lineNumbers: true,
              styleActiveLine: true,
              matchBrackets: true,
              mode:  \"text/x-plsql\",
              theme: \"elegant\"
            });"))))))

(defun front-query-result ()
  "Display query result, with tabs for different charts types."
  (with-html-output-to-string (s)
    (htm
     (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
           (:h1 :class "page-header" "Query Results")
           (:ul :id "charts" :class "nav nav-tabs"
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
           (:div :id "qresult")))))

(defun front-new-query (db)
  "Allow user to enter a new query."
  (serve-page
   (with-html-output-to-string (s)
     (htm
      (str (front-edit-query db))
      (str (front-query-result)))))))

(defun front-display-query (db qid)
  "Allow user to run and edit a known query."
  (serve-page
   (with-html-output-to-string (s)
     (htm
      (str (front-edit-query db qid))
      (str (front-query-result))))))

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

(defun front-save-query ()
  "Save SQL query as given by form."
  (let ((dbname     (hunchentoot:post-parameter "dbname"))
        (qid        (hunchentoot:post-parameter "qid"))
        (qname      (hunchentoot:post-parameter "qname"))
        (qdesc      (hunchentoot:post-parameter "qdesc"))
        (query      (hunchentoot:post-parameter "query"))
        (qcats      (hunchentoot:post-parameter "cats"))
        (qseries    (hunchentoot:post-parameter "series"))
        (xtitle     (hunchentoot:post-parameter "xtitle"))
        (ytitle     (hunchentoot:post-parameter "ytitle"))
        (chart-type (hunchentoot:post-parameter "chart-type")))
    (with-pgsql-connection (*dburi*)
      ;; basically insert or update, depending on whether we already have a
      ;; query id or not.
      (let ((query
             (if qid
                 (make-dao 'query
                           :dbname dbname
                           :qname qname   :description qdesc   :sql query
                           :cats qcats    :series qseries
                           :xtitle xtitle :ytitle ytitle :chart-type chart-type)
                 (update-dao (make-instance 'query
                                            :dbname dbname :qname qname
                                            :description qdesc :sql query
                                            :cats qcats    :series qseries
                                            :xtitle xtitle :ytitle ytitle
                                            :chart-type chart-type)))))
        ;; and now redirect to editing that same query
        (hunchentoot:redirect (format nil "/q/~a/~36r" dbname (qid query))
                              :code hunchentoot:+http-moved-temporarily+)))))

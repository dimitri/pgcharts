(in-package #:pgcharts)

;;;
;;; Raw query allows to easily play around with a SQL query without having
;;; to actually prepare and setup all the Charting jazz.
;;;

(defun front-raw-query (&optional qid)
  "Return the HTML to display a query form."
  (destructuring-bind (&key q ((:d dbname-list)))
      (let ((dbname     (hunchentoot:post-parameter "dbname"))
            (qname      (hunchentoot:post-parameter "qname"))
            (qdesc      (hunchentoot:post-parameter "qdesc"))
            (query      (hunchentoot:post-parameter "query")))
        (with-pgsql-connection (*dburi*)
          (list :q (if qid
                       (get-dao 'query (parse-integer qid :radix 36))
                       (make-instance 'query
                                      :dbname dbname
                                      :qname qname
                                      :description qdesc
                                      :sql query
                                      :cats ""
                                      :series ""
                                      :xtitle ""
                                      :ytitle ""
                                      :chart-type ""))
                :d (query "select dbname from db order by 1" :column))))
    (serve-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "SQL Query")
              (:form :role "query"
                     :id "run-query"
                     :method "post"
                     :action "/q/raw"
                     :class "form-horizontal"
                     (:input :type "hidden"
                             :id "chart-type" :name "chart-type" :value "raw")
                     (:div :class "form-group"
                           (:label :for "dbname" :class "col-sm-3 control-label"
                                   "Database name")
                           (:div :class "col-sm-9"
                                 (:select :id "dbname"
                                          :name "dbname"
                                          :class "form-control"
                                          (loop :for dbname :in dbname-list
                                             :do (htm (:option (str dbname)))))))
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
                           (:label :for "query" :class "col-sm-3 control-label"
                                   "Query SQL")
                           (:div :class "col-sm-9"
                                 (:textarea :id "query" :name "query" :rows "25"
                                            (str (qsql q)))))

                     (:div :class "form-group"
                           (:div :class "col-sm-offset-3 col-sm-2"
                                 (:button :id "btn-run-raw-query"
                                          :class "btn btn-success"
                                          :type "submit" "Run Query"))

                           (:div :class "col-sm-offset-5 col-sm-2"
                                 (:button :id "btn-save-raw-query"
                                          :class "btn btn-primary"
                                          :type "button" "Save Query"))))

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
              (:h1 :class "page-header" "Result Set")
              (:div :id "qresult" :class "table-responsive"
                    (when (and (qsql q) (not (string= "" (qsql q))))
                      (str (front-raw-result (dbname q) (qsql q)))))))))))

(defun front-raw-result (dbname query)
  "Return the HTML string for the result of given query."
  (let* ((qdburi  (with-pgsql-connection (*dburi*)
                    (db-uri (get-dao 'db dbname))))
         (data    (with-pgsql-connection (qdburi)
                    (query query :alists))))
    (with-html-output-to-string (s)
      (:table :class "table table-stripped table-hover table-condensed"
              (:thead
               (:tr (loop :for (col . val) :in (first data)
                       :do (htm (:th (str col))))))
              (:tbody
               (loop :for row :in data
                  :do (htm
                       (:tr
                        (loop :for (col . val) :in row
                           :do (htm (:td (str val))))))))))))

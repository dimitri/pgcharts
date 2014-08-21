(in-package #:pgcharts)

;;;
;;; Main dashboard
;;;
(defun front-list-queries ()
  "Serve the list of SQL queries."
  (let ((query-list (with-pgsql-connection (*dburi*)
                      (select-dao 'query t 'db 'qname))))
    (serve-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h1 :class "page-header" "Queries")
              (:div :class "table-responsive"
                    (:table :class "table table-stripped"
                            (:thead
                             (:tr (:th "Query")
                                  (:th "Database")
                                  (:th "Description")
                                  (:th "X Title")
                                  (:th "Y Title")
                                  (:th "Chart")))
                            (:tbody
                             (loop :for query :in query-list
                                :do (htm
                                     (:tr
                                      (:td (:a :href (q/url query)
                                               (:span :class "glyphicon glyphicon-edit
"
                                                      " "
                                                      (str (format nil "~36r" (qid query))))))
                                      (:td (str (dbname query)))
                                      (:td (:a :href (q/url query)
                                               (str (qdesc query))))
                                      (:td (str (xtitle query)))
                                      (:td (str (ytitle query)))
                                      (:td (:a :href (c/url query)
                                               (:span :class "glyphicon glyphicon-stats"
                                                      " "
                                                      (str (chart-type query)))))))))))))))))

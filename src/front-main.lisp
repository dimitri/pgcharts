(in-package #:pgcharts)

;;;
;;; Main dashboard
;;;
(defun list-queries (query-list &key (title "Queries"))
  "Return a whole web page for the QUERY-LIST."
  (serve-page
   (with-html-output-to-string (s)
     (htm
      (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
            (:h1 :class "page-header" (str title))
            (:h4 :style "color: red;" "Warning "
                 (:small "The delete action requires no validation."))
            (:div :class "table-responsive"
                  (:table :class "table table-stripped"
                          (:thead
                           (:tr (:th "")
                                (:th "Query")
                                (:th "Database")
                                (:th "Description")
                                (:th "Chart")))
                          (:tbody
                           (loop :for query :in query-list
                              :do (htm
                                   (:tr
                                    (:td
                                     (:a :href (q/url query)
                                             (:span :class "glyphicon glyphicon-edit"
                                                    :style "color: black;"))
                                         " "
                                         (:a :href (q/del/url query)
                                             (:span :class "glyphicon glyphicon-remove"
                                                    :style "color: red;")))
                                    (:td (:a :href (q/url query)
                                             (str (format nil "~36r" (qid query)))))
                                    (:td (str (dbname query)))
                                    (:td (:a :href (q/url query)
                                             (str (qdesc query))))
                                    (:td (:a :href (c/url query)
                                             (:span :class "glyphicon glyphicon-stats"
                                                    " "
                                                    (str (chart-type query))))))))))))))))

(defun front-list-queries ()
  "Serve the list of SQL queries."
  (list-queries (with-pgsql-connection (*dburi*)
                  (select-dao 'query t 'db 'qname))))

(defun front-search-queries ()
  "Return a list of queries loosely matching given TERMS."
  (let ((terms (hunchentoot:get-parameter "terms")))
    (list-queries (with-pgsql-connection (*dburi*)
                    (select-dao 'query (:or (:~* 'description terms)
                                            (:~* 'sql terms))))
                  :title (format nil "Queries matching regexp: <tt>~a</tt>" terms))))

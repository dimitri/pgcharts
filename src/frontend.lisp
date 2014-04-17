(in-package #:pgcharts)

;;;
;;; General tools to render frontend code
;;;
(defvar *menu* '(("/#q" . "Query")
                 ("/#r" . "Result")
                 ("/#c" . "Chart"))
  "An alist of HREF and TITLE for the main menu.")

(defun compute-menu (current-url-path)
  "List all files found in the *DOCROOT* directory and turns the listing
   into a proper bootstrap menu."
  (when *dburi*
    ;; all the entries in the menu only work properly with a database
    ;; connection (that has been setup), so refrain from displaying them
    ;; when the basic setup has not been done yet.
    (with-html-output-to-string (s)
      (htm
       (:div :class "col-sm-3 col-md-2 sidebar"
             (:ul :class "nav nav-sidebar"
                  (loop :for (href . title) :in *menu*
                     :for active := (string= href current-url-path)
                     :do (if active
                             (htm
                              (:li :class "active"
                                   (:a :href (str href) (str title))))
                             (htm
                              (:li
                               (:a :href (str href) (str title))))))))))))

(defun serve-pgcharts-js-file ()
  "Serve whatever /dist/.* has been asked."
  (let ((filename (format nil "~a/~{~a~^/~}"
                          *js-root*
                          ;; skip leading /js/ from the script name
                          (cddr (split-sequence #\/ (hunchentoot:script-name*))))))
    (hunchentoot:handle-static-file filename)))

(defun serve-bootstrap-file ()
  "Serve whatever /dist/.* has been asked."
  (let ((filename (format nil "~a/~{~a~^/~}"
                          *bootstrap-root*
                          ;; skip leading /dist/ from the script name
                          (cddr (split-sequence #\/ (hunchentoot:script-name*))))))
    (hunchentoot:handle-static-file filename)))

(defun serve-demo-data-file ()
  "Serve whatever /test/.* has been asked."
  (let ((filename (format nil "~a/~{~a~^/~}"
                          *charts-demo-root*
                          ;; skip leading /test/ from the script name
                          (cddr (split-sequence #\/ (hunchentoot:script-name*))))))
    (hunchentoot:handle-static-file filename)))

(defun front-demo (chart)
  "Serve the static demo file."
  (hunchentoot:handle-static-file
   (make-pathname :directory (directory-namestring *charts-demo-root*)
                  :name chart
                  :type "html")))

(defun serve-d3js ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *d3js*))

(defun serve-codemirror-js ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *codemirror-js*))

(defun serve-codemirror-css ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *codemirror-css*))

(defun serve-codemirror-theme-elegant ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *codemirror-theme-elegant*))

(defun serve-page (content)
  "Return the whole CONTENT for the HTML output, including header/footer."
  (concatenate 'string
               (read-file-into-string *header-path*)
               (compute-menu "/#q")
               content
               (read-file-into-string *footer-path*)))

;;;
;;; Frontend Code
;;;
(defun front-home ()
  "Display the home page"
  (hunchentoot:handle-static-file
   (merge-pathnames "textarea.html" *document-root*)))

(defun front-test (chart)
  "Display the static test chart"
  (format t "PLOP: ~s~%" chart)
  (let ((filename (format nil "~a/~a.html" *chart-test-root* chart)))
    (format t "PLOP: ~s~%" filename)
    (hunchentoot:handle-static-file filename)))

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
                   (:li :class "active" (:a :id "raw" :href "#raw" "Raw Results"))
                   (:li (:a :id "bar" :href "#bar" "Bar Chart"))
                   (:li (:a :id "pie" :href "#pie" "Pie Chart"))
                   (:li (:a :id "donut" :href "#donut" "Donut Chart")))
              (:div :id "qresult"))
        ))))

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

(defun front-display-graph-given-query-text (kind)
  "Draw what's been asked for: pie chart, line chart, etc."
  (front-fetch-csv-data))

(in-package #:pgcharts)

;;;
;;; General tools to render frontend code
;;;
(defvar *menu* '(("/"      "dashboard" " Dashboard")
                 ("/db"    "log-in"    " Databases")
                 ("/users" "user"      " Users")
                 ("/q"     "filter"    " Queries"))
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
                  (loop :for (href icon title) :in *menu*
                     :for active := (string= href current-url-path)
                     :do (if active
                             (htm
                              (:li :class "active"
                                   (:a :href (str href)
                                       (:span :class (str (format nil "glyphicon glyphicon-~a" icon)))
                                       (str title))))
                             (htm
                              (:li
                               (:a :href (str href)
                                   (:span :class (str (format nil "glyphicon glyphicon-~a" icon)))
                                   (str title))))))))))))

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

(defun serve-highcharts-file ()
  "Serve whatever /dist/.* has been asked."
  (let ((filename (format nil "~a/~{~a~^/~}"
                          *highcharts-root*
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


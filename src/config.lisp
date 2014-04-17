(in-package #:pgcharts)

(defvar *config-filename* "~/.pgcharts.ini"
  "Where to store pginstall configuration.")

(defparameter *dburi* "postgresql:///pgcharts"
  "PostgreSQL database connection.")

(defparameter *listen-port* 9042
  "Port bound by the repository server, exposing the HTTP protocol.")

(defparameter *document-root*
  (asdf:system-relative-pathname :pgcharts "web/"))

(defparameter *js-root*
  (asdf:system-relative-pathname :pgcharts "web/js"))

(defparameter *bootstrap-root*
  (asdf:system-relative-pathname :pgcharts "web/bootstrap-3.1.1-dist"))

(defparameter *d3js*
  (asdf:system-relative-pathname :pgcharts "web/d3js/d3.min.js"))

(defparameter *codemirror-js*
  (asdf:system-relative-pathname :pgcharts "web/codemirror-compressed.js"))

(defparameter *codemirror-css*
  (asdf:system-relative-pathname :pgcharts "web/codemirror-4.0/lib/codemirror.css"))

(defparameter *codemirror-theme-elegant*
  (asdf:system-relative-pathname :pgcharts "web/codemirror-4.0/theme/elegant.css"))

(defparameter *charts-root*
  (asdf:system-relative-pathname :pgcharts "web/charts/"))

(defparameter *charts-demo-root*
  (asdf:system-relative-pathname :pgcharts "web/demo/"))

(defparameter *header-path*
  (asdf:system-relative-pathname :pgcharts "web/header.html"))

(defparameter *footer-path*
  (asdf:system-relative-pathname :pgcharts "web/footer.html"))

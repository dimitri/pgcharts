(in-package #:pgcharts)

(defvar *config-filename* "~/.pginstall.ini"
  "Where to store pginstall configuration.")

(defparameter *dburi* "postgresql:///pginstall"
  "PostgreSQL database connection.")

(defparameter *listen-port* 9042
  "Port bound by the repository server, exposing the HTTP protocol.")

(defparameter *document-root*
  (asdf:system-relative-pathname :pgcharts "web/"))

(defparameter *bootstrap-root*
  (asdf:system-relative-pathname :pgcharts "web/bootstrap-3.1.1-dist"))

(defparameter *d3js*
  (asdf:system-relative-pathname :pgcharts "web/d3js/d3.min.js"))

(defparameter *chart-test-root*
  (asdf:system-relative-pathname :pgcharts "web/charts/"))

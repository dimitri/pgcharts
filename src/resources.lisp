(in-package #:pgcharts)

;;;
;;; General tools to render web static resources
;;;
(defun serve-static-file (root script-name &key (depth 1))
  "Serve a static file given a ROOT location and a SCRIPT-NAME.

   Skip DEPTH leading directory entries in SCRIPT-NAME."
  (let ((filename (format nil "~a/~{~a~^/~}"
                          root
                          (nthcdr (+ 1 depth) (split-sequence #\/ script-name)))))
    (hunchentoot:handle-static-file filename)))

(defun serve-header ()
  "Serve the header file."
  (read-file-into-string *header-path*))

(defun serve-footer ()
  "Serve the footer file."
  (read-file-into-string *footer-path*))

(defun serve-pgcharts-js-file ()
  "Serve whatever /dist/.* has been asked."
  (serve-static-file *js-root* (hunchentoot:script-name*)))

(defun serve-bootstrap-file ()
  "Serve whatever /dist/.* has been asked."
  (serve-static-file *bootstrap-root* (hunchentoot:script-name*)))

(defun serve-highcharts-file ()
  "Serve whatever /dist/.* has been asked."
  (serve-static-file *highcharts-root* (hunchentoot:script-name*)))

(defun serve-image-file ()
  "Serve whatever /dist/.* has been asked."
  (serve-static-file *images-root* (hunchentoot:script-name*)))

(defun serve-codemirror-js ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *codemirror-js*))

(defun serve-codemirror-css ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *codemirror-css*))

(defun serve-codemirror-theme-elegant ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *codemirror-theme-elegant*))


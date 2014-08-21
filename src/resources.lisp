(in-package #:pgcharts)

;;;
;;; General tools to render web static resources
;;;
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

(defun serve-image-file ()
  "Serve whatever /dist/.* has been asked."
  (let ((filename (format nil "~a/~{~a~^/~}"
                          *images-root*
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

(defun serve-codemirror-js ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *codemirror-js*))

(defun serve-codemirror-css ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *codemirror-css*))

(defun serve-codemirror-theme-elegant ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *codemirror-theme-elegant*))


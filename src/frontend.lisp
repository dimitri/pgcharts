(in-package #:pgcharts)


(defun serve-bootstrap-file ()
  "Serve whatever /dist/.* has been asked."
  (let ((filename (format nil "~a/~{~a~^/~}"
                          *bootstrap-root*
                          ;; skip leading /dist/ from the script name
                          (cddr (split-sequence #\/ (hunchentoot:script-name*))))))
    (hunchentoot:handle-static-file filename)))

(defun serve-test-file ()
  "Serve whatever /test/.* has been asked."
  (let ((filename (format nil "~a/~{~a~^/~}"
                          *chart-test-root*
                          ;; skip leading /test/ from the script name
                          (cddr (split-sequence #\/ (hunchentoot:script-name*))))))
    (hunchentoot:handle-static-file filename)))

(defun serve-d3js ()
  "Serve the d3js file, minified."
  (hunchentoot:handle-static-file *d3js*))

(defun front-home ()
  "Display the home page"
  "Hello, world!")

(defun front-test (chart)
  "Display the static test chart"
  (format t "PLOP: ~s~%" chart)
  (let ((filename (format nil "~a/~a.html" *chart-test-root* chart)))
    (format t "PLOP: ~s~%" filename)
    (hunchentoot:handle-static-file filename)))

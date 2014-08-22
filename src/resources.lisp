(in-package #:pgcharts)

;;;
;;; When building a self-contained binary, we want to load all static
;;; resources in-memory.
;;;
(defun load-static-file (fs pathname url-path)
  "Load given PATHNAME contents at URL-PATH in FS."
  (setf (gethash url-path fs)
        (read-file-into-byte-vector pathname)))

(defun pathname-to-url (pathname url-path)
  "Transform given PATHNAME into an URL at which to serve it within URL-PATH."
  (multiple-value-bind (flag path-list last-component file-namestring-p)
      (uiop:split-unix-namestring-directory-components
       (uiop:native-namestring pathname))
    (declare (ignore flag file-namestring-p))
    (format nil "~a~{/~a~}/~a" url-path path-list last-component)))

(defun load-static-directory (fs root url-path)
  "Walk PATH and load all files found in there as binary sequence, FS being
   an hash table referencing the full path against the bytes."
  (flet ((collectp  (dir) (declare (ignore dir)) t)
         (recursep  (dir) (declare (ignore dir)) t)
         (collector (dir)
           (loop :for pathname :in (uiop:directory-files dir)
              :unless (or (uiop:directory-pathname-p pathname)
                          (string= "zip" (pathname-type pathname)))
              :do (let ((url (pathname-to-url
                              (uiop:enough-pathname pathname root) url-path)))
                    (load-static-file fs pathname url)))))
    (uiop:collect-sub*directories root #'collectp #'recursep #'collector)))

(defparameter *fs*
  (let ((fs (make-hash-table :test #'equal)))
    (loop :for (root . url-path) :in `((,*js-root*         . "/js")
                                       (,*images-root*     . "/images")
                                       (,*bootstrap-root*  . "/dist")
                                       (,*highcharts-root* . "/highcharts"))
       :for root-dir := (uiop:ensure-directory-pathname root)
       :do (load-static-directory fs root-dir url-path))

    (loop :for (filename . url-path)
       :in `((,*codemirror-js*  . "/codemirror.js")
             (,*codemirror-css* . "/codemirror.css")
             (,*codemirror-theme-elegant* . "/cm-s-elegant.css"))
       :do (load-static-file fs filename url-path))
    fs)
  "File system as an hash-table in memory.")

(defparameter *header*              (read-file-into-string *header-path*))
(defparameter *footer*              (read-file-into-string *footer-path*))

;;;
;;; General tools to render web static resources
;;;


;;;
;;; Single files
;;;
(defun serve-header ()
  "Serve the header file."
  (if *serve-from-cache* *header*
      (read-file-into-string *header-path*)))

(defun serve-footer ()
  "Serve the footer file."
  (if *serve-from-cache* *footer*
      (read-file-into-string *footer-path*)))

(defun serve-codemirror-js ()
  "Serve the d3js file, minified."
  (if *serve-from-cache*
      (handle-loaded-file (hunchentoot:script-name*))
      (hunchentoot:handle-static-file *codemirror-js*)))

(defun serve-codemirror-css ()
  "Serve the d3js file, minified."
  (if *serve-from-cache*
      (handle-loaded-file (hunchentoot:script-name*))
      (hunchentoot:handle-static-file *codemirror-css*)))

(defun serve-codemirror-theme-elegant ()
  "Serve the d3js file, minified."
  (if *serve-from-cache*
      (handle-loaded-file (hunchentoot:script-name*))
      (hunchentoot:handle-static-file *codemirror-theme-elegant*)))


;;;
;;; Sub-directories
;;;
(defun serve-static-file (root script-name &key (depth 1))
  "Serve a static file given a ROOT location and a SCRIPT-NAME.

   Skip DEPTH leading directory entries in SCRIPT-NAME."
  (if *serve-from-cache*
      (handle-loaded-file script-name)
      (let* ((components (nthcdr (+ 1 depth) (split-sequence #\/ script-name)))
             (filename   (format nil "~a/~{~a~^/~}" root components)))
        (hunchentoot:handle-static-file filename))))

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

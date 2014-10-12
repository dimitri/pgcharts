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

(defvar *url-to-dir-mapping* `(("js"         . ,*js-root*)
                               ("dist"       . ,*bootstrap-root*)
                               ("highcharts" . ,*highcharts-root*)
                               ("cm"         . ,*codemirror-root*)
                               ("images"     . ,*images-root*)
                               ("hallo"      . ,*hallo-root*)
                               ("fa"         . ,*fontawesome-root*))
  "Map URL first directory to its on-disk locations.")

(defparameter *fs*
  (let ((fs (make-hash-table :test #'equal)))
    (loop :for (first-dir . root ) :in *url-to-dir-mapping*
       :for url-path := (format nil "/~a" first-dir)
       :for root-dir := (uiop:ensure-directory-pathname root)
       :do (load-static-directory fs root-dir url-path))
    fs)
  "File system as an hash-table in memory.")

(defparameter *header* (read-file-into-string *header-path*))
(defparameter *footer* (read-file-into-string *footer-path*))

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


;;;
;;; Sub-directories
;;;
(defun serve-resource-from-file (&optional
                                   (script-name (hunchentoot:script-name*)))
  "Serve a static resource from a file"
  (destructuring-bind (first-dir &rest components)
      ;; the url always begins with a / and we skip it
      (cdr (split-sequence #\/ script-name))
    (let* ((root     (cdr (assoc first-dir *url-to-dir-mapping* :test #'string=)))
           (filename (format nil "~a/~{~a~^/~}" root components)))
      (hunchentoot:handle-static-file filename))))

(defun serve-resource-from-cache (&optional
                                    (script-name (hunchentoot:script-name*)))
  "Serve a static resource from the cache"
  (handle-loaded-file script-name))

(defun serve-resource ()
  "Serve a static resource"
  (if *serve-from-cache*
      (serve-resource-from-cache)
      (serve-resource-from-file)))

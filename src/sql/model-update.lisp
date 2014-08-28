(in-package #:pgcharts)

;;;
;;; To automate pgcharts model updates, we model a system after the
;;; PostgreSQL extension update mecanism.
;;;
;;; This is an implementation of Dijkstra's algorithm to find the shortest
;;; path copied from the PostgreSQL's source code (in C).
;;;
(defparameter *upgrade-scripts*
  (mapcar
   (lambda (pathname)
     (cons (pathname-name pathname) (read-queries pathname)))
   (remove-if-not (lambda (pathname)
                    (and (pathname-type pathname)
                         (string= "sql" (pathname-type pathname))))
                  (uiop:directory-files
                   (asdf:system-relative-pathname :pgcharts "src/sql/"))))
  "List of SQL upgrade scripts, each one being and alist of its pathname as
   the key and the script itself as the value." )

(defun pathname-to-versions (upgrade-script)
  "Given a PATHNAME, return source and target version of the script."
  (mapcar #'parse-integer (cl-ppcre:split "--" (car upgrade-script))))

(defun versions-to-pathname (versions)
  "Given a list of versions, returns the SQL script pathname."
  (format nil "~{~a~^--~}" versions))

(defun find-update-path (old-version new-version)
  "Pick an upgrade path given the list of available update scripts."
  (let* ((graph (make-instance 'graph:graph))
         (edges (mapcar #'pathname-to-versions *upgrade-scripts*)))
    (graph:populate graph :edges edges)

    (mapcar #'versions-to-pathname
            (graph:shortest-path graph
                                 (parse-integer old-version)
                                 (parse-integer new-version)))))


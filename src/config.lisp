(in-package #:pgcharts)

(defvar *config-filename* "~/.pgcharts.ini"
  "Where to store pgcharts configuration.")

(defparameter *dburi* nil
  "PostgreSQL database connection.")

(defparameter *listen-port* 9042
  "Port bound by the repository server, exposing the HTTP protocol.")

(defparameter *logfile* "/tmp/pgcharts.log"
  "Main logfile for pgcharts")

(defparameter *document-root*
  (asdf:system-relative-pathname :pgcharts "web/"))

(defparameter *js-root*
  (asdf:system-relative-pathname :pgcharts "web/js"))

(defparameter *bootstrap-root*
  (asdf:system-relative-pathname :pgcharts "web/bootstrap-3.1.1-dist"))

(defparameter *images-root*
  (asdf:system-relative-pathname :pgcharts "web/images"))

(defparameter *highcharts-root*
  (asdf:system-relative-pathname :pgcharts "web/Highcharts"))

(defparameter *d3js*
  (asdf:system-relative-pathname :pgcharts "web/d3js/d3.min.js"))

(defparameter *codemirror-js*
  (asdf:system-relative-pathname :pgcharts "web/codemirror-compressed.js"))

(defparameter *codemirror-css*
  (asdf:system-relative-pathname :pgcharts "web/codemirror-4.5/lib/codemirror.css"))

(defparameter *codemirror-theme-elegant*
  (asdf:system-relative-pathname :pgcharts "web/codemirror-4.5/theme/elegant.css"))

(defparameter *header-path*
  (asdf:system-relative-pathname :pgcharts "web/header.html"))

(defparameter *footer-path*
  (asdf:system-relative-pathname :pgcharts "web/footer.html"))

(defvar *serve-from-cache* nil
  "Set to t to serve static resources from in-memory cache.")


;;;
;;; System integration: configuration file.
;;;
(defun expand-user-homedir-pathname (namestring)
  "Expand NAMESTRING replacing leading ~ with (user-homedir-pathname)"
  (cond ((or (string= "~" namestring) (string= "~/" namestring))
         (user-homedir-pathname))

        ((and (<= 2 (length namestring))
              (char= #\~ (aref namestring 0))
              (char= #\/ (aref namestring 1)))
         (uiop:merge-pathnames* (uiop:parse-unix-namestring (subseq namestring 2))
                                (user-homedir-pathname)))

        (t
         (uiop:parse-unix-namestring namestring))))

(defun set-config-filename (namestring)
  (setf *config-filename* (expand-user-homedir-pathname namestring)))

(defun read-config (&optional (filename *config-filename*))
  "Read the INI configuration file at *config-filename*"
  (when (probe-file filename)
    (let* ((ini  (ini:make-config))
           (conf (ini:read-files ini (list filename))))
      (when (ini:has-section-p conf "pgcharts")
       ;; dburi
       (when (ini:has-option-p conf "pgcharts" "dburi")
         (setf *dburi* (ini:get-option conf "pgcharts" "dburi")))

       ;; listen-port
       (when (ini:has-option-p conf "pgcharts" "port")
         (setf *listen-port*
               (parse-integer (ini:get-option conf "pgcharts" "port"))))

       ;; logfile
       (when (ini:has-option-p conf "pgcharts" "logs")
         (setf *logfile*
               (expand-user-homedir-pathname
                (ini:get-option conf "pgcharts" "logs")))))

      ini)))

(defun write-config (&optional (pathname
                                (expand-user-homedir-pathname *config-filename*)))
  "Write current configuration into FILENAME."
  (with-open-file (s pathname
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :external-format :utf8)
    (let ((conf (ini:make-config)))
      (ini:add-section conf "pgcharts")
      (ini:set-option conf "pgcharts" "dburi" *dburi*)
      (ini:set-option conf "pgcharts" "port"  *listen-port*)
      (ini:set-option conf "pgcharts" "logs"  (uiop:native-namestring *logfile*))

      (ini:write-stream conf s)

      (values pathname conf))))

(defun config-value (key)
  "Return configuration value for KEY."
  (cond ((string-equal key "dburi") *dburi*)
        ((string-equal key "port")  *listen-port*)
        ((string-equal key "logs")  (uiop:native-namestring *logfile*))))

(defun (setf config-value) (val key)
  "Set configuration variable NAME to NEWVALUE."
  (cond ((string-equal key "dburi")
                    (let ((*dburi* val))
                      (validate-dburi *dburi*)
                      (write-config)))

                   ((string-equal key "port")
                    (let ((*listen-port* (parse-integer val)))
                      (write-config)))

                   ((string-equal key "logs")
                    (let ((*logfile* (expand-user-homedir-pathname val)))
                      (write-config)))

                   (t (error "Unknown parameter ~s.~%" key)))
  val)

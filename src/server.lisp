(in-package #:pgcharts)

(defvar *routes*
      (compile-routes
       ;; Home page
       (:GET  "/"                 'front-list-queries)

       ;; Resources
       (:GET  "/js/.*"            'serve-resource) ; Our own javascript glue
       (:GET  "/dist/.*"          'serve-resource) ; Bootstrap
       (:GET  "/highcharts/.*"    'serve-resource) ; HighCharts
       (:GET  "/images/.*"        'serve-resource) ; Static images
       (:GET  "/cm/.*"            'serve-resource) ; CodeMirror
       (:GET  "/hallo/.*"         'serve-resource) ; Hallo.js
       (:GET  "/fa/.*"            'serve-resource) ; FontAwesome

       ;; Server status and control
       (:GET  "/status"    'front-server-status)

       ;; Queries
       (:GET  "/q/new"     'front-new-query)
       (:GET  "/q/raw/:id" 'front-raw-query)
       (:GET  "/q/raw"     'front-raw-query)
       (:POST "/q/raw"     'front-raw-query) ; one-page style form
       (:POST "/q/save"    'front-save-query)
       (:GET  "/q/del/:id" 'front-delete-query)
       (:GET  "/q/:id"     'front-display-query)

       ;; Charts only
       (:GET "/c/:id"      'front-display-query-chart)

       ;; AJAX API to get at query result data
       (:POST "/json"      'front-fetch-json-data)
       (:POST "/csv"       'front-fetch-csv-data)

       ;; Database browser
       (:GET "/d/:dbname"  'front-browse-database)

       ;; Search
       (:GET "/s"          'front-search-queries)

       ;; Document editing, trying hallo.js for an SQL Notebook
       (:GET "/nb"         'front-new-notebook)))

(defvar *acceptor* nil "The Web Server")
(defvar *server-is-running* nil)

(defun start-server (&key (logs *terminal-io*))
  "Start the web server"
  (read-config)
  (when *acceptor*
    (error "The web server is already running."))

  (setf *acceptor* (make-instance 'simpleroutes-acceptor
                                  :routes '*routes*
                                  :port *listen-port*
                                  :document-root nil
                                  :access-log-destination logs
                                  :message-log-destination logs))
  (hunchentoot:start *acceptor*)
  (setf *server-is-running* t))

(defun stop-server ()
  "Stop the web server"
  (unless *acceptor*
    (error "The web server isn't running."))

  (hunchentoot:stop *acceptor*)
  (setf *acceptor* nil *server-is-running* nil))

(defun restart-server ()
  (stop-server)
  (start-server))

(defun front-server-status ()
  "Return OK when the server is OK."
  (setf (hunchentoot:content-type*) "text/plain")
  "OK")



(in-package #:pgcharts)

(defvar *routes*
      (compile-routes
       ;; User website
       (:GET  "/"                 'front-new-query)
       (:GET  "/js/.*"            'serve-pgcharts-js-file)
       (:GET  "/dist/.*"          'serve-bootstrap-file)
       (:GET  "/test/.*"          'serve-demo-data-file)
       (:GET  "/d3.js"            'serve-d3js)
       (:GET  "/d3.v3.js"         'serve-d3js)
       (:GET  "/d3.min.js"        'serve-d3js)
       (:GET  "/d3.v3.min.js"     'serve-d3js)
       (:GET  "/codemirror.js"    'serve-codemirror-js)
       (:GET  "/codemirror.css"   'serve-codemirror-css)
       (:GET  "/cm-s-elegant.css" 'serve-codemirror-theme-elegant)

       (:GET  "/status"    'front-server-status)

       (:GET  "/q"               'front-new-query)
       (:POST "/run"             'front-run-query)
       (:GET  "/q/:id"           'front-display-query)
       (:GET  "/chart/:kind/:id" 'front-display-graph-given-query-id)

       (:POST "/chart/:kind"     'front-display-graph-given-query-text)

       (:POST "/json"            'front-fetch-json-data)
       (:POST "/csv"             'front-fetch-csv-data)

       (:GET  "/demo/:chart"        'front-demo)
       ))

(defvar *acceptor* nil "The Web Server")
(defvar *server-is-running* nil)

(defun start-server ()
  "Start the web server"
  (when *acceptor*
    (error "The web server is already running."))

  (setf *acceptor* (make-instance 'simpleroutes-acceptor
                                  :routes '*routes*
                                  :port *listen-port*
                                  :document-root *document-root*
                                  :access-log-destination *terminal-io*
                                  :message-log-destination *terminal-io*))
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



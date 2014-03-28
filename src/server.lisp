(in-package #:pgcharts)

(setf *routeslist*
      (compile-routes
       ;; User website
       (:GET  "/"             'front-home)
       (:GET  "/dist/.*"      'serve-bootstrap-file)
       (:GET  "/test/.*"      'serve-test-file)
       (:GET  "/d3.js"        'serve-d3js)
       (:GET  "/d3.min.js"    'serve-d3js)
       (:GET  "/d3.v3.min.js" 'serve-d3js)

       (:GET  "/status"    'front-server-status)

       (:GET  "/q/:id"           'front-display-query)
       (:GET  "/chart/:kind/:id" 'front-display-graph-for-query)

       (:GET  "/demo/:chart"        'front-test)
       ))

(defvar *acceptor* nil "The Web Server")
(defvar *server-is-running* nil)

(defun start-server ()
  "Start the web server"
  (when *acceptor*
    (error "The web server is already running."))

  (setf *acceptor* (make-instance 'simpleroutes-acceptor
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



(defpackage #:pgcharts.dburi
  (:use #:cl #:esrap)
  (:import-from #:postmodern
                #:with-connection
                #:query)
  (:export #:parse-pgsql-connection-string
           #:validate-dburi
           #:with-pgsql-connection))

(defpackage #:pgcharts.sql
  (:use #:cl)
  (:export #:read-queries))

(defpackage #:pgcharts
  (:use #:cl
        #:postmodern
        #:simple-routes
        #:cl-who
        #:pgcharts.dburi
        #:pgcharts.sql)
  (:import-from #:alexandria
                #:read-file-into-string
                #:read-file-into-byte-vector)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:cl-postgres
                #:database-error
                #:database-error-code
                #:database-error-message
                #:database-error-detail
                #:database-error-hint
                #:database-error-context
                #:database-error-query
                #:database-error-position
                #:database-error-cause)
  (:export #:*acceptor*
           #:*server-is-running*
           #:start-server
           #:stop-server
           #:restart-server))

;;;
;;; Package aliasing
;;;
(rename-package 'py-configparser 'py-configparser '(ini))

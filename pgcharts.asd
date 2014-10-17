;;;; pgcharts.asd

(asdf:defsystem #:pgcharts
    :serial t
    :description "Repository of PostgreSQL Extensions"
    :author "Dimitri Fontaine <dimitri@2ndQuadrant.fr>"
    :license "The PostgreSQL Licence"
    :depends-on (#:uiop			; host system integration
		 #:postmodern		; PostgreSQL protocol implementation
                 #:simple-date          ; Handling of pgsql date formats
		 #:esrap		; parser generator
		 #:py-configparser	; Read old-style INI config files
                 #:split-sequence       ; split strings
                 #:cl-ppcre             ; Regular Expressions
                 #:alexandria           ; Some utilities
                 #:hunchentoot          ; http server
                 #:yason                ; JSON routines
                 #:closer-mop           ; introspection
                 #:daemon               ; run the repo server in the background
                 #:cl-who               ; HTML production from lisp code
                 #:trivial-backtrace    ; Produces backtraces
                 #:drakma               ; HTTP client, to check server status
                 #:graph                ; Find SQL model upgrade paths
		 )
    :components
    ((:module "lib"
              :components
              ((:file "simple-routes")))
     (:module "src"
              :depends-on ("lib")
	      :components
              ((:file "package")
               (:module utils
                        :depends-on ("package")
                        :components ((:file "dburi")
                                     (:file "cache")
                                     (:file "cli-parser")
                                     (:file "read-sql-files")))
               (:module sql
                        :depends-on ("package" "utils")
                        :components ((:file "model-update")))

               (:file "config"         :depends-on ("package" "utils"))
               (:file "model"          :depends-on ("package" "utils" "sql"))

               ;; frontend
               (:file "resources"   :depends-on ("package" "utils"))
               (:file "front-tools" :depends-on ("package" "utils"))
               (:file "front-db"    :depends-on ("package" "utils" "front-tools"))
               (:file "front-main"  :depends-on ("package" "utils" "front-tools"))
               (:file "front-query" :depends-on ("package" "utils" "front-tools"))
               (:file "front-notebook" :depends-on ("package" "utils" "front-tools"))

               (:file "front-raw-query" :depends-on ("package"
                                                     "utils"
                                                     "front-tools"
                                                     "front-query"))

               ;; http server control and main routing
               (:file "server"   :depends-on ("package"
                                              "utils"
                                              "front-tools"
                                              "front-main"
                                              "front-db"
                                              "front-query"
                                              "front-notebook"))

               ;; main entry point for the binary (command line)
               (:file "pgcharts" :depends-on ("package" "utils"))))))


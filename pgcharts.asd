;;;; pgcharts.asd

(asdf:defsystem #:pgcharts
    :serial t
    :description "Repository of PostgreSQL Extensions"
    :author "Dimitri Fontaine <dimitri@2ndQuadrant.fr>"
    :license "The PostgreSQL Licence"
    :depends-on (#:uiop			; host system integration
		 #:postmodern		; PostgreSQL protocol implementation
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
                 #:cl-markdown          ; HTML production from Markdown docs
                 #:trivial-backtrace    ; Produces backtraces
		 )
    :components
    ((:module "lib"
              :components
              ((:file "simple-routes")))
     (:module "src"
              :depends-on ("lib")
	      :components
              ((:file "package")
               (:file "dburi"          :depends-on ("package"))
               (:file "config"         :depends-on ("package"))
               (:file "cache"          :depends-on ("package"))
               (:file "read-sql-files" :depends-on ("package"))
               (:file "model"          :depends-on ("package"
                                                    "config"
                                                    "read-sql-files"
                                                    "dburi"))

               ;; frontend
               (:file "resources"   :depends-on ("package" "config"))
               (:file "front-db"    :depends-on ("package" "config" "dburi"))
               (:file "front-tools" :depends-on ("package" "config" "dburi"))
               (:file "front-main"  :depends-on ("package"
                                                 "config"
                                                 "dburi"))
               (:file "front-query" :depends-on ("package"
                                                 "config"
                                                 "read-sql-files"
                                                 "dburi"))

               ;; http server control and main routing
               (:file "server"   :depends-on ("package"
                                              "config"
                                              "front-tools"
                                              "front-main"
                                              "front-query"))
               (:file "pgcharts" :depends-on ("package"))))))


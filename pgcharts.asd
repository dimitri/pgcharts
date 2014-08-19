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
               (:file "read-sql-files" :depends-on ("package"))
               (:file "model"          :depends-on ("package"
                                                    "config"
                                                    "read-sql-files"
                                                    "dburi"))

               (:file "frontend"    :depends-on ("package"
                                                 "config"
                                                 "read-sql-files"
                                                 "dburi"))
               (:file "frontend-db" :depends-on ("package"
                                                 "config"
                                                 "read-sql-files"
                                                 "dburi"))
               (:file "server"   :depends-on ("package"
                                              "config"
                                              "frontend"
                                              "frontend-db"))
               (:file "pgcharts" :depends-on ("package"))))))


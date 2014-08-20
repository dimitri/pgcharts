(in-package #:pgcharts)

;;;
;;; User and groups and privileges management
;;;
(defun front-manage-users ()
  "NIY"
  (serve-page
   (with-html-output-to-string (s)
     (htm
      (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
            (:h1 "Users and Groups Management")
            (:p "This module will allow to create users and groups and give
            them privileges to use databases connection, so as not to give
            them either the privilege to use pgcharts as an admin, nor the
            connection string details.") )))))

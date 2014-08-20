(in-package #:pgcharts)

;;;
;;; Main dashboard
;;;
(defun front-dashboard ()
  "NIY"
  (serve-page
   (with-html-output-to-string (s)
     (htm
      (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
            (:h1 "pgcharts dashboard")
            (:p "Soon, a nice dashboard showing the list of databases you
            did setup to be able to work with pgcharts, and maybe some
            direct link to most recently used queries and graphs and things
            like that.") )))))

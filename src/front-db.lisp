(in-package #:pgcharts)

;;;
;;; Database objects browser.
;;;

(defun front-browse-database (dbname)
  "Not Yet Implemented"
  (serve-page
   (with-html-output-to-string (s)
     (htm
      (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
            (:h2 :class "page-header"
                 (:span :class "glyphicon glyphicon-th-list"
                        " database browser for: " (str dbname)))
            (:p "Soon, a nice database objects browser. Or something."))))))

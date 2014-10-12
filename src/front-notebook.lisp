(in-package #:pgcharts)

;;;
;;; Frontend for editing a notebook, thanks to Hallo
;;;
(defun front-new-notebook ()
  "Return the basic HTML for a new notebook"
  (serve-page
   (with-html-output-to-string (s)
     (htm
      (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
            (:div :class "editable" :contenteditable "true"
                  (:h1 "Your new SQL Notebook title")
                  (:p "Edit your text here, whatever you want")))))))

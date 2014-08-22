(in-package #:pgcharts)

;;;
;;; General tools to render frontend code
;;;
(defun compute-menu (current-url-path)
  "List all files found in the *DOCROOT* directory and turns the listing
   into a proper bootstrap menu."
  (when *dburi*
    ;; all the entries in the menu only work properly with a database
    ;; connection (that has been setup), so refrain from displaying them
    ;; when the basic setup has not been done yet.
    (with-html-output-to-string (s)
      (htm
       (:div :class "col-sm-3 col-md-2 sidebar"
             (:ul :class "nav nav-sidebar"
                  (:li :class (when (string= current-url-path "/") "active")
                       (:a :href "/" (:span :class "glyphicon glyphicon-filter"
                                            " "
                                            (str "Queries"))))
                  (:li :class (when (string= current-url-path "/q/raw") "active")
                       (:a :href "/q/raw" (:span :class "glyphicon glyphicon-pencil"
                                                 " "
                                                 (str "New Query")) ))
                  (:li :class (when (string= current-url-path "/q/new") "active")
                       (:a :href "/q/new" (:span :class "glyphicon glyphicon-tasks"
                                                 " "
                                                 (str "New Chart")) ))
                  (:hr)
                  (loop :for db
                     :in (with-pgsql-connection (*dburi*)
                           (select-dao 'db t 'dbname))
                     :for active := (string= (dbname db) current-url-path)
                     :do (htm
                          (:li :class (when active "active")
                               (:a :href (format nil "/d/~a" (dbname db))
                                   (:span :class "glyphicon glyphicon-folder-open")
                                   " "
                                   (str (dbname db))))))))))))

(defmacro serve-page (content)
  "Return the content with header and footer and menu, and handle errors."
  `(concatenate 'string
                (serve-header)
                (compute-menu "/#q")

                (handler-case
                    ,content
                  (condition (e)
                    (with-html-output-to-string (s)
                      (htm
                       (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
                             (:h2 :class "page-header"
                                  (:span :class "glyphicon glyphicon-eye-close"
                                         " Unexpected condition"))
                             (:h4 :style "color: red; text-weight: bold;"
                                    (str (format nil "~a" e)))
                             (:pre
                              (str
                               (trivial-backtrace:print-backtrace e
                                                                  :output nil
                                                                  :verbose t))))))))

                (serve-footer)))

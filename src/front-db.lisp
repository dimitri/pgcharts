(in-package #:pgcharts)

;;;
;;; Database objects browser.
;;;

(defun front-browse-database (dbname)
  "Not Yet Implemented"
  (let* ((qdburi     (with-pgsql-connection (*dburi*)
                       (db-uri (get-dao 'db dbname))))
         (table-list (with-pgsql-connection (qdburi)
                       (query "
          select nspname, relname, relkind,
                 string_agg(a.attname, ', ' order by a.attnum) as cols
            from pg_class c
                 join pg_namespace n on n.oid = c.relnamespace
                 left join pg_attribute a on a.attrelid = c.oid and attnum > 0
           where n.nspname not in ('pg_catalog', 'information_schema')
                 and relkind in ('r', 'v')
                 and has_table_privilege(c.oid, 'SELECT')
                 and pg_table_is_visible(c.oid)
        group by nspname, relname, relkind
        order by nspname, relname" :rows))))
    (serve-page
     (with-html-output-to-string (s)
       (htm
        (:div :class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"
              (:h2 :class "page-header" (str dbname))
              (:div :class "table-responsive"
                    (:table :class "table table-stripped table-hover"
                            (:thead
                             (:tr (:th "Schema")
                                  (:th "Table&nbsp;name")
                                  (:th "Columns")))
                            (:tbody
                             (loop :for (nspname relname relkind cols) :in table-list
                                :do (htm
                                     (:tr
                                      (:td (str nspname))
                                      (:td (str relname))
                                      (:td (str cols))))))))))))))

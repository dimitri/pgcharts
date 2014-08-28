---
--- Upgrade database model from version 20140823 to version 20140828
---

create table pgcharts.catalog
(
  version     text primary key
);

create table pgcharts.new_db
(
  dbname      text primary key,
  dburi       text
);

insert into pgcharts.new_db
     select dbname,    'pgsql://'
                    || coalesce(dbuser, '')
                    || case when dbpass is not null and dbpass <> 'false'
                            then ':' || dbpass
                            else ''
                        end
                    || case when dbhost is not null
                            then case when dbuser is not null
                                      then '@' || dbhost
                                      else dbhost
                                 end
                            else ''
                         end
                    || case when dbport is not null
                            then ':' || dbport
                            else ''
                        end
                    || '/'
                    || dbname
       from pgcharts.db;

alter table pgcharts.db rename to old_db;
alter table pgcharts.new_db rename to db;

insert into pgcharts.catalog(version) values('20140828');

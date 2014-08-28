---
--- pgcharts model
---
create schema if not exists pgcharts;

create table pgcharts.catalog
(
  version     text primary key
);

create table pgcharts.db
(
  dbname      text primary key,
  dburi       text
);

-- the minvalue is just so that we begin with 3 letters URLs
create sequence pgcharts.query_id_seq minvalue 10000;

create table pgcharts.query
(
  id            bigint not null default nextval('pgcharts.query_id_seq') primary key,
  db            text not null references pgcharts.db(dbname),
  qname         text unique not null,
  description   text,
  sql           text,
  cats          text,
  series        text,
  x_title       text,
  y_title       text,
  chart_type    text
);

alter sequence pgcharts.query_id_seq owned by pgcharts.query.id;

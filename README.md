# PostgreSQL Charts

The *pgcharts* projects is a little web application that takes as input an
SQL query text and outputs its data in one of the following forms:

  - HTML table
  - CSV file
  - Pie Chart
  - Donut Chart
  - Donut Chart
  - Area Chart
  - Line Chart
  - Stacked Area Chart
  - Bar Chart
  - Stacked Bar Chart
  - Grouped Bar Chart
  
# Implementation

pgchart needs a database where to handle its own data, as it is storing a
list of database connections (where to run the queries) and a list of
queries (with a name and a tags list).

TODO: see about storing query results on the *pgcharts* database so that
      it's possible to get back to them later. Maybe with some ways to run
      the query again and compare?

# Usage

Connect to the web application. Use it.

# Install

The usual bits about `make` and all.

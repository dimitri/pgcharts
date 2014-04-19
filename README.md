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

# Roles

The *admin* is the only role allowed to edit database connections, and
assign (grant) their usage to unprivileged pgcharts users.

# Authentication

Passwords are dead, and never where a great idea anyways. We're using GnuPG
asymetric encryption challenge instead:

  - at user creation, a first-login tokenized URL is generated
  - at user first login, a GnuPG keypair is generated on the client
  - at user login, the server sends a challenge encrypted to the user's key
  - the users browser must answer with the challenge encrypted to the server's key

# Usage

pgcharts is a self-contained web application. As such, when you start the
command line application, it starts its own web server that you can connect
to.

When starting the application for the first time, you can either use it
directly with the default admin account, or setup some other unprivileged
roles that are going to be using the application.

As an admin here's what you can do:

  - add database connections
  - add roles within groups
  - grant groups the *usage* of some database connections
  - grant groups the *snapshot* ability
  
As a user of pgcharts, here's what you can do:

  - run SQL queries and see charts of their result
  - save SQL queries with some properties
  - save a snapshot of a query, when given privileges to do so
  - re-use a saved query
  - share a read-only link to a snapshot you took

# Install

The usual bits about `make` and all.

# PostgreSQL Charts

## Screen Shots

It's intended as a visaul project. Here's what it looks like:

![pgcharts-query](https://raw.github.com/dimitri/pgcharts/master/propaganda/pgcharts-query.png)

![pgcharts-bar-chart](https://raw.github.com/dimitri/pgcharts/master/propaganda/pgcharts-bar-chart.png)

## Description

The *pgcharts* projects is a little web application that takes as input an
SQL query text and outputs its data in one of the following forms:

  - HTML table
  - Column Chart
  - Bar Chart
  - Pie Chart
  - Donut Chart
  
With more to come (TODO):

  - Area Chart
  - Line Chart
  - Stacked Area Chart
  - Stacked Bar Chart
  - Grouped Bar Chart
  - CSV file

# Initial Setup

The *pgcharts* application needs its own PostgreSQL database to be able to
register user queries and their charts setup:

    $ createdb pgcharts
    $ pgcharts setup pgsql://localhost/pgcharts
    
Once the database has been created, it's necessary to *register* the
database servers you want to run queries against:
    
    $ pgcharts register pgsql://user:pass@host/dbname
    $ pgcharts register pgsql://user:pass@host/seconddbname
    
Then you can start the service, which defaults to listening to
[http://localhost:9042/]():

    $ pgcharts start
    $ open http://localhost:9042/

Now, you can use *pgcharts* from your browser. Issue new query, save them
away, and see nice charts from their results!

# Implementation

pgchart needs a database where to handle its own data, as it is storing a
list of database connections (where to run the queries) and a list of
queries (with a name and a tags list).

TODO: see about storing query results on the *pgcharts* database so that
      it's possible to get back to them later. Maybe with some ways to run
      the query again and compare?

# Security

The *pgcharts* web service offers no security implementation, no user role
management or privileges. To keep the service secure, users are only allowed
to query against *registered* database servers.

To register a database server to *pgcharts*, the command line interface must
be used, so only the service administrator is in a position to register new
database servers.

# Usage

pgcharts is a self-contained web application. As such, when you start the
command line application, it starts its own web server that you can connect
to.

# Install

The *pgcharts* application has been written in Common Lisp and uses a bunch
of librairies that are available through the *Quicklisp* distribution
system. The included `Makefile` cares about building a self-contained binary
for you, and can be used as following:

    $ <install recent sbcl>
    $ make
    $ ./build/bin/pgcharts --help

Note that the self-contained binary also includes static web resources such
as *jquery*, *bootstrap*, *Highcharts* and *codemirror*.

## Build Dependencies

You need a recent enough [SBCL](http://sbcl.org/) Common Lisp compiler to be
able to compile pgcharts. It's easy to install on Linux, MacOSX and Windows.

    debian$ sudo apt-get install sbcl
    centos$ sudo yum install sbcl
    macosx$ brew install sbcl

When using `debian stable` you might need to *backport* a recent enough
version of the compiler, because stable contains a very old version of it as
seen at [http://packages.debian.org/search?keywords=sbcl](). You will find
my backport at [http://pgsql.tapoueh.org/sbcl/]() to get you started
quickly, or apply the following recipe:

    $ <add sid as a deb-src source in /etc/apt/sources.list.d/*
    $ sudo apt-get udpate
    $ sudo apt-get build-dep sbcl
    $ sudo apt-get source -b sbcl
    $ sudo dpkg -i <resulting sbcl-*.deb>

That's about it.

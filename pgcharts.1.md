# pgcharts(1) -- PostgreSQL data loader

## SYNOPSIS

`pgcharts` [<options>] [<command-file>]...

## DESCRIPTION

pgcharts is The PostgreSQL Extension Installer server.

## OPTIONS

  * `-h`, `--help`:
    Show command usage summary and exit.

  * `-V`, `--version`:
    Show pgcharts version string and exit.

  * `-c`, `--config`:
    Use the given configuration file (default to "~/.pgcharts.ini").

## COMMANDS

The pgcharts binary allows running and controling the pgcharts embedded web
server.

### CONFIGURATION CONTROL

While it's possible to ship a configuration file or to prepare it by hand,
the following commands allow to control the setup from the command line.

  - `config [ name ] [ value ]`
     
    Without arguments, print the whole configuration file content. When
    given a variable *name*, print its current value. When given both a
    *name* and a *value*, set the configuration variable to the given value.
     
  - `config get <name>`
    
    Print the current value of the configuration variable *name*.

  - `config set <name> <value>`
    
    Set the variable *name* to the given *value*.

### SERVER CONTROL

The PostgreSQL Extension Installer comes with a PostgreSQL plugin that
downloads static files: that part doesn't need any server at all. This
server is meant to be used by maintainers of a set of extension archives,
when they want to ease the maintenance and setup of the building.

  - `start`
     
    Start the embedded pgcharts HTTP server on the port it's been setup to
    listen to, which defaults to 8042. The
     
  - `stop`
     
    Stops the server.
     
  - `status`
    
    Print the result of querying the HTTP status API against the (hopefully)
    running server.

  - `pid`
    
    Print the registered pid of the server process. This information might
    be stale in case of unexpected termination of the server.

  - `server reload`
    
    Forces the server to reload its configuration file.
    
  - `setup <dburi>`
    
    Connects to the PostgreSQL database specified with the *dburi* parameter
    and install the database model there.

### REGISTERING DATABASES

Once pgcharts is properly setup (see the `setup` command above) then it's
necessary to add databases against which you want to run queries and draw
charts.

  - `register <dburi>`
  
    Register given *dburi*.

## DATABASE URI

The *dburi* connection string is expected to be given as a *Connection URI*
as documented in the PostgreSQL documentation at
http://www.postgresql.org/docs/9.3/static/libpq-connect.html#LIBPQ-CONNSTRING.

    postgresql://[user[:password]@][netloc][:port][/dbname][?sslmode=...]

Where:

  - *user*

    Can contain any character, including colon (`:`) which must then be
    doubled (`::`) and at-sign (`@`) which must then be doubled (`@@`).
    
    When omitted, the *user* name defaults to the value of the `PGUSER`
    environment variable, and if it is unset, the value of the `USER`
    environment variable.

  - *password*

	Can contain any character, including that at sign (`@`) which must then
	be doubled (`@@`). To leave the password empty, when the *user* name
	ends with at at sign, you then have to use the syntax user:@.

    When omitted, the *password* defaults to the value of the `PGPASSWORD`
    environment variable if it is set, otherwise the password is left
    unset.

  - *netloc*

    Can be either a hostname in dotted notation, or an ipv4, or an Unix
    domain socket path. Empty is the default network location, under a
    system providing *unix domain socket* that method is preferred, otherwise
    the *netloc* default to `localhost`.

	It's possible to force the *unix domain socket* path by using the syntax
	`unix:/path/to/where/the/socket/file/is`, so to force a non default
	socket path and a non default port, you would have:

	    postgresql://unix:/tmp:54321/dbname

    The *netloc* defaults to the value of the `PGHOST` environment
    variable, and if it is unset, to either the default `unix` socket path
    when running on a Unix system, and `localhost` otherwise.

  - *dbname*

	Should be a proper identifier (letter followed by a mix of letters,
	digits and the punctuation signs comma (`,`), dash (`-`) and underscore
	(`_`).

    When omitted, the *dbname* defaults to the value of the environment
    variable `PGDATABASE`, and if that is unset, to the *user* value as
    determined above.

  - The only optional parameter supported is `sslmode` and it accepts the
    values `disable`, `allow`, `prefer` and `require`.

## AUTHOR

Dimitri Fontaine <dimitri@2ndQuadrant.fr>

## SEE ALSO

The pgcharts source code and all documentation may be downloaded from
<https://github.com/dimitri/pgcharts/>.

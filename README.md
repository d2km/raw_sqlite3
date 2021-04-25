raw_sqlite3
=====

`raw_sqlite3` library is a thin, low-level NIF wrapper for [SQLite](https://sqlite.org/index.html) C API.

The main motivation for the library is to have a low-level wrapper which
exposes as much as of SQLite C API as possible, excluding anything that is
outdated, dangerous, or just makes little or no sense for using in Erlang code.

One of the design goals was to have many, potentially tens of thousands DB
connections in one Erlang node. To achieve this, the library avoids spawning
extra OS threads by making relevant functions to run on dirty schedulers.

The library is believed to be safe to use, including automatic object
de-allocation, although, it might still be possible to crash the VM because
there is no built-in validation for passed parameters (e.g. nothing will
prevent you from passing `SQLITE_OPEN_NOMUTEX` flag which will likely crash the
VM).

With great power comes great responsibility. Use with caution!

## Building

Running `rebar3 compile` should just work on any modern Linux and FreeBSD system.

To have a better control over the enabled SQLite features, the library includes
the complete SQLite amalgamation.  The features are defined in `c_src/Makefile`.
In particular, `SQLITE_CFLAGS` variable defines the available features.

## Testing

The library comes with a fairly comprehensive test suite which can be run with
`rebar3 eunit`.

## Usage

### High-level interface

The `raw_sqlite3` module contains high-level wrappers for common usage
patterns.  The interface is stable and considered to be
production-ready. However, it does not expose some of the more interesting
SQlite3 APIs.

#### Opening a database connection

```erlang
{ok, Db} = raw_sqlite3:open("/path/to/my/file")
%% everywhere where a string is expected it is possible to pass in a binary
%% provided that it contains a properly UTF8-encoded character sequence.
{ok, Db} = raw_sqlite3:open(<<"/path/to/my/file">>)
%% More specialised version accepts flags (defined in raw_sqlite3.hrl)
Flags = ?SQLITE_OPEN_READONLY bor ?SQLITE_OPEN_CREATE
{ok, Db} = raw_sqlite3:open(<<"path to my file">>, Flags)
```
There is a function which converts atoms to flags. This may be handy when
the library is used from Elixir.

```elixir
open_flags = [:SQLITE_OPEN_READONLY, :SQLITE_OPEN_URI, :SQLITE_OPEN_CREATE]
             |> :raw_sqlite3.make_flags()
```

Closing an open connection is not necessary since it will be closed
automatically as soon as the `Db` term will be garbage-collected. In some
situations, however, it is desirable to explicitly close a DB connection to
de-allocate resources and ensure that all pending writes have been finished.

```erlang
raw_sqlite3:close(Db)
```

The `close/1` function is idempotent, but an attempt to use a closed connection
in any other function will generate the `badarg` exception.

#### Running DQL/DML/DDL queries

The go-to functions are `q/2-3` and `exec/2-3`. The difference between the two
is that `q/2-3` assumes that the provided string is a single query which
returns some result (i.e. `SELECT` expression), while `exec/2-3` evaluates
every SQL statement and discards the result, indicating only success of the operation.

```erlang
%% select from table binding the query parameter
Items = raw_sqlite3:q(Db, "SELECT * FROM t WHERE id=?", [42])
%% evaluate multiple statements
ok = raw_sqlite3:exec(Db, "CREATE TABLE t(c TEXT); INSERT INTO t VALUES ('hello');")
```

Note: `exec/3` tries to bind parameters to every expression, so it mostly makes
sense when used with a single DML expression.

There is a specialised function to insert many values at once (probably should
be called within `with_trxn/2` context)

```erlang
Values = [[1, 'hello'], [2, 'world'], [3, 'universe']],
ok = raw_sqlite3:insert_many(Db, "INSERT INTO t VALUES (?, ?)", Values)
```

#### Higher-kind functions

It is possible to process query results without creating an intermediate list
with the `fold/4-5` function

```erlang
%% Sum all expenses from the table where amount > 42.
%% This can be done in SQL itself, of course, but maybe be handy for side effects
Total = raw_sqlite3:fold(Db, "SELECT amount FROM expenses WHERE amount > ?",
                         _QueryParameters = [42],
                         fun(Elem, Acc) -> Elem + Acc end,
                         _Acc = 0)
```

There is also the `map/3-4` function which applies a transformation to every
result value without intermediate list allocation

```erlang
Whatevers = raw_sqlite3:map(Db, "SELECT * FROM items", fun(Elem) -> do_whatever(Elem) end)
```

And finally, there is `with_trxn/2` function which begins/commits a transaction
and automatically reverts it if the function throws an exception

```erlang
ok = raw_sqlite3:with_trxn(Db, fun() ->
         %% a sequence of raw_sqlite3:q|exec|fold|map calls,
         %% perhaps interleaved with business logic
     end)
```

### Low-level interface

The `sqlite3_nif` module provides a low-level interface to SQLite. It allows
more precise control over the SQLite usage as well as access to the
less-frequently-used SQLite APIs.

For instance, this is how a database file can be efficiently copied into a
freshly created in-memory database

```erlang
%% NOTE: no error handling!
init() ->
    Flags = ?SQLITE_OPEN_CREATE bor ?SQLITE_OPEN_READWRITE,
    {ok, Db} = sqlite3_nif:sqlite3_open_v2("/my/db", Flags, ""),
    {ok, DbMem} = sqlite3_nif:sqlite3_open_v2(":memory:", Flags, ""),
    {ok, Backup} = sqlite3_nif:sqlite3_backup_init(DbMem, "main", Db, "main"),
    do_backup(Backup).

do_backup(Backup) ->
    ?SQLITE_DONE = sqlite3_nif:sqlite3_backup_step(Backup, -1),
    ?SQLITE_OK = sqlite3_nif:sqlite3_backup_finish(Backup).
```

Essentially, the `sqlite3_nif` module provides wrappers for the every SQLite C API function,
except:

 * Deprecated functions;
 * Potentially dangerous, global state mutating functions, such as `sqlite3_config`;
 * Functions which require a function pointer;
 * `sqlite3_bind_*` functions which are replaced by the unified `sqlite3_bind/2`;
 * `sqlite3_column_*` functions which are merged into `sqlite3_step/1`;
 * `sqlite3_mutex_*` functions which make no sense in Erlang code;
 * API related to user functions creation (if you think that you may have a
   use case for Erlang-based callbacks, please create an issue);
 * Unicode variants of the C API (only UTF-8 versions are allowed);
 * Memory managing functions;
 * `_vfs_*` functions;

See `test/happy_path_tests.erl` for more examples and consult [SQLite C API
documentation](https://sqlite.org/c3ref/intro.html) for details.


## Documentation
EDoc-generated documentation available at
[https://hexdocs.pm/raw_sqlite3](https://hexdocs.pm/raw_sqlite3/).

Tutorial
====


# Using high-level interface

The high-level interface provides convenience wrappers around more commonly
used low-level functions. It is fairly stable and considered to be
production-ready. However, it does not expose some of the more interesting
SQlite3 APIs.

## Opening a database connection

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
automatically as soon as the `Db` term garbage-collected. However, to have more
tight control it is possible to forcefully close it.

```erlang
raw_sqlite3:close(Db)
```

The `close/1` function is idempotent, however using a closed connection will
throw the `badarg` exception.

## Running DQL/DML/DDL queries

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

## Higher-kind functions

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

# Using low-level interface

The low-level interface provides more precise control over what exactly is
called and also access to less-frequently-used SQLite APIs.

For instance, this is how a database can be efficiently copied into in-memory
database

```erlang
%% Warning: no error handling!
init() ->
    Flags = ?SQLITE_OPEN_CREATE bor ?SQLITE_OPEN_READWRITE,
    {ok, Db} = sqlite3_nif:sqlite3_open_v2("/my/db", Flags, ""),
    {ok, DbMem} = sqlite3_nif:sqlite3_open_v2(":memory:", Flags, ""),
    {ok, Backup} = sqlite3_nif:sqlite3_backup_init(DbMem, "main", Db, "main"),
    do_backup(Backup).

do_backup(Backup) ->
    ?SQLITE_DONE = sqlite3_backup_step(Backup, -1),
    ?SQLITE_OK = sqlite3_backup_finish(Backup).
```

See `test/happy_path_tests.erl` for more examples and consult [SQLite C API
documentation](https://sqlite.org/c3ref/intro.html) for details.

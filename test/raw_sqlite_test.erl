-module(raw_sqlite_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("raw_sqlite3/include/sqlite3_nif.hrl").

%% exec() should ignore whitespaces
exec_whitespace_test() ->
    Sql = "BEGIN;"
        "CREATE TABLE t(id INTEGER PRIMARY KEY, txt TEXT);"
        "INSERT INTO t(id, txt) VALUES (1, 'hello world');"
        "COMMIT; ",
    {ok, Db} = raw_sqlite3:open(":memory:"),
    ?assertEqual(ok, raw_sqlite3:exec(Db, Sql)).

insert_many_test() ->
    Sql1 = "CREATE TABLE t(id INTEGER PRIMARY KEY, txt TEXT)",
    Sql2 = "INSERT INTO t(id, txt) VALUES (?, ?)",
    Values = [[1, {text, "hello"}], [2, {text, "world"}]],
    {ok, Db} = raw_sqlite3:open(":memory:"),
    ?assertEqual(ok, raw_sqlite3:exec(Db, Sql1)),
    ?assertEqual(ok, raw_sqlite3:insert_many(Db, Sql2, Values)),
    ?assertEqual([{2}], raw_sqlite3:q(Db, "SELECT count(*) FROM t")).

-module(raw_sqlite3_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("raw_sqlite3/include/sqlite3_nif.hrl").

%% exec() should ignore whitespaces
exec_whitespace_test() ->
    Sql = "BEGIN;"
        "CREATE TABLE t(id INTEGER PRIMARY KEY, txt TEXT);"
        "INSERT INTO t(id, txt) VALUES (1, 'hello world');"
        "COMMIT; \r\n\t",
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

%% prepare should ignore leftover statements
prepare_statement_test() ->
    Sqls = [erlang:list_to_binary(["SELECT * FROM t;" || _ <- lists:seq(1, N)])
            || N <- lists:seq(1, 100)],
    Sql1 = "SELECT y FROM t",
    {ok, Db} = raw_sqlite3:open(":memory:"),
    ok = raw_sqlite3:exec(Db, "CREATE TABLE t(x INTEGER)"),
    lists:map(fun(Sql) -> ?assertMatch({ok, _}, raw_sqlite3:prepare(Db, Sql)) end, Sqls),
    ?assertMatch({error, _}, raw_sqlite3:prepare(Db, Sql1)).
   

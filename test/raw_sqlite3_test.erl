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

%% reset should return ok on success and {error, Err} on error
reset_ret_test() ->
    Sql1 = "CREATE TABLE t(id INTEGER PRIMARY KEY, txt TEXT)",
    Sql2 = "SELECT * FROM t",
    Sql3 = "DROP TABLE t",
    {ok, Db} = raw_sqlite3:open(":memory:"),
    ok = raw_sqlite3:exec(Db, Sql1),
    {ok, Stmt} = raw_sqlite3:prepare(Db, Sql2),
    ?assertEqual(ok, raw_sqlite3:reset(Stmt)),
    ok = raw_sqlite3:exec(Db, Sql3),
    {error, _} = raw_sqlite3:step(Stmt),
    ?assertMatch({error, _}, raw_sqlite3:reset(Stmt)).

%% close should return ok on success
close_ret_test() ->
    {ok, Db} = raw_sqlite3:open(":memory:"),
    ?assertEqual(ok, raw_sqlite3:close(Db)).

bind_test() ->
    Sql1 = "CREATE TABLE t(id INTEGER)",
    Sql2 = "INSERT INTO t(id) VALUES (?)",
    {ok, Db} = raw_sqlite3:open(":memory:"),
    ok = raw_sqlite3:exec(Db, Sql1),
    {ok, Stmt1} = raw_sqlite3:prepare(Db, Sql2),

    % bind should return `ok` on success
    ?assertEqual(ok, raw_sqlite3:bind(Stmt1, [{text, "hello"}])),

    % bind should return `wrong_parameter_count` error with too many parameters
    ?assertEqual({error, wrong_parameter_count},
                 raw_sqlite3:bind(Stmt1, [{text, "hello"}, {text, "world"}])),

    % bind should return `wrong_parameter_count` error with too few parameters
    ?assertEqual({error, wrong_parameter_count},
                 raw_sqlite3:bind(Stmt1, [])),

    % Generate insert queries with multiple parameters, one of which is incorrect.
    Count = 10,
    lists:foreach(fun(N) ->
                    Q = ["INSERT INTO t(id) VALUES " | lists:join(",", ["(?)" ||
                        _ <- lists:seq(1, Count)])],
                    P = [{text, "hello"} || _ <- lists:seq(1, N - 1)] ++
                        [{text, 1}] ++
                        [{text, "hello"} || _ <- lists:seq(1, Count - N)],
                    {ok, S} = raw_sqlite3:prepare(Db, Q),

                    % bind should return `{wrong_parameter_type, N}` error when the Nth
                    % parameter is malformed.
                    ?assertEqual({error, {wrong_parameter_type, N}}, raw_sqlite3:bind(S, P))
                  end, lists:seq(1, Count)).

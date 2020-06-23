-module(stress_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("raw_sqlite3/include/sqlite3_nif.hrl").

%% many writes
%% The idea is to test creating/finalizing many prepared statements
many_writes_single_proc_test() ->
    NWrites = 10000,
    Sql = "CREATE TABLE t(id INTEGER PRIMARY KEY, txt TEXT)",
    Sql2 = "INSERT INTO t(id, txt) VALUES (?, ?)",
    Sql3 = "SELECT count(*) FROM t",
    F = fun(Db, N) ->
                Txt = io_lib:format("text~p", [N]),
                raw_sqlite3:exec(Db, Sql2, [N, {text, lists:flatten(Txt)}]),
                ?assertMatch([{N}], raw_sqlite3:q(Db, Sql3))
        end,
    {ok, Db} = raw_sqlite3:open(":memory:"),
    ?assertEqual(ok, raw_sqlite3:exec(Db, Sql)),
    [F(Db, N) || N <- lists:seq(1, NWrites)],
    ?assertMatch([{NWrites}], raw_sqlite3:q(Db, Sql3)).

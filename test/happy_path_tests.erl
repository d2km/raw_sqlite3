-module(happy_path_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("raw_sqlite3/include/sqlite3_nif.hrl").

-import(sqlite3_nif, [sqlite3_open_v2/3,
                      sqlite3_close_v2/1,
                      sqlite3_limit/3,
                      sqlite3_load_extension/3,
                      sqlite3_db_cacheflush/1,
                      sqlite3_db_release_memory/1,
                      sqlite3_db_config/3,
                      sqlite3_set_last_insert_rowid/2,
                      sqlite3_busy_timeout/2,
                      sqlite3_extended_result_codes/2,
                      sqlite3_interrupt/1,
                      sqlite3_get_autocommit/1,
                      sqlite3_last_insert_rowid/1,
                      sqlite3_changes/1,
                      sqlite3_total_changes/1,
                      sqlite3_db_filename/2,
                      sqlite3_db_readonly/2,
                      sqlite3_db_status/3,
                      sqlite3_txn_state/2,
                      sqlite3_prepare_v2/2,
                      sqlite3_bind/2,
                      sqlite3_step/1,
                      sqlite3_finalize/1,
                      sqlite3_reset/1,
                      sqlite3_clear_bindings/1,
                      sqlite3_data_count/1,
                      sqlite3_sql/1,
                      sqlite3_expanded_sql/1,
                      sqlite3_normalized_sql/1,
                      sqlite3_stmt_busy/1,
                      sqlite3_stmt_isexplain/1,
                      sqlite3_stmt_readonly/1,
                      sqlite3_stmt_status/3,
                      sqlite3_backup_init/4,
                      sqlite3_backup_step/2,
                      sqlite3_backup_finish/1,
                      sqlite3_backup_pagecount/1,
                      sqlite3_backup_remaining/1,
                      sqlite3_blob_open/6,
                      sqlite3_blob_bytes/1,
                      sqlite3_blob_close/1,
                      sqlite3_blob_read/3,
                      sqlite3_blob_reopen/2,
                      sqlite3_blob_write/3,
                      sqlite3_errcode/1,
                      sqlite3_extended_errcode/1,
                      sqlite3_errmsg/1,
                      sqlite3_errstr/1,
                      sqlite3_wal_autocheckpoint/2,
                      sqlite3_wal_checkpoint_v2/3,
                      sqlite3_compileoption_get/1,
                      sqlite3_compileoption_used/1,
                      sqlite3_libversion/0,
                      sqlite3_libversion_number/0,
                      sqlite3_sourceid/0,
                      sqlite3_complete/1,
                      sqlite3_snapshot_get/2,
                      sqlite3_snapshot_open/3,
                      sqlite3_snapshot_cmp/2,
                      sqlite3_snapshot_free/1,
                      sqlite3_snapshot_recover/2,
                      sqlite3_serialize/2,
                      sqlite3_deserialize/3]).

-define(CRW, ?SQLITE_OPEN_CREATE bor ?SQLITE_OPEN_READWRITE).
-define(RO, ?SQLITE_OPEN_READONLY).
-define(URI, ?SQLITE_OPEN_URI).
-define(NOMUTEX, ?SQLITE_OPEN_NOMUTEX).
-define(FULLMUTEX, ?SQLITE_OPEN_FULLMUTEX).
-define(MEMORY, ?SQLITE_OPEN_MEMORY).

open_v2_test_() ->
    TestCases = [{":memory:", ?CRW, "", "CRW"},
                 {"/foo", ?CRW bor ?MEMORY, "", "CRW|MEMORY"},
                 {"/tmp/open_test/foo", ?CRW, "", "CRW"},
                 {"/tmp/open_test/foo", ?RO, "", "RO"},
                 {"file:/foo?mode=memory", ?CRW bor ?URI, "", "CRW|URI"},
                 {"file:/tmp/open_test/foo?mode=ro", ?RO bor ?URI, "", "RO|URI"},
                 {"/tmp/open_test/foo", ?CRW bor ?NOMUTEX, "", "CRW|NOMUTEX"},
                 {"/tmp/open_test/foo", ?CRW bor ?FULLMUTEX, "", "CRW|FULLMUTEX"}
                ],
    F = fun({Path, Flags, Vfs, StrFlags}) ->
                Title = io_lib:format("sqlite3_open_v2(~p, ~p, ~p)",
                                      [Path, StrFlags, Vfs]),
                {lists:flatten(Title),
                 ?_assertMatch({ok, _}, sqlite3_open_v2(Path, Flags, Vfs))}
        end,
    Tests = lists:map(F, TestCases),
    Setup = fun() -> ?cmd("mkdir -p /tmp/open_test") end,
    Cleanup = fun(_) -> ?cmd("rm -rf /tmp/open_test") end,
    {setup, Setup, Cleanup, Tests}.

close_v2_test_() ->
    Path = "/tmp/close_test/foo",
    OpenClose = fun() ->
                        {ok, Db} = sqlite3_open_v2(Path, ?CRW, ""),
                        Err = sqlite3_nif:sqlite3_close_v2(Db),
                        ?assertEqual(?SQLITE_OK, Err)
                end,
    CloseTwice = fun() ->
                         {ok, Db} = sqlite3_open_v2(Path, ?CRW, ""),
                         Err = sqlite3_close_v2(Db),
                         ?assertEqual(?SQLITE_OK, Err),
                         Err2 = sqlite3_close_v2(Db),
                         ?assertEqual(?SQLITE_OK, Err2)
                 end,
    Setup = fun() -> ?cmd("mkdir -p /tmp/close_test") end,
    Cleanup = fun(_) -> ?cmd("rm -rf /tmp/close_test") end,
    Tests = [{"open/close test", OpenClose},
             {"close twice test", CloseTwice}],
    {setup, Setup, Cleanup, Tests}.

limit_test_() ->
    Limits = [
              ?SQLITE_LIMIT_LENGTH,
              ?SQLITE_LIMIT_SQL_LENGTH,
              ?SQLITE_LIMIT_COLUMN,
              ?SQLITE_LIMIT_EXPR_DEPTH,
              ?SQLITE_LIMIT_COMPOUND_SELECT,
              ?SQLITE_LIMIT_VDBE_OP,
              ?SQLITE_LIMIT_FUNCTION_ARG,
              ?SQLITE_LIMIT_ATTACHED,
              ?SQLITE_LIMIT_LIKE_PATTERN_LENGTH,
              ?SQLITE_LIMIT_VARIABLE_NUMBER,
              ?SQLITE_LIMIT_TRIGGER_DEPTH,
              ?SQLITE_LIMIT_WORKER_THREADS
             ],
    lists:map(
      fun(Limit) ->
              Title = io_lib:format("sqlite3_limit(~p)", [Limit]),
              F = fun() ->
                          {ok, Db} = sqlite3_open_v2("", ?CRW, ""),
                          Val = sqlite3_limit(Db, Limit, -1),
                          NewVal = case Val of 0 -> 0; _ -> Val - 1 end,
                          _ = sqlite3_limit(Db, Limit, NewVal),
                          Val2 = sqlite3_limit(Db, Limit, -1),
                          ?assertEqual(Val2, NewVal)
                  end,
              {lists:flatten(Title), F}
      end, Limits).

%% TODO: add test extension
%% load_extension_test() ->
%%     ok.

db_cacheflush_test_() ->
    Path = "/tmp/cachflush_test",
    Setup = fun() ->
                    ?cmd("mkdir -p " ++ Path),
                    {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                    Db
            end,
    Cleanup = fun(Db) ->
                      sqlite3_close_v2(Db),
                      ?cmd("rm -rf " ++ Path)
              end,
    Inst = fun(Db) ->
                   ?_assertEqual(?SQLITE_OK, sqlite3_db_cacheflush(Db))
           end,
    {setup, Setup, Cleanup, Inst}.

db_release_memory_test_() ->
    Path = "/tmp/db_release_memory_test",
    Setup = fun() ->
                    ?cmd("mkdir -p " ++ Path),
                    {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                    Db
            end,
    Cleanup = fun(Db) ->
                      sqlite3_close_v2(Db),
                      ?cmd("rm -rf " ++ Path)
              end,
    Inst = fun(Db) ->
                   ?_assertEqual(?SQLITE_OK, sqlite3_db_release_memory(Db))
           end,
    {setup, Setup, Cleanup, Inst}.


db_config_test_() ->
    Path = "/tmp/db_config_test",
    Setup = fun() ->
                    ?cmd("mkdir -p " ++ Path),
                    {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                    Db
            end,
    Cleanup = fun(Db) ->
                      sqlite3_close_v2(Db),
                      ?cmd("rm -rf " ++ Path)
              end,
    ConfOpts = [
                {?SQLITE_DBCONFIG_MAINDBNAME, "main2", ?SQLITE_OK},
                {?SQLITE_DBCONFIG_ENABLE_FKEY, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_ENABLE_VIEW, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_ENABLE_TRIGGER, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_ENABLE_QPSG, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_TRIGGER_EQP, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_DEFENSIVE, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_WRITABLE_SCHEMA, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_LEGACY_ALTER_TABLE, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_DQS_DML, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_DQS_DDL, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_TRUSTED_SCHEMA, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_LEGACY_FILE_FORMAT, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_RESET_DATABASE, 1, {?SQLITE_OK, 1}},
                {?SQLITE_DBCONFIG_RESET_DATABASE, 0, {?SQLITE_OK, 0}}
               ],
    I = fun(Db) ->
                M = fun({Opt, Val, ExpectedRv}) ->
                            Title = io_lib:format(
                                      "sqlite3_db_config(~p, ~p)", [Opt, Val]
                                     ),
                            F = fun() ->
                                        Rv = sqlite3_db_config(Db, Opt, Val),
                                        ?assertEqual(ExpectedRv, Rv)
                                end,
                            {lists:flatten(Title), F}
                    end,
                lists:map(M, ConfOpts)
        end,
    {setup, Setup, Cleanup, I}.

set_last_insert_rowid_test() ->
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    ok = sqlite3_set_last_insert_rowid(Db, 10),
    ?assertEqual(10, sqlite3_last_insert_rowid(Db)).

busy_timeout_test() ->
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    Rv = sqlite3_busy_timeout(Db, 100),
    ?assertEqual(Rv, ?SQLITE_OK).


extended_result_codes_test() ->
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    Rv = sqlite3_extended_result_codes(Db, 1),
    ?assertEqual(Rv, ?SQLITE_OK).

interrupt_test() ->
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    ok = sqlite3_interrupt(Db).

get_autocommit_test() ->
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    ?assertEqual(1, sqlite3_get_autocommit(Db)),
    ok = raw_sqlite3:exec(Db, "BEGIN"),
    ?assertEqual(0, sqlite3_get_autocommit(Db)),
    ok = raw_sqlite3:exec(Db, "COMMIT"),
    ?assertEqual(1, sqlite3_get_autocommit(Db)).

last_insert_rowid_test() ->
    Sql = "CREATE TABLE t (id INTEGER PRIMARY KEY);"
        "INSERT INTO t(id) VALUES(10);",
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    ok = raw_sqlite3:exec(Db, Sql),
    ?assertEqual(10, sqlite3_last_insert_rowid(Db)).

changes_test() ->
    N = 100,
    MakeVal = fun(I) ->
                      lists:flatten(io_lib:format("(~B, 'hello~B')", [I, I]))
              end,
    Sql = "CREATE TABLE t (id INTEGER PRIMARY KEY, x TEXT);"
        "INSERT INTO t(id, x) VALUES " ++
        lists:join(", ", [ MakeVal(I) || I <- lists:seq(1, 100)]) ++ ";",
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    ok = raw_sqlite3:exec(Db, Sql),
    ?assertEqual(N, sqlite3_changes(Db)).

total_changes_test() ->
    N = 100,
    MakeVal = fun(I) ->
                      lists:flatten(io_lib:format("(~B, 'hello~B')", [I, I]))
              end,
    Sql = "CREATE TABLE t (id INTEGER PRIMARY KEY, x TEXT);"
        "INSERT INTO t(id, x) VALUES " ++
        lists:join(", ", [ MakeVal(I) || I <- lists:seq(1, 100)]) ++ ";",
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    ok = raw_sqlite3:exec(Db, Sql),
    ok = raw_sqlite3:exec(Db, "UPDATE t SET x=id"),
    ?assertEqual(2*N, sqlite3_total_changes(Db)).

db_filename_test_() ->
    Path = "/tmp/db_filename_test",
    Setup = fun() ->
                    ?cmd("mkdir -p " ++ Path),
                    {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                    Db
            end,
    Cleanup = fun(Db) ->
                      sqlite3_close_v2(Db),
                      ?cmd("rm -rf " ++ Path)
              end,
    Inst = fun(Db) ->
                   fun() ->
                           DbFname = sqlite3_db_filename(Db, "main"),
                           DbNameBin = iolist_to_binary(DbFname),
                           PathBin = iolist_to_binary(Path ++ "/foo"),
                           ?assertEqual(PathBin, DbNameBin)
                   end
           end,
    {setup, Setup, Cleanup, Inst}.

db_readonly_test_() ->
    Path = "/tmp/db_readonly_test",
    Sql = "ATTACH DATABASE 'file:" ++ Path ++ "/ro?mode=ro' AS ro",
    Setup = fun() ->
                    ?cmd("mkdir -p " ++ Path),
                    {ok, DbRO} = sqlite3_open_v2(Path ++ "/ro", ?CRW, ""),
                    ?SQLITE_OK = sqlite3_close_v2(DbRO),
                    {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                    ok = raw_sqlite3:exec(Db, Sql),
                    Db
            end,
    Cleanup = fun(Db) ->
                      sqlite3_close_v2(Db),
                      ?cmd("rm -rf " ++ Path)
              end,
    Inst = fun(Db) ->
                   [?_assertEqual(0, sqlite3_db_readonly(Db, "main")),
                    ?_assertEqual(1, sqlite3_db_readonly(Db, "ro")),
                    ?_assertEqual(-1, sqlite3_db_readonly(Db, "nonexistent"))]
           end,
    {setup, Setup, Cleanup, Inst}.

db_status_test_() ->
    Path = "/tmp/db_status_test",
    Setup = fun() ->
                    ?cmd("mkdir -p " ++ Path),
                    {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                    Db
            end,
    Cleanup = fun(Db) ->
                      sqlite3_close_v2(Db),
                      ?cmd("rm -rf " ++ Path)
              end,
    Flags = [?SQLITE_DBSTATUS_LOOKASIDE_USED,
             ?SQLITE_DBSTATUS_CACHE_USED,
             ?SQLITE_DBSTATUS_SCHEMA_USED,
             ?SQLITE_DBSTATUS_STMT_USED,
             ?SQLITE_DBSTATUS_LOOKASIDE_HIT,
             ?SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE,
             ?SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL,
             ?SQLITE_DBSTATUS_CACHE_HIT,
             ?SQLITE_DBSTATUS_CACHE_MISS,
             ?SQLITE_DBSTATUS_CACHE_WRITE,
             ?SQLITE_DBSTATUS_DEFERRED_FKS,
             ?SQLITE_DBSTATUS_CACHE_USED_SHARED,
             ?SQLITE_DBSTATUS_CACHE_SPILL],
    T = fun(Flag, Db) ->
                Title = io_lib:format("sqlite3_db_status(~B, 0)", [Flag]),
                F = fun() ->
                            Rv = sqlite3_db_status(Db, Flag, 0),
                            ?assertMatch({L , H} when is_integer(L)
                                                      andalso is_integer(H),
                                                      Rv)
                    end,
                {lists:flatten(Title), F}
        end,
    Inst = fun(Db) ->
                   [T(Flag, Db) || Flag <- Flags]
           end,
    {setup, Setup, Cleanup, Inst}.

txn_state_test_() ->
    Path = "/tmp/txn_state_test",
    Setup = fun() ->
                    ?cmd("mkdir -p " ++ Path),
                    {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                    Sql = "CREATE TABLE t(c1 TEXT); INSERT INTO t VALUES ('hello'), ('world')",
                    ok = raw_sqlite3:exec(Db, Sql),
                    Db
            end,
    Cleanup = fun(Db) ->
                      _ = raw_sqlite3:exec(Db, "ROLLBACK"),
                      sqlite3_close_v2(Db),
                      ?cmd("rm -rf " ++ Path)
              end,

    T1 = fun(Db) ->
                 Title = "sqlite3_txn_state arg error",
                 F = fun() ->
                             Rv = sqlite3_txn_state(Db, "nonexistent"),
                             ?assertEqual(-1, Rv)
                     end,
                 {lists:flatten(Title), F}
         end,
    T2 = fun(Db) ->
                 Title = "sqlite3_txn_state no txn",
                 F = fun() ->
                             Rv = sqlite3_txn_state(Db, "main"),
                             ?assertEqual(?SQLITE_TXN_NONE, Rv)
                     end,
                 {lists:flatten(Title), F}
         end,
    T3 = fun(Db) ->
                 Title = "sqlite3_txn_state read",
                 F = fun() ->
                             ok = raw_sqlite3:exec(Db, "BEGIN"),
                             _ = raw_sqlite3:q(Db, "SELECT * FROM t"),
                             Rv = sqlite3_txn_state(Db, "main"),
                             ?assertEqual(?SQLITE_TXN_READ, Rv)
                     end,
                 {lists:flatten(Title), F}
         end,
    T4 = fun(Db) ->
                 Title = "sqlite3_txn_state write",
                 F = fun() ->
                             _ = raw_sqlite3:exec(Db, "ROLLBACK"),
                             ok = raw_sqlite3:exec(Db, "BEGIN"),
                             ok = raw_sqlite3:exec(Db, "INSERT INTO t VALUES ('hi')"),
                             Rv = sqlite3_txn_state(Db, "main"),
                             ?assertEqual(?SQLITE_TXN_WRITE, Rv)
                     end,
                 {lists:flatten(Title), F}
         end,
    Inst = fun(Db) ->
                   [T1(Db), T2(Db), T3(Db), T4(Db)]
           end,
    {setup, Setup, Cleanup, Inst}.


prepare_v2_test() ->
    Leftover = " CREATE TABLE t2(c3 TEXT)",
    Sql = "CREATE TABLE t1(c1 TEXT, c2 TEXT);" ++ Leftover,
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    Rv = sqlite3_prepare_v2(Db, Sql),
    ?assertMatch({ok, _, _}, Rv),
    {ok, _Stmt, Leftover1} = Rv,
    LeftoverBin = iolist_to_binary(Leftover),
    Leftover1Bin = iolist_to_binary(Leftover1),
    ?assertEqual(LeftoverBin, Leftover1Bin).

bind_and_step_test() ->
    Sql1 = "CREATE TABLE "
        "t(c_int INTEGER, c_text TEXT, c_blob BLOB, c_bool BOOL, c_opt TEXT)",
    Sql2 = "INSERT INTO t(c_int, c_text, c_blob, c_bool, c_opt)"
        " VALUES (?, ?, ?, ?, ?)",
    Sql3 = "SELECT * FROM t",
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    ?assertEqual(ok, raw_sqlite3:exec(Db, Sql1)),
    {ok, Stmt2, _} = sqlite3_prepare_v2(Db, Sql2),
    Params = [1, {text, "hello"}, {blob, <<"world">>}, false, nil],
    ?assertEqual(ok, sqlite3_bind(Stmt2, Params)),
    ?assertEqual(ok, sqlite3_step(Stmt2)),
    {ok, Stmt3, _} = sqlite3_prepare_v2(Db, Sql3),
    {Int, Text, Blob, Bool, nil} = sqlite3_step(Stmt3),
    ?assertEqual(iolist_to_binary(Text), <<"hello">>),
    ?assertEqual(iolist_to_binary(Blob), <<"world">>),
    ?assertEqual(1, Int),
    ?assertEqual(0, Bool).

finalize_test() ->
    Sql = "CREATE TABLE t(c TEXT)",
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    {ok, Stmt, _} = sqlite3_prepare_v2(Db, Sql),
    ?assertEqual(ok, sqlite3_step(Stmt)),
    ?assertEqual(?SQLITE_OK, sqlite3_finalize(Stmt)),
    %% finalize should be idempotent
    ?assertEqual(?SQLITE_OK, sqlite3_finalize(Stmt)).

data_count_test() ->
    Sql1 = "CREATE TABLE "
        "t(c_int INTEGER, c_text TEXT, c_blob BLOB, c_bool BOOL, c_opt TEXT)",
    Sql2 = "INSERT INTO t(c_int, c_text, c_blob, c_bool, c_opt)"
        " VALUES (?, ?, ?, ?, ?)",
    Sql3 = "SELECT * FROM t",
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    ?assertEqual(ok, raw_sqlite3:exec(Db, Sql1)),
    {ok, Stmt2, _} = sqlite3_prepare_v2(Db, Sql2),
    Params = [1, {text, "hello"}, {blob, <<"world">>}, false, nil],
    ?assertEqual(ok, sqlite3_bind(Stmt2, Params)),
    ?assertEqual(ok, sqlite3_step(Stmt2)),
    {ok, Stmt3, _} = sqlite3_prepare_v2(Db, Sql3),
    ?assertMatch({_Int, _Text, _Blob, _Bool, nil}, sqlite3_step(Stmt3)),
    ?assertEqual(5, sqlite3_data_count(Stmt3)),
    ?assertEqual(ok, sqlite3_step(Stmt3)),
    ?assertEqual(0, sqlite3_data_count(Stmt3)).

sql_test() ->
    Sql = <<"CREATE TABLE t(c TEXT)">>,
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    {ok, Stmt, _} = sqlite3_prepare_v2(Db, Sql),
    SqlRv = sqlite3_sql(Stmt),
    SqlRvBin = iolist_to_binary(SqlRv),
    ?assertEqual(Sql, SqlRvBin).

expanded_sql_test() ->
    Sql = "SELECT upper(?)",
    ExpandedSql = <<"SELECT upper('hello')">>,
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    {ok, Stmt, _} = sqlite3_prepare_v2(Db, Sql),
    ?assertEqual(ok, sqlite3_bind(Stmt, [{text, "hello"}])),
    ?assertEqual(ExpandedSql, sqlite3_expanded_sql(Stmt)).

normalized_sql_test() ->
    Sql = "SELECT upper(?)",
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    {ok, Stmt, _} = sqlite3_prepare_v2(Db, Sql),
    ?assertEqual(ok, sqlite3_bind(Stmt, [{text, "hello"}])),
    %% NOTE: from https://sqlite.org/c3ref/expanded_sql.html
    %% > The sqlite3_normalized_sql(P) interface returns a pointer to a UTF-8
    %% > string containing the normalized SQL text of prepared statement P. The
    %% > semantics used to normalize a SQL statement are unspecified and
    %% > subject to change.
    ?assertMatch(S when is_binary(S), sqlite3_normalized_sql(Stmt)).

stmt_busy_test() ->
    Sql = "SELECT sqlite_version()",
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    {ok, Stmt, _} = sqlite3_prepare_v2(Db, Sql),
    ?assertEqual(0, sqlite3_stmt_busy(Stmt)),
    ?assertMatch({_}, sqlite3_step(Stmt)),
    ?assertEqual(1, sqlite3_stmt_busy(Stmt)),
    ?assertEqual(?SQLITE_OK, sqlite3_reset(Stmt)),
    ?assertEqual(0, sqlite3_stmt_busy(Stmt)).

stmt_isexplain_test() ->
    Sql = "SELECT sqlite_version()",
    ExplainSql = "EXPLAIN SELECT sqlite_version()",
    ExplainPlanSql = "EXPLAIN QUERY PLAN SELECT sqlite_version()",
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    {ok, Stmt1, _} = sqlite3_prepare_v2(Db, Sql),
    ?assertEqual(0, sqlite3_stmt_isexplain(Stmt1)),
    {ok, Stmt2, _} = sqlite3_prepare_v2(Db, ExplainSql),
    ?assertEqual(1, sqlite3_stmt_isexplain(Stmt2)),
    {ok, Stmt3, _} = sqlite3_prepare_v2(Db, ExplainPlanSql),
    ?assertEqual(2, sqlite3_stmt_isexplain(Stmt3)).

stmt_readonly_test() ->
    Sql1 = "SELECT sqlite_version()",
    Sql2 = "CREATE TABLE t(c TEXT)",
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    {ok, Stmt1, _} = sqlite3_prepare_v2(Db, Sql1),
    ?assertEqual(1, sqlite3_stmt_readonly(Stmt1)),
    {ok, Stmt2, _} = sqlite3_prepare_v2(Db, Sql2),
    ?assertEqual(0, sqlite3_stmt_readonly(Stmt2)).

stmt_status_test_() ->
    %% NOTE: it's not clear how to test that exactly because actual values may differ
    %% accross SQLite versions and different environments.
    Path = "/tmp/stmt_status_test",
    Setup = fun() ->
                    ?cmd("mkdir -p " ++ Path),
                    {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                    Db
            end,
    Cleanup = fun(Db) ->
                      sqlite3_close_v2(Db),
                      ?cmd("rm -rf " ++ Path)
              end,
    Flags = [
             ?SQLITE_STMTSTATUS_FULLSCAN_STEP,
             ?SQLITE_STMTSTATUS_SORT,
             ?SQLITE_STMTSTATUS_AUTOINDEX,
             ?SQLITE_STMTSTATUS_VM_STEP,
             ?SQLITE_STMTSTATUS_REPREPARE,
             ?SQLITE_STMTSTATUS_RUN,
             ?SQLITE_STMTSTATUS_MEMUSED
            ],
    T = fun(Flag, Db) ->
                Title = io_lib:format("sqlite3_stmt_status(~B, 0)", [Flag]),
                {ok, Stmt, _} = sqlite3_prepare_v2(Db, "SELECT sqlite_version()"),
                F = fun() ->
                            Rv = sqlite3_stmt_status(Stmt, Flag, 0),
                            ?assertMatch(R when is_integer(R), Rv)
                    end,
                {lists:flatten(Title), F}
        end,
    Inst = fun(Db) ->
                   [T(Flag, Db) || Flag <- Flags]
           end,
    {setup, Setup, Cleanup, Inst}.

backup_test_() ->
    Path = "/tmp/backup_test",
    PathDb = Path ++ "/foo",
    MakeVal = fun(I) ->
                      lists:flatten(io_lib:format("(~B, 'hello~B')", [I, I]))
              end,
    InsertSql = "CREATE TABLE t (id INTEGER PRIMARY KEY, x TEXT);"
        "INSERT INTO t(id, x) VALUES " ++
        lists:join(", ", [ MakeVal(I) || I <- lists:seq(1, 100)]) ++ ";",
    Setup = fun() -> ?cmd("mkdir -p " ++ Path) end,
    Cleanup = fun(_) -> ?cmd("rm -rf " ++ Path) end,
    T = fun() ->
                {ok, Db} = sqlite3_open_v2(PathDb, ?CRW, ""),
                ?assertEqual(ok, raw_sqlite3:exec(Db, InsertSql)),
                {ok, DbMem} = sqlite3_open_v2(":memory:", ?CRW, ""),
                {ok, Backup} = sqlite3_backup_init(DbMem, "main", Db, "main"),
                ?assertEqual(?SQLITE_OK, sqlite3_backup_step(Backup, 1)),
                PageCount = sqlite3_backup_pagecount(Backup),
                Remaining = sqlite3_backup_remaining(Backup),
                ?assertMatch(X when is_integer(X) andalso X > 0, PageCount),
                ?assertEqual(PageCount - 1, Remaining),
                ?assertEqual(?SQLITE_DONE, sqlite3_backup_step(Backup, -1)),
                ?assertEqual(0, sqlite3_backup_remaining(Backup)),
                ?assertEqual(?SQLITE_OK, sqlite3_backup_finish(Backup))
        end,
    {setup, Setup, Cleanup, T}.

blob_test_() ->
    Path = "/tmp/blob_test",
    Sql = "CREATE TABLE t (id INTEGER PRIMARY KEY, x TEXT);"
        "INSERT INTO t(id, x) VALUES (1, zeroblob(10)), (2, NULL);",
    Setup = fun() -> ?cmd("mkdir -p " ++ Path) end,
    Cleanup = fun(_) -> ?cmd("rm -rf " ++ Path) end,
    T = fun() ->
                {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                ?assertEqual(ok, raw_sqlite3:exec(Db, Sql)),
                {ok, Blob} = sqlite3_blob_open(Db, "main", "t", "x", 1, 1),
                ?assertEqual(10, sqlite3_blob_bytes(Blob)),
                ?assertEqual(ok, sqlite3_blob_write(Blob, "Greetings!", 0)),
                Rv = sqlite3_blob_read(Blob, 10, 0),
                ?assertMatch({ok, <<"Greetings!">>}, Rv),
                Sql2 = "UPDATE t SET x=? WHERE id=2",
                ok = raw_sqlite3:exec(Db, Sql2, [{blob, "hello world"}]),
                ?assertEqual(?SQLITE_OK, sqlite3_blob_reopen(Blob, 2)),
                Rv2 = sqlite3_blob_read(Blob, 11, 0),
                ?assertMatch({ok, <<"hello world">>}, Rv2)
        end,
    {setup, Setup, Cleanup, T}.

err_test() ->
    {ok, Db} = sqlite3_open_v2(":memory:", ?CRW, ""),
    {error, Rv} = sqlite3_prepare_v2(Db, "creat table foo(x)"),
    ?assertEqual(?SQLITE_ERROR, Rv),
    ?assertEqual(?SQLITE_ERROR, sqlite3_errcode(Db)),
    ?assertEqual(?SQLITE_ERROR, sqlite3_extended_errcode(Db)),
    ?assertEqual(<<"near \"creat\": syntax error">>, sqlite3_errmsg(Db)),
    ?assertEqual(<<"SQL logic error">>, sqlite3_errstr(Rv)).


wal_checkpoint_test_() ->
    Path = "/tmp/wal_checkpoint_test",
    Sql = "PRAGMA journal_mode=WAL;"
        "CREATE TABLE t(x TEXT)",
    Sql2 = "INSERT INTO t(x) VALUES(?)",
    Setup = fun() -> ?cmd("mkdir -p " ++ Path) end,
    Cleanup = fun(_) -> ?cmd("rm -rf " ++ Path) end,
    T = fun() ->
                {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                ?assertEqual(ok, raw_sqlite3:exec(Db, Sql)),
                ?assertEqual(?SQLITE_OK, sqlite3_wal_autocheckpoint(Db, 10)),
                Rv1 = sqlite3_wal_checkpoint_v2(Db, "main", ?SQLITE_CHECKPOINT_PASSIVE),
                ?assertMatch({ok, {2, 2}}, Rv1),
                ?assertEqual(ok, raw_sqlite3:exec(Db, Sql2, [{text, "hello"}])),
                Rv2 = sqlite3_wal_checkpoint_v2(Db, "main", ?SQLITE_CHECKPOINT_FULL),
                ?assertMatch({ok, {1, 1}}, Rv2),
                ?assertEqual(ok, raw_sqlite3:exec(Db, Sql2, [{text, "world"}])),
                Rv3 = sqlite3_wal_checkpoint_v2(Db, "main", ?SQLITE_CHECKPOINT_RESTART),
                ?assertMatch({ok, {1, 1}}, Rv3),
                ?assertEqual(ok, raw_sqlite3:exec(Db, Sql2, [{text, "!"}])),
                Rv4 = sqlite3_wal_checkpoint_v2(Db, "main", ?SQLITE_CHECKPOINT_TRUNCATE),
                ?assertMatch({ok, {0, 0}}, Rv4)
        end,
    {setup, Setup, Cleanup, T}.

compileoption_test() ->
    OptList = fun F(N) ->
                     case sqlite3_compileoption_get(N) of
                         nil -> [];
                         Opt -> [Opt | F(N+1)]
                     end
              end,
    [?assertEqual(1, sqlite3_compileoption_used(Opt)) || Opt <- OptList(0)].

-define(VSN, <<"3.34.0">>).
-define(VSN_NUM, 3034000).
libversion_test() ->
    ?assertEqual(?VSN, sqlite3_libversion()),
    ?assertEqual(?VSN_NUM, sqlite3_libversion_number()).

-define(SOURCE_ID, <<"2020-12-01 16:14:00 a26b6597e3ae272231b96f9982c3bcc17ddec2f2b6eb4df06a224b91089fed5b">>).
sourceid_test() ->
    ?assertEqual(?SOURCE_ID, sqlite3_sourceid()).

complete_test() ->
    ?assertEqual(1, sqlite3_complete("CREATE foo;")).

%% sqlite3_snapshot_get/2,
%% sqlite3_snapshot_open/3,
%% sqlite3_snapshot_cmp/2,
%% sqlite3_snapshot_free/1,
%% sqlite3_snapshot_recover/2,
snapshot_test_() ->
    Path = "/tmp/snapshot_test",
    Sql = "PRAGMA journal_mode=WAL;"
        "CREATE TABLE t(x TEXT)",
    Sql2 = "INSERT INTO t(x) VALUES(?)",
    Setup = fun() -> ?cmd("mkdir -p " ++ Path) end,
    Cleanup = fun(_) -> ?cmd("rm -rf " ++ Path) end,
    T = fun() ->
                {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                ?assertEqual(ok, raw_sqlite3:exec(Db, Sql)),
                GetSnapshot = fun() -> sqlite3_snapshot_get(Db, "main") end,
                Rv1 = raw_sqlite3:with_trxn(Db, GetSnapshot),
                ?assertMatch({ok, _}, Rv1),
                {ok, Snapshot1} = Rv1,
                ?assertEqual(ok, raw_sqlite3:exec(Db, Sql2, [{text, "hello"}])),
                ?assertEqual(ok, raw_sqlite3:exec(Db, Sql2, [{text, "world"}])),
                ?assertEqual(ok, raw_sqlite3:exec(Db, "BEGIN")),
                ?assertEqual(?SQLITE_OK, sqlite3_snapshot_open(Db, "main", Snapshot1)),
                Rows = raw_sqlite3:q(Db, "SELECT * FROM t"),
                ?assertEqual(0, length(Rows)),
                ?assertEqual(ok, raw_sqlite3:exec(Db, "COMMIT")),
                Rv2 = raw_sqlite3:with_trxn(Db, GetSnapshot),
                ?assertMatch({ok, _}, Rv2),
                {ok, Snapshot2} = Rv2,
                Rv3 = sqlite3_snapshot_cmp(Snapshot1, Snapshot2),
                ?assert(Rv3 < 0),
                ?assertEqual(ok, sqlite3_snapshot_free(Snapshot1)),
                ?assertEqual(ok, sqlite3_snapshot_free(Snapshot2)),
                ?assertEqual(?SQLITE_OK, sqlite3_snapshot_recover(Db, "main"))
        end,
    {setup, Setup, Cleanup, T}.

serialize_test_() ->
    Path = "/tmp/snapshot_test",
    Sql = "PRAGMA journal_mode=WAL;"
        "CREATE TABLE t(x TEXT);"
        "INSERT INTO t(x) VALUES ('hello'), ('world')",
    Setup = fun() -> ?cmd("mkdir -p " ++ Path) end,
    Cleanup = fun(_) -> ?cmd("rm -rf " ++ Path) end,
    T = fun() ->
                {ok, Db} = sqlite3_open_v2(Path ++ "/foo", ?CRW, ""),
                ?assertEqual(ok, raw_sqlite3:exec(Db, Sql)),
                Rv = sqlite3_serialize(Db, "main"),
                ?assertMatch({ok, _}, Rv),
                {ok, S} = Rv,
                {ok, Db2} = sqlite3_open_v2(":memory:", ?CRW, ""),
                ?assertEqual(?SQLITE_OK, sqlite3_deserialize(Db2, "main", S)),
                Rv2 = raw_sqlite3:q(Db, "SELECT * FROM t"),
                ?assertMatch([{<<"hello">>}, {<<"world">>}], Rv2)
        end,
    {setup, Setup, Cleanup, T}.

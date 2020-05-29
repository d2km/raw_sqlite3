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

open_test_() ->
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

close_test_() ->
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
                            Title = io_lib:format("sqlite3_db_config(~p, ~p)", [Opt, Val]),
                            F = fun() ->
                                        Rv = sqlite3_db_config(Db, Opt, Val),
                                        ?assertEqual(ExpectedRv, Rv)
                                end,
                            {lists:flatten(Title), F}
                    end,
                lists:map(M, ConfOpts)
        end,
    {setup, Setup, Cleanup, I}.

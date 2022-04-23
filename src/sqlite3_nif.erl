%% @doc A low-level NIF interface to sqlite3.
%%
%% Most of the exposed functions follow the C API to the letter so <a
%% href="https://sqlite.org/c3ref/funclist.html"> the official documentation</a>
%% should be considered as
%% the primary source of information, while <a href="https://sqlite.org/cintro.html">
%% the C API introduction</a> should be read as a "Getting Started Guide".

-module(sqlite3_nif).

-export_type([sqlite3/0,
              sqlite3_stmt/0,
              sqlite3_backup/0,
              sqlite3_blob/0,
              sqlite3_str/0,
              sqlite3_snapshot/0,
              sqlite3_error_code/0]).

-export([sqlite3_open_v2/3,
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
         sqlite3_changes64/1,
         sqlite3_total_changes/1,
         sqlite3_total_changes64/1,
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
         sqlite3_error_offset/1,
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

-on_load({init, 0}).

-define(APPNAME, raw_sqlite3).
-define(LIBNAME, sqlite3_nif).

-opaque sqlite3() :: reference().
%% An opaque handle that represents a database connection object.

-opaque sqlite3_stmt() :: reference().
%% A opaque handle that represents a prepared statement.

-opaque sqlite3_blob() :: reference().
%% A opaque handle to use with Blobs API.

-opaque sqlite3_backup() :: reference().
%% A opaque handle to use with Backup API.

-opaque sqlite3_snapshot() :: reference().
%% A opaque handle to use with Snapshot API.

-type sqlite3_error_code() :: integer().
%% <code>sqlite3_error_code</code> is an integer which corresponds to one of the
%% constants specified at <a href="https://sqlite.org/rescode.html">Result and
%% Error Codes</a> page.

-type sqlite3_str() :: iodata().
%% <code>sqlite3_str()</code> is <code>iodata()</code> which contains a
%% UTF-8 encoded string.

-include_lib("raw_sqlite3/include/sqlite3_nif.hrl").

%%%%%%%%%%%%%%%%%%%
%% Connection API %
%%%%%%%%%%%%%%%%%%%

-type open_flag() :: ?SQLITE_OPEN_READONLY
                   | ?SQLITE_OPEN_READWRITE
                   | ?SQLITE_OPEN_CREATE
                   | ?SQLITE_OPEN_URI
                   | ?SQLITE_OPEN_MEMORY
                   | ?SQLITE_OPEN_FULLMUTEX
                   | ?SQLITE_OPEN_SHAREDCACHE
                   | ?SQLITE_OPEN_PRIVATECACHE
                   | ?SQLITE_OPEN_NOFOLLOW.

-type open_flags() :: open_flag() | integer().
%% <code>open_flags()</code> is <code>Flag1 bor Flag2 bor ...</code> where
%% <code>FlagN</code> is one of the <code>open_flag()</code> values. See <a
%% href="https://sqlite.org/c3ref/open.html">C function documentation</a> for
%% details.

-spec sqlite3_open_v2(FileName, Flags, Vfs) -> Result
              when FileName :: sqlite3_str(),
                   Flags    :: open_flags(),
                   Vfs      :: sqlite3_str(),
                   Result   :: {ok, sqlite3()} |
                               {error, sqlite3_error_code()}.
%% @doc Open a new database connection.
sqlite3_open_v2(_FileName, _Flags, _Vfs) ->
    not_loaded(?LINE).

-spec sqlite3_close_v2(sqlite3()) -> sqlite3_error_code().
%% @doc Close a database connection. Note, it is not mandatory to close an open
%% connection using the function since it is reference counted and will be
%% closed automatically. The only case where it may be required to close it is
%% when some action is expected to be performed by SQLite engine (e.g. WAL file
%% is removed when all connections to a DB file are closed).
sqlite3_close_v2(_Db) ->
    not_loaded(?LINE).

%% Connection mutation functions
-spec sqlite3_limit(sqlite3(), LimitId, NewVal) -> integer()
              when LimitId :: ?SQLITE_LIMIT_LENGTH |
                              ?SQLITE_LIMIT_SQL_LENGTH |
                              ?SQLITE_LIMIT_COLUMN |
                              ?SQLITE_LIMIT_EXPR_DEPTH |
                              ?SQLITE_LIMIT_COMPOUND_SELECT |
                              ?SQLITE_LIMIT_VDBE_OP |
                              ?SQLITE_LIMIT_FUNCTION_ARG |
                              ?SQLITE_LIMIT_ATTACHED |
                              ?SQLITE_LIMIT_LIKE_PATTERN_LENGTH |
                              ?SQLITE_LIMIT_VARIABLE_NUMBER |
                              ?SQLITE_LIMIT_TRIGGER_DEPTH |
                              ?SQLITE_LIMIT_WORKER_THREADS,
                   NewVal  :: integer().
%% @doc Change per-connection run-time limits.
sqlite3_limit(_Db, _LimitId, _NewVal) ->
    not_loaded(?LINE).

-spec sqlite3_load_extension(sqlite3(), ExtFile, EntryPoint) -> Result
              when ExtFile    :: sqlite3_str(),
                   EntryPoint :: sqlite3_str(),
                   Result     :: ok | {error, string()}.
%% @doc Load an extension.
sqlite3_load_extension(_Db, _ExtFile, _EntryPoint) ->
    not_loaded(?LINE).

-spec sqlite3_db_cacheflush(sqlite3()) -> sqlite3_error_code().
%% @doc Flush caches to disk mid-transaction.
sqlite3_db_cacheflush(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_db_release_memory(sqlite3()) -> sqlite3_error_code().
%% @doc Free memory used by a database connection.
sqlite3_db_release_memory(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_db_config(sqlite3(), Opt, Val) -> Result
              when Opt    :: ?SQLITE_DBCONFIG_MAINDBNAME |
                             ?SQLITE_DBCONFIG_ENABLE_FKEY |
                             ?SQLITE_DBCONFIG_ENABLE_TRIGGER |
                             ?SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER |
                             ?SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION |
                             ?SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE |
                             ?SQLITE_DBCONFIG_ENABLE_QPSG |
                             ?SQLITE_DBCONFIG_TRIGGER_EQP |
                             ?SQLITE_DBCONFIG_RESET_DATABASE |
                             ?SQLITE_DBCONFIG_DEFENSIVE |
                             ?SQLITE_DBCONFIG_WRITABLE_SCHEMA |
                             ?SQLITE_DBCONFIG_LEGACY_ALTER_TABLE |
                             ?SQLITE_DBCONFIG_DQS_DML |
                             ?SQLITE_DBCONFIG_DQS_DDL |
                             ?SQLITE_DBCONFIG_ENABLE_VIEW |
                             ?SQLITE_DBCONFIG_LEGACY_FILE_FORMAT |
                             ?SQLITE_DBCONFIG_TRUSTED_SCHEMA,
                   Val    :: integer() | sqlite3_str(),
                   Result :: sqlite3_error_code() |
                             {sqlite3_error_code(), integer()}.
%% @doc Configure a database connection.
sqlite3_db_config(_Db, _Opt, _Val) ->
    not_loaded(?LINE).

-spec sqlite3_set_last_insert_rowid(sqlite3(), integer()) -> ok.
%% doc Set the last insert rowid value.
sqlite3_set_last_insert_rowid(_Db, _NewRowid) ->
    not_loaded(?LINE).

-spec sqlite3_busy_timeout(sqlite3(), integer()) -> sqlite3_error_code().
%% @doc Set a busy timeout.
sqlite3_busy_timeout(_Db, _Duration) ->
    not_loaded(?LINE).

-spec sqlite3_extended_result_codes(sqlite3(), OnOff) -> sqlite3_error_code()
              when OnOff :: 0 | 1.
%% @doc Enable or disable extended result codes.
sqlite3_extended_result_codes(_Db, _OnOff) ->
    not_loaded(?LINE).

-spec sqlite3_interrupt(sqlite3()) -> ok.
%% @doc Interrupt a long-running query.
sqlite3_interrupt(_Db) ->
    not_loaded(?LINE).

%% Connection information functions
-spec sqlite3_get_autocommit(sqlite3()) -> integer().
%% @doc Test for auto-commit mode.
sqlite3_get_autocommit(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_last_insert_rowid(sqlite3()) -> integer().
%% @doc Get the last insert rowid value.
sqlite3_last_insert_rowid(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_changes(sqlite3()) -> integer().
%% @doc Count the number of modified rows by the last statement.
sqlite3_changes(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_changes64(sqlite3()) -> integer().
%% @doc Count the number of modified rows by the last statement.
sqlite3_changes64(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_total_changes(sqlite3()) -> integer().
%% @doc Count the total number of modified rows since the connection was opened.
sqlite3_total_changes(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_total_changes64(sqlite3()) -> integer().
%% @doc Count the total number of modified rows since the connection was opened.
sqlite3_total_changes64(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_db_filename(sqlite3(), DbName) -> DbFileName
              when DbName     :: sqlite3_str(),
                   DbFileName :: binary().
%% @doc Return the filename for a database connection.
sqlite3_db_filename(_Db, _DbName) ->
    not_loaded(?LINE).

-spec sqlite3_db_readonly(sqlite3(), DbName) -> Result
              when DbName :: sqlite3_str(),
                   Result :: -1 | 0 | 1.
%% @doc Test if the DB is opened in read-only mode.
sqlite3_db_readonly(_Db, _DbName) ->
    not_loaded(?LINE).

-spec sqlite3_db_status(sqlite3(), StatusOpt, ResetFlag) -> Result
              when StatusOpt :: ?SQLITE_DBSTATUS_LOOKASIDE_USED |
                                ?SQLITE_DBSTATUS_CACHE_USED |
                                ?SQLITE_DBSTATUS_SCHEMA_USED |
                                ?SQLITE_DBSTATUS_STMT_USED |
                                ?SQLITE_DBSTATUS_LOOKASIDE_HIT |
                                ?SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE |
                                ?SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL |
                                ?SQLITE_DBSTATUS_CACHE_HIT |
                                ?SQLITE_DBSTATUS_CACHE_MISS |
                                ?SQLITE_DBSTATUS_CACHE_WRITE |
                                ?SQLITE_DBSTATUS_DEFERRED_FKS |
                                ?SQLITE_DBSTATUS_CACHE_USED_SHARED |
                                ?SQLITE_DBSTATUS_CACHE_SPILL,
                   ResetFlag :: 0 | 1,
                   Result    :: {CurVal, HighVal} |
                                {error, sqlite3_error_code()},
                   CurVal    :: integer(),
                   HighVal   :: integer().
%% @doc Get database connection stats.
sqlite3_db_status(_Db, _StatusOpt, _ResetFlag) ->
    not_loaded(?LINE).

-spec sqlite3_txn_state(sqlite3(), DbName) -> Result
              when DbName :: sqlite3_str(),
                   Result :: -1 |
                             ?SQLITE_TXN_NONE |
                             ?SQLITE_TXN_READ |
                             ?SQLITE_TXN_WRITE.
%% @doc Determine the transaction state of a database.
sqlite3_txn_state(_Db, _DbName) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prepared statements API %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sqlite3_prepare_v2(sqlite3(), Sql) -> Result
              when Sql      :: sqlite3_str(),
                   Result   :: {ok, sqlite3_stmt(), Leftover} |
                               {error, sqlite3_error_code()},
                   Leftover :: binary().
%% @doc Compile an SQL statement.
sqlite3_prepare_v2(_Db, _Sql) ->
    not_loaded(?LINE).

-spec sqlite3_bind(sqlite3_stmt(), [Val]) -> Result
              when Val    :: number() | true | false | Null |
                             {text, sqlite3_str()} |
                             {blob, iodata()},
                   Null   :: nil | undefined,
                   Result :: ok | {error, Reason},
                   Reason :: sqlite3_error_code() |
                             {wrong_parameter_type, pos_integer()} |
                             wrong_parameter_count.
%% @doc Bind parameters to a compiled SQL statement.
sqlite3_bind(_Stmt, _Terms) ->
    not_loaded(?LINE).

-spec sqlite3_step(sqlite3_stmt()) -> Result
              when Result :: ok | Row |
                             {error, sqlite3_error_code()},
                   Row    :: tuple().
%% @doc Evaluate a compiled SQL statement.
sqlite3_step(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_reset(sqlite3_stmt()) -> sqlite3_error_code().
%% @doc Reset a prepared statement object.
sqlite3_reset(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_clear_bindings(sqlite3_stmt()) -> sqlite3_error_code().
%% @doc Reset all bindings on a prepared statement.
sqlite3_clear_bindings(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_finalize(sqlite3_stmt()) -> sqlite3_error_code().
%% @doc Free resources allocated for a prepared statement. Note, normally there
%% is no need to call the function because the statement is reference-counted
%% and will be de-allocated automatically.
sqlite3_finalize(_Stmt) ->
    not_loaded(?LINE).

%% Query information about prepared statements
-spec sqlite3_data_count(sqlite3_stmt()) -> integer().
%% @doc Number of columns in a result set.
sqlite3_data_count(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_sql(sqlite3_stmt()) -> binary().
%% @doc Return SQL code used for the compiled statement.
sqlite3_sql(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_expanded_sql(sqlite3_stmt()) -> binary().
%% @doc Return SQL code associated with the compiled statement with the
%% parameter list expanded.
sqlite3_expanded_sql(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_normalized_sql(sqlite3_stmt()) -> binary().
%% @doc Return normalised SQL code associated with the compiled statement,
%% whatever it means.
sqlite3_normalized_sql(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_stmt_busy(sqlite3_stmt()) -> integer().
%% @doc Determine if a prepared statement has been reset.
sqlite3_stmt_busy(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_stmt_isexplain(sqlite3_stmt()) -> integer().
%% @doc Query the EXPLAIN setting for a prepared statement.
sqlite3_stmt_isexplain(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_stmt_readonly(sqlite3_stmt()) -> integer().
%% @doc Determine if an sql statement writes the database.
sqlite3_stmt_readonly(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_stmt_status(sqlite3_stmt(), StmtStatus, ResetFlag) -> Result
              when StmtStatus :: ?SQLITE_STMTSTATUS_FULLSCAN_STEP |
                                 ?SQLITE_STMTSTATUS_SORT |
                                 ?SQLITE_STMTSTATUS_AUTOINDEX |
                                 ?SQLITE_STMTSTATUS_VM_STEP |
                                 ?SQLITE_STMTSTATUS_REPREPARE |
                                 ?SQLITE_STMTSTATUS_RUN |
                                 ?SQLITE_STMTSTATUS_MEMUSED,
                   ResetFlag  :: 0 | 1,
                   Result     :: integer().
%% @doc Prepared statement status.
sqlite3_stmt_status(_Stmt, _StmtStatus, _ResetFlag) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%
%% Backup API %
%%%%%%%%%%%%%%%

-spec sqlite3_backup_init(DstConn, DstDbName, SrcConn, SrcDbName) -> Result
              when DstConn   :: sqlite3(),
                   DstDbName :: sqlite3_str(),
                   SrcConn   :: sqlite3(),
                   SrcDbName :: sqlite3_str(),
                   Result    :: {ok, Backup} | {error, ErrorCode},
                   Backup    :: sqlite3_backup(),
                   ErrorCode :: sqlite3_error_code().
%% @doc Initialise Backup object.
sqlite3_backup_init(_DstConn, _DstDbName, _SrcConn, _SrcDbName) ->
    not_loaded(?LINE).

-spec sqlite3_backup_step(sqlite3_backup(), NumPages) -> Result
              when NumPages :: integer(),
                   Result   :: sqlite3_error_code().
%% @doc Copy the specified number of pages or all pages if
%% <code>NumPages</code> is set to -1.
sqlite3_backup_step(_Backup, _NumPages) ->
    not_loaded(?LINE).

-spec sqlite3_backup_finish(Backup) -> Result
              when Backup :: sqlite3_backup(),
                   Result :: sqlite3_error_code().
%% @doc Release resources associated with the backup. Note, there is no need to
%% call the function because the Backup object is reference-counted and will be
%% released automatically.
sqlite3_backup_finish(_Backup) ->
    not_loaded(?LINE).

-spec sqlite3_backup_pagecount(Backup) -> Result
              when Backup :: sqlite3_backup(),
                   Result :: integer().
%% @doc Get the number of pages to be copied for the database.
sqlite3_backup_pagecount(_Backup) ->
    not_loaded(?LINE).

-spec sqlite3_backup_remaining(Backup) -> Result
              when Backup :: sqlite3_backup(),
                   Result :: integer().
%% @doc Get the remaining number of pages to copy.
sqlite3_backup_remaining(_Backup) ->
    not_loaded(?LINE).


%%%%%%%%%%%%%
%% BLOB API %
%%%%%%%%%%%%%
-spec sqlite3_blob_open(sqlite3(), DbName, TabName, ColumnName, RowId, Flags) -> Result
              when DbName     :: sqlite3_str(),
                   TabName    :: sqlite3_str(),
                   ColumnName :: sqlite3_str(),
                   RowId      :: integer(),
                   Flags      :: integer(),
                   Result     :: {ok, sqlite3_blob()} | {error, sqlite3_error_code()}.
%% @doc Open a BLOB for incremental I/O.
sqlite3_blob_open(_Db, _DbName, _TabName, _ColumnName, _Rowid, _Flags) ->
    not_loaded(?LINE).

-spec sqlite3_blob_bytes(sqlite3_blob()) -> integer().
%% @doc Size of the BLOB.
sqlite3_blob_bytes(_Blob) ->
    not_loaded(?LINE).

-spec sqlite3_blob_close(sqlite3_blob()) -> sqlite3_error_code().
%% @doc Close a BLOB handle. Note, normally, there is no need to call the
%% function since the object is reference-counted and will be released
%% automatically.
sqlite3_blob_close(_Blob) ->
    not_loaded(?LINE).

-spec sqlite3_blob_read(sqlite3_blob(), N, Offset) -> Result
              when N      :: non_neg_integer(),
                   Offset :: non_neg_integer(),
                   Result :: {ok, binary()} | {error, sqlite3_error_code()}.
%% @doc Read data from a BLOB incrementally.
sqlite3_blob_read(_Blob, _N, _Offset) ->
    not_loaded(?LINE).

-spec sqlite3_blob_reopen(sqlite3_blob(), Rowid) -> sqlite3_error_code()
              when Rowid :: integer().
%% @doc Move a BLOB handle to a new row.
sqlite3_blob_reopen(_Blob, _Rowid) ->
    not_loaded(?LINE).

-spec sqlite3_blob_write(Blob, Data, Offset) -> Result
              when Blob   :: sqlite3_blob(),
                   Data   :: iodata(),
                   Offset :: non_neg_integer(),
                   Result :: ok | {error, sqlite3_error_code()}.
%% @doc Write data into a BLOB incrementally.
sqlite3_blob_write(_Blob, _Data, _Offset) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%%%%%%%%%
%% Error handling API %
%%%%%%%%%%%%%%%%%%%%%%%
-spec sqlite3_errcode(sqlite3()) -> sqlite3_error_code().
%% @doc Get the latest error code associated with the connection.
sqlite3_errcode(_Db) ->
    not_loaded(?LINE).


-spec sqlite3_extended_errcode(sqlite3()) -> sqlite3_error_code().
%% @doc Get the latest extended error code associated with the connection.
sqlite3_extended_errcode(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_error_offset(sqlite3()) -> integer().
%% @doc Get byte offset of the latest parse error or -1.
sqlite3_error_offset(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_errmsg(sqlite3()) -> binary().
%% @doc Get the human-readable error message associated with the connection.
sqlite3_errmsg(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_errstr(sqlite3_error_code()) -> binary().
%% @doc Convert error code to a human-readable message.
sqlite3_errstr(_ErrorCode) ->
    not_loaded(?LINE).

%% WAL-related functions
-spec sqlite3_wal_autocheckpoint(sqlite3(), NFrames) -> sqlite3_error_code()
              when NFrames :: integer().
%% @doc Configure an auto-checkpoint.
sqlite3_wal_autocheckpoint(_Db, _N) ->
    not_loaded(?LINE).

-spec sqlite3_wal_checkpoint_v2(sqlite3(), DbName, Mode) -> Result
          when DbName      :: sqlite3_str(),
               Mode        :: ?SQLITE_CHECKPOINT_PASSIVE |
                              ?SQLITE_CHECKPOINT_FULL |
                              ?SQLITE_CHECKPOINT_RESTART |
                              ?SQLITE_CHECKPOINT_TRUNCATE,
               Result      :: {ok, {LogSize, TotalFrames}}
                            | {error, sqlite3_error_code()},
               LogSize     :: integer(),
               TotalFrames :: integer().
%% @doc Checkpoint a database.
sqlite3_wal_checkpoint_v2(_Db, _DbName, _Mode) ->
    not_loaded(?LINE).

%% Global informational/utility functions
-spec sqlite3_compileoption_get(N :: integer()) -> binary() | nil.
%% @doc Get a compile option string number <code>N</code>. Can be used to
%% receive the total number of compile opitons.
sqlite3_compileoption_get(_N) ->
    not_loaded(?LINE).

-spec sqlite3_compileoption_used(OptName) -> integer()
              when OptName :: sqlite3_str().
%% @doc Ask if a specified compile option was set.
sqlite3_compileoption_used(_OptName) ->
    not_loaded(?LINE).

-spec sqlite3_libversion() -> binary().
%% @doc Library version.
sqlite3_libversion() ->
    not_loaded(?LINE).

-spec sqlite3_libversion_number() -> integer().
%% @doc Library version as number.
sqlite3_libversion_number() ->
    not_loaded(?LINE).

-spec sqlite3_sourceid() -> binary().
%% @doc Timestamp and hash of the source code used to build the sqlite3
%% library.
sqlite3_sourceid() ->
    not_loaded(?LINE).

-spec sqlite3_complete(Sql :: sqlite3_str()) -> integer().
%% @doc Determine if an SQL statement is complete.
sqlite3_complete(_Sql) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%%%
%% Snapshot API %
%%%%%%%%%%%%%%%%%

-spec sqlite3_snapshot_get(sqlite3(), DbName) -> Result
              when DbName :: sqlite3_str(),
                   Result :: {ok, sqlite3_snapshot()} |
                             {error, sqlite3_error_code()}.
%% @doc Record a database snapshot.
sqlite3_snapshot_get(_Db, _DbName) ->
    not_loaded(?LINE).

-spec sqlite3_snapshot_free(sqlite3_snapshot()) -> ok.
%% @doc Destroy a snapshot. Note, normally, there is no need to call the
%% function since the snapshot object is reference-counted and destroyed
%% automatically.
sqlite3_snapshot_free(_Snapshot) ->
    not_loaded(?LINE).

-spec sqlite3_snapshot_open(
        sqlite3(),
        DbName :: sqlite3_str(),
        sqlite3_snapshot()
       ) -> sqlite3_error_code().
%% @doc Start a read transaction on an historical snapshot.
sqlite3_snapshot_open(_Db, _DbName, _Snapshot) ->
    not_loaded(?LINE).

-spec sqlite3_snapshot_cmp(Snapshot1, Snapshot2) -> Result
              when Snapshot1 :: sqlite3_snapshot(),
                   Snapshot2 :: sqlite3_snapshot(),
                   Result    :: integer().
%% @doc Compare which snapshot coresspond to an earlier database state.
sqlite3_snapshot_cmp(_Snapshot1, _Snapshot2) ->
    not_loaded(?LINE).

-spec sqlite3_snapshot_recover(sqlite3(), DbName) -> Result
              when DbName :: sqlite3_str(),
                   Result :: sqlite3_error_code().
%% @doc Recover snapshots from a wal file.
sqlite3_snapshot_recover(_Db, _DbName) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%%%%
%% Serialize API %
%%%%%%%%%%%%%%%%%%

-spec sqlite3_serialize(sqlite3(), DbName) -> Result
              when DbName :: sqlite3_str(),
                   Result :: {ok, Serialization :: binary()} |
                             {error, alloc_memory}.
%% @doc Serialise a database into a binary.
sqlite3_serialize(_Db, _DbName) ->
    not_loaded(?LINE).

-spec sqlite3_deserialize(sqlite3(), DbName, Serialization) -> Result
              when DbName :: sqlite3_str(),
                   Serialization :: binary(),
                   Result :: sqlite3_error_code().
%% @doc Replace a DB schema with the contents of the serialized database.
sqlite3_deserialize(_Db, _DbName, _Seralization) ->
    not_loaded(?LINE).

%% NIF initalization
init() ->
    SoName = case code:priv_dir(?APPNAME) of
               {error, bad_name} ->
                   case filelib:is_dir(filename:join(["..", priv])) of
                     true ->
                         filename:join(["..", priv, ?LIBNAME]);
                     _ ->
                         filename:join([priv, ?LIBNAME])
                   end;
               Dir ->
                   filename:join(Dir, ?LIBNAME)
             end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

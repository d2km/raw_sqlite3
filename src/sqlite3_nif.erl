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

-on_load({init, 0}).

-define(APPNAME, raw_sqlite3).
-define(LIBNAME, sqlite3_nif).

-opaque sqlite3() :: reference().
-opaque sqlite3_stmt() :: reference().
-opaque sqlite3_blob() :: reference().
-opaque sqlite3_backup() :: reference().
-opaque sqlite3_snapshot() :: reference().

-type sqlite3_error_code() :: integer().
-type sqlite3_str() :: iodata().

%%%%%%%%%%%%%%%%%%%
%% Connection API %
%%%%%%%%%%%%%%%%%%%
-spec sqlite3_open_v2(FileName, Flags, Vfs) -> Result
              when FileName :: sqlite3_str(),
                   Flags    :: integer(),
                   Vfs      :: sqlite3_str(),
                   Result   :: {ok, sqlite3()} |
                               {error, sqlite3_error_code()}.
sqlite3_open_v2(_FileName, _Flags, _Vfs) ->
    not_loaded(?LINE).

-spec sqlite3_close_v2(sqlite3()) -> sqlite3_error_code().
sqlite3_close_v2(_Db) ->
    not_loaded(?LINE).

%% Connection mutation functions
-spec sqlite3_limit(sqlite3(), LimitId, NewVal) -> integer()
              when LimitId :: integer(),
                   NewVal  :: integer().
sqlite3_limit(_Db, _LimitId, _NewVal) ->
    not_loaded(?LINE).

-spec sqlite3_load_extension(sqlite3(), ExtFile, EntryPoint) -> Result
              when ExtFile    :: sqlite3_str(),
                   EntryPoint :: sqlite3_str(),
                   Result     :: ok | {error, string()}.
sqlite3_load_extension(_Db, _ExtFile, _EntryPoint) ->
    not_loaded(?LINE).

-spec sqlite3_db_cacheflush(sqlite3()) -> sqlite3_error_code().
sqlite3_db_cacheflush(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_db_release_memory(sqlite3()) -> sqlite3_error_code().
sqlite3_db_release_memory(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_db_config(sqlite3(), Opt, Val) -> Result
              when Opt    :: integer(),
                   Val    :: integer() | sqlite3_str(),
                   Result :: sqlite3_error_code() |
                             {sqlite3_error_code(), integer()}.
sqlite3_db_config(_Db, _Opt, _Val) ->
    not_loaded(?LINE).

-spec sqlite3_set_last_insert_rowid(sqlite3(), integer()) -> ok.
sqlite3_set_last_insert_rowid(_Db, _NewRowid) ->
    not_loaded(?LINE).

-spec sqlite3_busy_timeout(sqlite3(), integer()) -> sqlite3_error_code().
sqlite3_busy_timeout(_Db, _Duration) ->
    not_loaded(?LINE).

-spec sqlite3_extended_result_codes(sqlite3(), integer()) -> sqlite3_error_code().
sqlite3_extended_result_codes(_Db, _OnOff) ->
    not_loaded(?LINE).

-spec sqlite3_interrupt(sqlite3()) -> ok.
sqlite3_interrupt(_Db) ->
    not_loaded(?LINE).

%% Connection information functions
-spec sqlite3_get_autocommit(sqlite3()) -> integer().
sqlite3_get_autocommit(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_last_insert_rowid(sqlite3()) -> integer().
sqlite3_last_insert_rowid(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_changes(sqlite3()) -> integer().
sqlite3_changes(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_total_changes(sqlite3()) -> integer().
sqlite3_total_changes(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_db_filename(sqlite3(), DbName) -> DbFileName
              when DbName     :: sqlite3_str(),
                   DbFileName :: binary().
sqlite3_db_filename(_Db, _DbName) ->
    not_loaded(?LINE).

-spec sqlite3_db_readonly(sqlite3(), DbName) -> Result
              when DbName :: sqlite3_str(),
                   Result :: -1 | 0 | 1.
sqlite3_db_readonly(_Db, _DbName) ->
    not_loaded(?LINE).

-spec sqlite3_db_status(sqlite3(), StatusOpt, ResetFlag) -> Result
              when StatusOpt :: integer(),
                   ResetFlag :: integer(),
                   Result    :: {CurVal, HighVal} |
                                {error, sqlite3_error_code()},
                   CurVal    :: integer(),
                   HighVal   :: integer().
sqlite3_db_status(_Db, _StatusOpt, _ResetFlag) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prepared statements API %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sqlite3_prepare_v2(sqlite3(), Sql) -> Result
              when Sql      :: sqlite3_str(),
                   Result   :: {ok, sqlite3_stmt(), Leftover} |
                               {error, sqlite3_error_code()},
                   Leftover :: binary().
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
sqlite3_bind(_Stmt, _Terms) ->
    not_loaded(?LINE).

-spec sqlite3_step(sqlite3_stmt()) -> Result
              when Result :: ok | Row |
                             {error, sqlite3_error_code()},
                   Row    :: tuple().
sqlite3_step(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_reset(sqlite3_stmt()) -> sqlite3_error_code().
sqlite3_reset(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_clear_bindings(sqlite3_stmt()) -> sqlite3_error_code().
sqlite3_clear_bindings(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_finalize(sqlite3_stmt()) -> sqlite3_error_code().
sqlite3_finalize(_Stmt) ->
    not_loaded(?LINE).

%% Query information about prepared statements
-spec sqlite3_data_count(sqlite3_stmt()) -> integer().
sqlite3_data_count(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_sql(sqlite3_stmt()) -> binary().
sqlite3_sql(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_expanded_sql(sqlite3_stmt()) -> binary().
sqlite3_expanded_sql(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_normalized_sql(sqlite3_stmt()) -> binary().
sqlite3_normalized_sql(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_stmt_busy(sqlite3_stmt()) -> integer().
sqlite3_stmt_busy(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_stmt_isexplain(sqlite3_stmt()) -> integer().
sqlite3_stmt_isexplain(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_stmt_readonly(sqlite3_stmt()) -> integer().
sqlite3_stmt_readonly(_Stmt) ->
    not_loaded(?LINE).

-spec sqlite3_stmt_status(sqlite3_stmt(), StmtStatus, ResetFlag) -> Result
              when StmtStatus :: integer(),
                   ResetFlag  :: integer(),
                   Result     :: integer().
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
sqlite3_backup_init(_DstConn, _DstDbName, _SrcConn, _SrcDbName) ->
    not_loaded(?LINE).

-spec sqlite3_backup_step(sqlite3_backup(), NumPages) -> Result
              when NumPages :: integer(),
                   Result   :: sqlite3_error_code().
sqlite3_backup_step(_Backup, _NumPages) ->
    not_loaded(?LINE).

-spec sqlite3_backup_finish(Backup) -> Result
              when Backup :: sqlite3_backup(),
                   Result :: sqlite3_error_code().
sqlite3_backup_finish(_Backup) ->
    not_loaded(?LINE).

-spec sqlite3_backup_pagecount(Backup) -> Result
              when Backup :: sqlite3_backup(),
                   Result :: integer().
sqlite3_backup_pagecount(_Backup) ->
    not_loaded(?LINE).

-spec sqlite3_backup_remaining(Backup) -> Result
              when Backup :: sqlite3_backup(),
                   Result :: integer().
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
sqlite3_blob_open(_Db, _DbName, _TabName, _ColumnName, _Rowid, _Flags) ->
    not_loaded(?LINE).

sqlite3_blob_bytes(_Blob) ->
    not_loaded(?LINE).

-spec sqlite3_blob_close(sqlite3_blob()) -> sqlite3_error_code().
sqlite3_blob_close(_Blob) ->
    not_loaded(?LINE).

-spec sqlite3_blob_read(sqlite3_blob(), N, Offset) -> Result
              when N      :: non_neg_integer(),
                   Offset :: non_neg_integer(),
                   Result :: {ok, binary()} | {error, sqlite3_error_code()}.
sqlite3_blob_read(_Blob, _N, _Offset) ->
    not_loaded(?LINE).

-spec sqlite3_blob_reopen(sqlite3_blob(), Rowid) -> sqlite3_error_code()
              when Rowid :: integer().
sqlite3_blob_reopen(_Blob, _Rowid) ->
    not_loaded(?LINE).

-spec sqlite3_blob_write(Blob, Data, Offset) -> Result
              when Blob   :: sqlite3_blob(),
                   Data   :: iodata(),
                   Offset :: non_neg_integer(),
                   Result :: ok | {error, sqlite3_error_code()}.
sqlite3_blob_write(_Blob, _Data, _Offset) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%%%%%%%%%
%% Error handling API %
%%%%%%%%%%%%%%%%%%%%%%%
-spec sqlite3_errcode(sqlite3()) -> sqlite3_error_code().
sqlite3_errcode(_Db) ->
    not_loaded(?LINE).


-spec sqlite3_extended_errcode(sqlite3()) -> sqlite3_error_code().
sqlite3_extended_errcode(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_errmsg(sqlite3()) -> binary().
sqlite3_errmsg(_Db) ->
    not_loaded(?LINE).

-spec sqlite3_errstr(sqlite3_error_code()) -> binary().
sqlite3_errstr(_ErrorCode) ->
    not_loaded(?LINE).

%% WAL-related functions
-spec sqlite3_wal_autocheckpoint(sqlite3(), NFrames) -> sqlite3_error_code()
              when NFrames :: integer().
sqlite3_wal_autocheckpoint(_Db, _N) ->
    not_loaded(?LINE).

-spec sqlite3_wal_checkpoint_v2(sqlite3(), DbName, Mode) -> Result
          when DbName      :: sqlite3_str(),
               Mode        :: integer(),
               Result      :: {ok, {LogSize, TotalFrames}}
                            | {error, sqlite3_error_code()},
               LogSize     :: integer(),
               TotalFrames :: integer().
sqlite3_wal_checkpoint_v2(_Db, _DbName, _Mode) ->
    not_loaded(?LINE).

%% Global informational/utility functions
-spec sqlite3_compileoption_get(N :: integer()) -> binary() | nil.
sqlite3_compileoption_get(_N) ->
    not_loaded(?LINE).

-spec sqlite3_compileoption_used(OptName) -> integer()
              when OptName :: sqlite3_str().
sqlite3_compileoption_used(_OptName) ->
    not_loaded(?LINE).

-spec sqlite3_libversion() -> binary().
sqlite3_libversion() ->
    not_loaded(?LINE).

-spec sqlite3_libversion_number() -> integer().
sqlite3_libversion_number() ->
    not_loaded(?LINE).

-spec sqlite3_sourceid() -> binary().
sqlite3_sourceid() ->
    not_loaded(?LINE).

-spec sqlite3_complete(Sql :: sqlite3_str()) -> integer().
sqlite3_complete(_Sql) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%%%
%% Snapshot API %
%%%%%%%%%%%%%%%%%

-spec sqlite3_snapshot_get(sqlite3(), DbName) -> Result
              when DbName :: sqlite3_str(),
                   Result :: {ok, sqlite3_snapshot()} |
                             {error, sqlite3_error_code()}.
sqlite3_snapshot_get(_Db, _DbName) ->
    not_loaded(?LINE).

-spec sqlite3_snapshot_free(sqlite3_snapshot()) -> ok.
sqlite3_snapshot_free(_Snapshot) ->
    not_loaded(?LINE).

-spec sqlite3_snapshot_open(
        sqlite3(),
        DbName :: sqlite3_str(),
        sqlite3_snapshot()
       ) -> sqlite3_error_code().
sqlite3_snapshot_open(_Db, _DbName, _Snapshot) ->
    not_loaded(?LINE).

-spec sqlite3_snapshot_cmp(Snapshot1, Snapshot2) -> Result
              when Snapshot1 :: sqlite3_snapshot(),
                   Snapshot2 :: sqlite3_snapshot(),
                   Result    :: integer().
sqlite3_snapshot_cmp(_Snapshot1, _Snapshot2) ->
    not_loaded(?LINE).

-spec sqlite3_snapshot_recover(sqlite3(), DbName) -> Result
              when DbName :: sqlite3_str(),
                   Result :: sqlite3_error_code().
sqlite3_snapshot_recover(_Db, _DbName) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%%%%
%% Serialize API %
%%%%%%%%%%%%%%%%%%

-spec sqlite3_serialize(sqlite3(), DbName) -> Result
              when DbName :: sqlite3_str(),
                   Result :: {ok, Serialization :: binary()} |
                             {error, alloc_memory}.
sqlite3_serialize(_Db, _DbName) ->
    not_loaded(?LINE).

-spec sqlite3_deserialize(sqlite3(), DbName, Serialization) -> Result
              when DbName :: sqlite3_str(),
                   Serialization :: binary(),
                   Result :: sqlite3_error_code().
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

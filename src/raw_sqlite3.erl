-module(raw_sqlite3).

-include_lib("raw_sqlite3/include/sqlite3_nif.hrl").

-export([open/1, open/2, open/3,
         close/1,
         prepare/2,
         bind/2,
         step/1,
         fetchall/1,
         q/2, q/3,
         exec/2, exec/3,
         fold/4, fold/5,
         map/3, map/4,
         make_flags/1,
         atom_to_int_flag/1,
         int_to_bool/1,
         bool_to_int/1,
         err_to_atom/1,
         expand_error/1, expand_error/2]).

-export_type([error_code/0,
             extended_error_code/0]).

-type error_code() ::
        'SQLITE_OK'         |
        'SQLITE_ERROR'      |
        'SQLITE_INTERNAL'   |
        'SQLITE_PERM'       |
        'SQLITE_ABORT'      |
        'SQLITE_BUSY'       |
        'SQLITE_LOCKED'     |
        'SQLITE_NOMEM'      |
        'SQLITE_READONLY'   |
        'SQLITE_INTERRUPT'  |
        'SQLITE_IOERR'      |
        'SQLITE_CORRUPT'    |
        'SQLITE_NOTFOUND'   |
        'SQLITE_FULL'       |
        'SQLITE_CANTOPEN'   |
        'SQLITE_PROTOCOL'   |
        'SQLITE_EMPTY'      |
        'SQLITE_SCHEMA'     |
        'SQLITE_TOOBIG'     |
        'SQLITE_CONSTRAINT' |
        'SQLITE_MISMATCH'   |
        'SQLITE_MISUSE'     |
        'SQLITE_NOLFS'      |
        'SQLITE_AUTH'       |
        'SQLITE_FORMAT'     |
        'SQLITE_RANGE'      |
        'SQLITE_NOTADB'     |
        'SQLITE_NOTICE'     |
        'SQLITE_WARNING'    |
        'SQLITE_ROW'        |
        'SQLITE_DONE'.


-type extended_error_code() ::
        'SQLITE_ERROR_MISSING_COLLSEQ'          |
        'SQLITE_ERROR_RETRY'                    |
        'SQLITE_ERROR_SNAPSHOT'                 |
        'SQLITE_IOERR_READ'                     |
        'SQLITE_IOERR_SHORT_READ'               |
        'SQLITE_IOERR_WRITE'                    |
        'SQLITE_IOERR_FSYNC'                    |
        'SQLITE_IOERR_DIR_FSYNC'                |
        'SQLITE_IOERR_TRUNCATE'                 |
        'SQLITE_IOERR_FSTAT'                    |
        'SQLITE_IOERR_UNLOCK'                   |
        'SQLITE_IOERR_RDLOCK'                   |
        'SQLITE_IOERR_DELETE'                   |
        'SQLITE_IOERR_BLOCKED'                  |
        'SQLITE_IOERR_NOMEM'                    |
        'SQLITE_IOERR_ACCESS'                   |
        'SQLITE_IOERR_CHECKRESERVEDLOCK'        |
        'SQLITE_IOERR_LOCK'                     |
        'SQLITE_IOERR_CLOSE'                    |
        'SQLITE_IOERR_DIR_CLOSE'                |
        'SQLITE_IOERR_SHMOPEN'                  |
        'SQLITE_IOERR_SHMSIZE'                  |
        'SQLITE_IOERR_SHMLOCK'                  |
        'SQLITE_IOERR_SHMMAP'                   |
        'SQLITE_IOERR_SEEK'                     |
        'SQLITE_IOERR_DELETE_NOENT'             |
        'SQLITE_IOERR_MMAP'                     |
        'SQLITE_IOERR_GETTEMPPATH'              |
        'SQLITE_IOERR_CONVPATH'                 |
        'SQLITE_IOERR_VNODE'                    |
        'SQLITE_IOERR_AUTH'                     |
        'SQLITE_IOERR_BEGIN_ATOMIC'             |
        'SQLITE_IOERR_COMMIT_ATOMIC'            |
        'SQLITE_IOERR_ROLLBACK_ATOMIC'          |
        'SQLITE_IOERR_DATA'                     |
        'SQLITE_LOCKED_SHAREDCACHE'             |
        'SQLITE_LOCKED_VTAB'                    |
        'SQLITE_BUSY_RECOVERY'                  |
        'SQLITE_BUSY_SNAPSHOT'                  |
        'SQLITE_BUSY_TIMEOUT'                   |
        'SQLITE_CANTOPEN_NOTEMPDIR'             |
        'SQLITE_CANTOPEN_ISDIR'                 |
        'SQLITE_CANTOPEN_FULLPATH'              |
        'SQLITE_CANTOPEN_CONVPATH'              |
        'SQLITE_CANTOPEN_DIRTYWAL'              |
        'SQLITE_CANTOPEN_SYMLINK'               |
        'SQLITE_CORRUPT_VTAB'                   |
        'SQLITE_CORRUPT_SEQUENCE'               |
        'SQLITE_CORRUPT_INDEX'                  |
        'SQLITE_READONLY_RECOVERY'              |
        'SQLITE_READONLY_CANTLOCK'              |
        'SQLITE_READONLY_ROLLBACK'              |
        'SQLITE_READONLY_DBMOVED'               |
        'SQLITE_READONLY_CANTINIT'              |
        'SQLITE_READONLY_DIRECTORY'             |
        'SQLITE_ABORT_ROLLBACK'                 |
        'SQLITE_CONSTRAINT_CHECK'               |
        'SQLITE_CONSTRAINT_COMMITHOOK'          |
        'SQLITE_CONSTRAINT_FOREIGNKEY'          |
        'SQLITE_CONSTRAINT_FUNCTION'            |
        'SQLITE_CONSTRAINT_NOTNULL'             |
        'SQLITE_CONSTRAINT_PRIMARYKEY'          |
        'SQLITE_CONSTRAINT_TRIGGER'             |
        'SQLITE_CONSTRAINT_UNIQUE'              |
        'SQLITE_CONSTRAINT_VTAB'                |
        'SQLITE_CONSTRAINT_ROWID'               |
        'SQLITE_CONSTRAINT_PINNED'              |
        'SQLITE_NOTICE_RECOVER_WAL'             |
        'SQLITE_NOTICE_RECOVER_ROLLBACK'        |
        'SQLITE_WARNING_AUTOINDEX'              |
        'SQLITE_AUTH_USER'                      |
        'SQLITE_OK_LOAD_PERMANENTLY'            |
        'SQLITE_OK_SYMLINK'                     |
        'SQLITE_OPEN_READONLY'                  |
        'SQLITE_OPEN_READWRITE'                 |
        'SQLITE_OPEN_CREATE'                    |
        'SQLITE_OPEN_URI'                       |
        'SQLITE_OPEN_MEMORY'                    |
        'SQLITE_OPEN_NOMUTEX'                   |
        'SQLITE_OPEN_FULLMUTEX'                 |
        'SQLITE_OPEN_SHAREDCACHE'               |
        'SQLITE_OPEN_PRIVATECACHE'              |
        'SQLITE_OPEN_NOFOLLOW'                  |
        'SQLITE_DBCONFIG_MAINDBNAME'            |
        'SQLITE_DBCONFIG_ENABLE_FKEY'           |
        'SQLITE_DBCONFIG_ENABLE_TRIGGER'        |
        'SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER' |
        'SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION' |
        'SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE'      |
        'SQLITE_DBCONFIG_ENABLE_QPSG'           |
        'SQLITE_DBCONFIG_TRIGGER_EQP'           |
        'SQLITE_DBCONFIG_RESET_DATABASE'        |
        'SQLITE_DBCONFIG_DEFENSIVE'             |
        'SQLITE_DBCONFIG_WRITABLE_SCHEMA'       |
        'SQLITE_DBCONFIG_LEGACY_ALTER_TABLE'    |
        'SQLITE_DBCONFIG_DQS_DML'               |
        'SQLITE_DBCONFIG_DQS_DDL'               |
        'SQLITE_DBCONFIG_ENABLE_VIEW'           |
        'SQLITE_DBCONFIG_LEGACY_FILE_FORMAT'    |
        'SQLITE_DBCONFIG_TRUSTED_SCHEMA'.

open(DbFile) ->
    open(DbFile, ?SQLITE_OPEN_READWRITE bor ?SQLITE_OPEN_CREATE).

open(DbFile, Flags) ->
    open(DbFile, Flags, "").

open(DbFile, Flags, Vfs) ->
    expand_error(sqlite3_nif:sqlite3_open_v2(DbFile, Flags, Vfs)).

close(Db) ->
    expand_error(sqlite3_nif:sqlite3_close_v2(Db)).

prepare(Db, Sql) ->
    expand_error(Db, sqlite3_nif:sqlite3_prepare_v2(Db, Sql)).


bind(Stmt, Params) ->
    expand_error(sqlite3_nif:sqlite3_bind(Stmt, Params)).

step(Stmt) ->
    expand_error(sqlite3_nif:sqlite3_step(Stmt)).

fetchall(Stmt) ->
    expand_error(fetchall(Stmt, [])).

fetchall(Stmt, Acc) ->
    case sqlite3_nif:sqlite3_step(Stmt) of
        ok ->
            lists:reverse(Acc);
        {error, _} = Err ->
            Err;
        Row ->
            fetchall(Stmt, [Row | Acc])
    end.

q(Db, Sql) ->
    q(Db, Sql, []).

q(Db, Sql, Params) ->
    case sqlite3_nif:sqlite3_prepare_v2(Db, Sql) of
        {ok, Stmt, _} ->
            case sqlite3_nif:sqlite3_bind(Stmt, Params) of
                ok ->
                    fetchall(Stmt);
                {error, _} = Err ->
                    expand_error(Db, Err)
            end;
        {error, _} = Err ->
            expand_error(Db, Err)
    end.

exec(Db, Sql) ->
    exec(Db, Sql, []).

exec(_Db, <<>>, _Params) ->
    ok;
exec(Db, Sql, Params) ->
    case sqlite3_nif:sqlite3_prepare_v2(Db, Sql) of
        {ok, Stmt, Leftover} ->
            case exec_one(Stmt, Params) of
                ok ->
                    exec(Db, Leftover, Params);
                {error, _} = Err ->
                    expand_error(Db, Err)
            end;
        {error, _} = Err ->
            expand_error(Db, Err)
    end.

exec_one(Stmt, Params) ->
    case sqlite3_nif:sqlite3_bind(Stmt, Params) of
        ok ->
            case fetchall(Stmt, []) of
                {error, _} = Err ->
                    Err;
                _ ->
                    ok
            end;
        {error, _} = Err ->
            Err
    end.

fold(Db, Sql, Fun, Acc) ->
    fold(Db, Sql, [], Fun, Acc).

fold(Db, Sql, Params, Fun, Acc) ->
    case sqlite3_nif:sqlite3_prepare_v2(Db, Sql) of
        {ok, Stmt, _Leftover} ->
            case sqlite3_nif:sqlite3_bind(Stmt, Params) of
                ok ->
                    expand_error(Db, do_fold(Stmt, Fun, Acc));
                {error, _} = Err ->
                    expand_error(Db, Err)
            end;
        {error, _} = Err ->
            expand_error(Db, Err)
    end.

do_fold(Stmt, Fun, Acc) ->
        case sqlite3_nif:sqlite3_step(Stmt) of
            ok ->
                {ok, Acc};
            {error, _} = Err ->
                Err;
            Row ->
                do_fold(Stmt, Fun, Fun(Row, Acc))
        end.

map(Db, Sql, Fun) ->
    map(Db, Sql, [], Fun).

map(Db, Sql, Params, Fun) ->
    case sqlite3_nif:sqlite3_prepare_v2(Db, Sql) of
        {ok, Stmt, _Leftover} ->
            case sqlite3_nif:sqlite3_bind(Stmt, Params) of
                ok ->
                    expand_error(do_map(Stmt, Fun, []));
                {error, _} = Err ->
                    expand_error(Db, Err)
            end;
        {error, _} = Err ->
            expand_error(Db, Err)
    end.

do_map(Stmt, Fun, Acc) ->
    case sqlite3_nif:sqlite3_step(Stmt) of
        ok ->
            {ok, lists:reverse(Acc)};
        {error, _} = Err ->
            Err;
        Row ->
            do_map(Stmt, Fun, [Fun(Row) | Acc])
    end.


make_flags(Atoms) ->
    lists:foldl(fun(Flag, Acc) ->
                        atom_to_int_flag(Flag) bor Acc
                end, 0, Atoms).

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0.

int_to_bool(0) ->
    false;
int_to_bool(X) when is_integer(X) ->
    true.

atom_to_int_flag('SQLITE_OPEN_READONLY') ->
    ?SQLITE_OPEN_READONLY;
atom_to_int_flag('SQLITE_OPEN_READWRITE') ->
    ?SQLITE_OPEN_READWRITE;
atom_to_int_flag('SQLITE_OPEN_CREATE') ->
    ?SQLITE_OPEN_CREATE;
atom_to_int_flag('SQLITE_OPEN_URI') ->
    ?SQLITE_OPEN_URI;
atom_to_int_flag('SQLITE_OPEN_MEMORY') ->
    ?SQLITE_OPEN_MEMORY;
atom_to_int_flag('SQLITE_OPEN_NOMUTEX') ->
    ?SQLITE_OPEN_NOMUTEX;
atom_to_int_flag('SQLITE_OPEN_FULLMUTEX') ->
    ?SQLITE_OPEN_FULLMUTEX;
atom_to_int_flag('SQLITE_OPEN_SHAREDCACHE') ->
    ?SQLITE_OPEN_SHAREDCACHE;
atom_to_int_flag('SQLITE_OPEN_PRIVATECACHE') ->
    ?SQLITE_OPEN_PRIVATECACHE;
atom_to_int_flag('SQLITE_OPEN_NOFOLLOW') ->
    ?SQLITE_OPEN_NOFOLLOW;
atom_to_int_flag('SQLITE_DBCONFIG_MAINDBNAME') ->
    ?SQLITE_DBCONFIG_MAINDBNAME;
atom_to_int_flag('SQLITE_DBCONFIG_ENABLE_FKEY') ->
    ?SQLITE_DBCONFIG_ENABLE_FKEY;
atom_to_int_flag('SQLITE_DBCONFIG_ENABLE_TRIGGER') ->
    ?SQLITE_DBCONFIG_ENABLE_TRIGGER;
atom_to_int_flag('SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER') ->
    ?SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER;
atom_to_int_flag('SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION') ->
    ?SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION;
atom_to_int_flag('SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE') ->
    ?SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE;
atom_to_int_flag('SQLITE_DBCONFIG_ENABLE_QPSG') ->
    ?SQLITE_DBCONFIG_ENABLE_QPSG;
atom_to_int_flag('SQLITE_DBCONFIG_TRIGGER_EQP') ->
    ?SQLITE_DBCONFIG_TRIGGER_EQP;
atom_to_int_flag('SQLITE_DBCONFIG_RESET_DATABASE') ->
    ?SQLITE_DBCONFIG_RESET_DATABASE;
atom_to_int_flag('SQLITE_DBCONFIG_DEFENSIVE') ->
    ?SQLITE_DBCONFIG_DEFENSIVE;
atom_to_int_flag('SQLITE_DBCONFIG_WRITABLE_SCHEMA') ->
    ?SQLITE_DBCONFIG_WRITABLE_SCHEMA;
atom_to_int_flag('SQLITE_DBCONFIG_LEGACY_ALTER_TABLE') ->
    ?SQLITE_DBCONFIG_LEGACY_ALTER_TABLE;
atom_to_int_flag('SQLITE_DBCONFIG_DQS_DML') ->
    ?SQLITE_DBCONFIG_DQS_DML;
atom_to_int_flag('SQLITE_DBCONFIG_DQS_DDL') ->
    ?SQLITE_DBCONFIG_DQS_DDL;
atom_to_int_flag('SQLITE_DBCONFIG_ENABLE_VIEW') ->
    ?SQLITE_DBCONFIG_ENABLE_VIEW;
atom_to_int_flag('SQLITE_DBCONFIG_LEGACY_FILE_FORMAT') ->
    ?SQLITE_DBCONFIG_LEGACY_FILE_FORMAT;
atom_to_int_flag('SQLITE_DBCONFIG_TRUSTED_SCHEMA') ->
    ?SQLITE_DBCONFIG_TRUSTED_SCHEMA;
atom_to_int_flag('SQLITE_LIMIT_LENGTH') ->
    ?SQLITE_LIMIT_LENGTH;
atom_to_int_flag('SQLITE_LIMIT_SQL_LENGTH') ->
    ?SQLITE_LIMIT_SQL_LENGTH;
atom_to_int_flag('SQLITE_LIMIT_COLUMN') ->
    ?SQLITE_LIMIT_COLUMN;
atom_to_int_flag('SQLITE_LIMIT_EXPR_DEPTH') ->
    ?SQLITE_LIMIT_EXPR_DEPTH;
atom_to_int_flag('SQLITE_LIMIT_COMPOUND_SELECT') ->
    ?SQLITE_LIMIT_COMPOUND_SELECT;
atom_to_int_flag('SQLITE_LIMIT_VDBE_OP') ->
    ?SQLITE_LIMIT_VDBE_OP;
atom_to_int_flag('SQLITE_LIMIT_FUNCTION_ARG') ->
    ?SQLITE_LIMIT_FUNCTION_ARG;
atom_to_int_flag('SQLITE_LIMIT_ATTACHED') ->
    ?SQLITE_LIMIT_ATTACHED;
atom_to_int_flag('SQLITE_LIMIT_LIKE_PATTERN_LENGTH') ->
    ?SQLITE_LIMIT_LIKE_PATTERN_LENGTH;
atom_to_int_flag('SQLITE_LIMIT_VARIABLE_NUMBER') ->
    ?SQLITE_LIMIT_VARIABLE_NUMBER;
atom_to_int_flag('SQLITE_LIMIT_TRIGGER_DEPTH') ->
    ?SQLITE_LIMIT_TRIGGER_DEPTH;
atom_to_int_flag('SQLITE_LIMIT_WORKER_THREADS') ->
    ?SQLITE_LIMIT_WORKER_THREADS;
atom_to_int_flag('SQLITE_DBSTATUS_LOOKASIDE_USED') ->
    ?SQLITE_DBSTATUS_LOOKASIDE_USED;
atom_to_int_flag('SQLITE_DBSTATUS_CACHE_USED') ->
    ?SQLITE_DBSTATUS_CACHE_USED;
atom_to_int_flag('SQLITE_DBSTATUS_SCHEMA_USED') ->
    ?SQLITE_DBSTATUS_SCHEMA_USED;
atom_to_int_flag('SQLITE_DBSTATUS_STMT_USED') ->
    ?SQLITE_DBSTATUS_STMT_USED;
atom_to_int_flag('SQLITE_DBSTATUS_LOOKASIDE_HIT') ->
    ?SQLITE_DBSTATUS_LOOKASIDE_HIT;
atom_to_int_flag('SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE') ->
    ?SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE;
atom_to_int_flag('SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL') ->
    ?SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL;
atom_to_int_flag('SQLITE_DBSTATUS_CACHE_HIT') ->
    ?SQLITE_DBSTATUS_CACHE_HIT;
atom_to_int_flag('SQLITE_DBSTATUS_CACHE_MISS') ->
    ?SQLITE_DBSTATUS_CACHE_MISS;
atom_to_int_flag('SQLITE_DBSTATUS_CACHE_WRITE') ->
    ?SQLITE_DBSTATUS_CACHE_WRITE;
atom_to_int_flag('SQLITE_DBSTATUS_DEFERRED_FKS') ->
    ?SQLITE_DBSTATUS_DEFERRED_FKS;
atom_to_int_flag('SQLITE_DBSTATUS_CACHE_USED_SHARED') ->
    ?SQLITE_DBSTATUS_CACHE_USED_SHARED;
atom_to_int_flag('SQLITE_DBSTATUS_CACHE_SPILL') ->
    ?SQLITE_DBSTATUS_CACHE_SPILL;
atom_to_int_flag('SQLITE_CHECKPOINT_PASSIVE') ->
    ?SQLITE_CHECKPOINT_PASSIVE;
atom_to_int_flag('SQLITE_CHECKPOINT_FULL') ->
    ?SQLITE_CHECKPOINT_FULL;
atom_to_int_flag('SQLITE_CHECKPOINT_RESTART') ->
    ?SQLITE_CHECKPOINT_RESTART;
atom_to_int_flag('SQLITE_CHECKPOINT_TRUNCATE') ->
    ?SQLITE_CHECKPOINT_TRUNCATE.

err_to_atom(?SQLITE_OK) ->
    'SQLITE_OK';
err_to_atom(?SQLITE_ERROR) ->
    'SQLITE_ERROR';
err_to_atom(?SQLITE_INTERNAL) ->
    'SQLITE_INTERNAL';
err_to_atom(?SQLITE_PERM) ->
    'SQLITE_PERM';
err_to_atom(?SQLITE_ABORT) ->
    'SQLITE_ABORT';
err_to_atom(?SQLITE_BUSY) ->
    'SQLITE_BUSY';
err_to_atom(?SQLITE_LOCKED) ->
    'SQLITE_LOCKED';
err_to_atom(?SQLITE_NOMEM) ->
    'SQLITE_NOMEM';
err_to_atom(?SQLITE_READONLY) ->
    'SQLITE_READONLY';
err_to_atom(?SQLITE_INTERRUPT) ->
    'SQLITE_INTERRUPT';
err_to_atom(?SQLITE_IOERR) ->
    'SQLITE_IOERR';
err_to_atom(?SQLITE_CORRUPT) ->
    'SQLITE_CORRUPT';
err_to_atom(?SQLITE_NOTFOUND) ->
    'SQLITE_NOTFOUND';
err_to_atom(?SQLITE_FULL) ->
    'SQLITE_FULL';
err_to_atom(?SQLITE_CANTOPEN) ->
    'SQLITE_CANTOPEN';
err_to_atom(?SQLITE_PROTOCOL) ->
    'SQLITE_PROTOCOL';
err_to_atom(?SQLITE_EMPTY) ->
    'SQLITE_EMPTY';
err_to_atom(?SQLITE_SCHEMA) ->
    'SQLITE_SCHEMA';
err_to_atom(?SQLITE_TOOBIG) ->
    'SQLITE_TOOBIG';
err_to_atom(?SQLITE_CONSTRAINT) ->
    'SQLITE_CONSTRAINT';
err_to_atom(?SQLITE_MISMATCH) ->
    'SQLITE_MISMATCH';
err_to_atom(?SQLITE_MISUSE) ->
    'SQLITE_MISUSE';
err_to_atom(?SQLITE_NOLFS) ->
    'SQLITE_NOLFS';
err_to_atom(?SQLITE_AUTH) ->
    'SQLITE_AUTH';
err_to_atom(?SQLITE_FORMAT) ->
    'SQLITE_FORMAT';
err_to_atom(?SQLITE_RANGE) ->
    'SQLITE_RANGE';
err_to_atom(?SQLITE_NOTADB) ->
    'SQLITE_NOTADB';
err_to_atom(?SQLITE_NOTICE) ->
    'SQLITE_NOTICE';
err_to_atom(?SQLITE_WARNING) ->
    'SQLITE_WARNING';
err_to_atom(?SQLITE_ROW) ->
    'SQLITE_ROW';
err_to_atom(?SQLITE_DONE) ->
    'SQLITE_DONE';
err_to_atom(?SQLITE_ERROR_MISSING_COLLSEQ) ->
    'SQLITE_ERROR_MISSING_COLLSEQ';
err_to_atom(?SQLITE_ERROR_RETRY) ->
    'SQLITE_ERROR_RETRY';
err_to_atom(?SQLITE_ERROR_SNAPSHOT) ->
    'SQLITE_ERROR_SNAPSHOT';
err_to_atom(?SQLITE_IOERR_READ) ->
    'SQLITE_IOERR_READ';
err_to_atom(?SQLITE_IOERR_SHORT_READ) ->
    'SQLITE_IOERR_SHORT_READ';
err_to_atom(?SQLITE_IOERR_WRITE) ->
    'SQLITE_IOERR_WRITE';
err_to_atom(?SQLITE_IOERR_FSYNC) ->
    'SQLITE_IOERR_FSYNC';
err_to_atom(?SQLITE_IOERR_DIR_FSYNC) ->
    'SQLITE_IOERR_DIR_FSYNC';
err_to_atom(?SQLITE_IOERR_TRUNCATE) ->
    'SQLITE_IOERR_TRUNCATE';
err_to_atom(?SQLITE_IOERR_FSTAT) ->
    'SQLITE_IOERR_FSTAT';
err_to_atom(?SQLITE_IOERR_UNLOCK) ->
    'SQLITE_IOERR_UNLOCK';
err_to_atom(?SQLITE_IOERR_RDLOCK) ->
    'SQLITE_IOERR_RDLOCK';
err_to_atom(?SQLITE_IOERR_DELETE) ->
    'SQLITE_IOERR_DELETE';
err_to_atom(?SQLITE_IOERR_BLOCKED) ->
    'SQLITE_IOERR_BLOCKED';
err_to_atom(?SQLITE_IOERR_NOMEM) ->
    'SQLITE_IOERR_NOMEM';
err_to_atom(?SQLITE_IOERR_ACCESS) ->
    'SQLITE_IOERR_ACCESS';
err_to_atom(?SQLITE_IOERR_CHECKRESERVEDLOCK) ->
    'SQLITE_IOERR_CHECKRESERVEDLOCK';
err_to_atom(?SQLITE_IOERR_LOCK) ->
    'SQLITE_IOERR_LOCK';
err_to_atom(?SQLITE_IOERR_CLOSE) ->
    'SQLITE_IOERR_CLOSE';
err_to_atom(?SQLITE_IOERR_DIR_CLOSE) ->
    'SQLITE_IOERR_DIR_CLOSE';
err_to_atom(?SQLITE_IOERR_SHMOPEN) ->
    'SQLITE_IOERR_SHMOPEN';
err_to_atom(?SQLITE_IOERR_SHMSIZE) ->
    'SQLITE_IOERR_SHMSIZE';
err_to_atom(?SQLITE_IOERR_SHMLOCK) ->
    'SQLITE_IOERR_SHMLOCK';
err_to_atom(?SQLITE_IOERR_SHMMAP) ->
    'SQLITE_IOERR_SHMMAP';
err_to_atom(?SQLITE_IOERR_SEEK) ->
    'SQLITE_IOERR_SEEK';
err_to_atom(?SQLITE_IOERR_DELETE_NOENT) ->
    'SQLITE_IOERR_DELETE_NOENT';
err_to_atom(?SQLITE_IOERR_MMAP) ->
    'SQLITE_IOERR_MMAP';
err_to_atom(?SQLITE_IOERR_GETTEMPPATH) ->
    'SQLITE_IOERR_GETTEMPPATH';
err_to_atom(?SQLITE_IOERR_CONVPATH) ->
    'SQLITE_IOERR_CONVPATH';
err_to_atom(?SQLITE_IOERR_VNODE) ->
    'SQLITE_IOERR_VNODE';
err_to_atom(?SQLITE_IOERR_AUTH) ->
    'SQLITE_IOERR_AUTH';
err_to_atom(?SQLITE_IOERR_BEGIN_ATOMIC) ->
    'SQLITE_IOERR_BEGIN_ATOMIC';
err_to_atom(?SQLITE_IOERR_COMMIT_ATOMIC) ->
    'SQLITE_IOERR_COMMIT_ATOMIC';
err_to_atom(?SQLITE_IOERR_ROLLBACK_ATOMIC) ->
    'SQLITE_IOERR_ROLLBACK_ATOMIC';
err_to_atom(?SQLITE_IOERR_DATA) ->
    'SQLITE_IOERR_DATA';
err_to_atom(?SQLITE_LOCKED_SHAREDCACHE) ->
    'SQLITE_LOCKED_SHAREDCACHE';
err_to_atom(?SQLITE_LOCKED_VTAB) ->
    'SQLITE_LOCKED_VTAB';
err_to_atom(?SQLITE_BUSY_RECOVERY) ->
    'SQLITE_BUSY_RECOVERY';
err_to_atom(?SQLITE_BUSY_SNAPSHOT) ->
    'SQLITE_BUSY_SNAPSHOT';
err_to_atom(?SQLITE_BUSY_TIMEOUT) ->
    'SQLITE_BUSY_TIMEOUT';
err_to_atom(?SQLITE_CANTOPEN_NOTEMPDIR) ->
    'SQLITE_CANTOPEN_NOTEMPDIR';
err_to_atom(?SQLITE_CANTOPEN_ISDIR) ->
    'SQLITE_CANTOPEN_ISDIR';
err_to_atom(?SQLITE_CANTOPEN_FULLPATH) ->
    'SQLITE_CANTOPEN_FULLPATH';
err_to_atom(?SQLITE_CANTOPEN_CONVPATH) ->
    'SQLITE_CANTOPEN_CONVPATH';
err_to_atom(?SQLITE_CANTOPEN_DIRTYWAL) ->
    'SQLITE_CANTOPEN_DIRTYWAL';
err_to_atom(?SQLITE_CANTOPEN_SYMLINK) ->
    'SQLITE_CANTOPEN_SYMLINK';
err_to_atom(?SQLITE_CORRUPT_VTAB) ->
    'SQLITE_CORRUPT_VTAB';
err_to_atom(?SQLITE_CORRUPT_SEQUENCE) ->
    'SQLITE_CORRUPT_SEQUENCE';
err_to_atom(?SQLITE_CORRUPT_INDEX) ->
    'SQLITE_CORRUPT_INDEX';
err_to_atom(?SQLITE_READONLY_RECOVERY) ->
    'SQLITE_READONLY_RECOVERY';
err_to_atom(?SQLITE_READONLY_CANTLOCK) ->
    'SQLITE_READONLY_CANTLOCK';
err_to_atom(?SQLITE_READONLY_ROLLBACK) ->
    'SQLITE_READONLY_ROLLBACK';
err_to_atom(?SQLITE_READONLY_DBMOVED) ->
    'SQLITE_READONLY_DBMOVED';
err_to_atom(?SQLITE_READONLY_CANTINIT) ->
    'SQLITE_READONLY_CANTINIT';
err_to_atom(?SQLITE_READONLY_DIRECTORY) ->
    'SQLITE_READONLY_DIRECTORY';
err_to_atom(?SQLITE_ABORT_ROLLBACK) ->
    'SQLITE_ABORT_ROLLBACK';
err_to_atom(?SQLITE_CONSTRAINT_CHECK) ->
    'SQLITE_CONSTRAINT_CHECK';
err_to_atom(?SQLITE_CONSTRAINT_COMMITHOOK) ->
    'SQLITE_CONSTRAINT_COMMITHOOK';
err_to_atom(?SQLITE_CONSTRAINT_FOREIGNKEY) ->
    'SQLITE_CONSTRAINT_FOREIGNKEY';
err_to_atom(?SQLITE_CONSTRAINT_FUNCTION) ->
    'SQLITE_CONSTRAINT_FUNCTION';
err_to_atom(?SQLITE_CONSTRAINT_NOTNULL) ->
    'SQLITE_CONSTRAINT_NOTNULL';
err_to_atom(?SQLITE_CONSTRAINT_PRIMARYKEY) ->
    'SQLITE_CONSTRAINT_PRIMARYKEY';
err_to_atom(?SQLITE_CONSTRAINT_TRIGGER) ->
    'SQLITE_CONSTRAINT_TRIGGER';
err_to_atom(?SQLITE_CONSTRAINT_UNIQUE) ->
    'SQLITE_CONSTRAINT_UNIQUE';
err_to_atom(?SQLITE_CONSTRAINT_VTAB) ->
    'SQLITE_CONSTRAINT_VTAB';
err_to_atom(?SQLITE_CONSTRAINT_ROWID) ->
    'SQLITE_CONSTRAINT_ROWID';
err_to_atom(?SQLITE_CONSTRAINT_PINNED) ->
    'SQLITE_CONSTRAINT_PINNED';
err_to_atom(?SQLITE_NOTICE_RECOVER_WAL) ->
    'SQLITE_NOTICE_RECOVER_WAL';
err_to_atom(?SQLITE_NOTICE_RECOVER_ROLLBACK) ->
    'SQLITE_NOTICE_RECOVER_ROLLBACK';
err_to_atom(?SQLITE_WARNING_AUTOINDEX) ->
    'SQLITE_WARNING_AUTOINDEX';
err_to_atom(?SQLITE_AUTH_USER) ->
    'SQLITE_AUTH_USER';
err_to_atom(?SQLITE_OK_LOAD_PERMANENTLY) ->
    'SQLITE_OK_LOAD_PERMANENTLY';
err_to_atom(?SQLITE_OK_SYMLINK) ->
    'SQLITE_OK_SYMLINK';
err_to_atom(_) ->
    'unknown_error'.

expand_error({ok, _Result} = OK) ->
    OK;
expand_error({error, Err}) when is_integer(Err) ->
    {error, err_to_atom(Err)};
expand_error(Err) when is_integer(Err) ->
    err_to_atom(Err);
expand_error(Otherwise) ->
    Otherwise.

expand_error(_Db, {ok, _Result} = OK) ->
    OK;
expand_error(Db, {error, Err}) when is_integer(Err) ->
    {error, {err_to_atom(Err), sqlite3_nif:sqlite3_errmsg(Db)}};
expand_error(_Db, Err) when is_integer(Err) ->
    err_to_atom(Err);
expand_error(_Db, Otherwise) ->
    Otherwise.

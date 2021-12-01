%% @doc Convenience and utility functions for the raw_sqlite3 library.
%%
-module(raw_sqlite3).

-include_lib("raw_sqlite3/include/sqlite3_nif.hrl").

-export([open/1, open/2, open/3,
         close/1,
         prepare/2,
         bind/2,
         step/1,
         reset/1,
         fetchall/1,
         q/2, q/3,
         exec/2, exec/3,
         insert_many/3,
         fold/4, fold/5,
         map/3, map/4,
         with_trxn/2,
         make_flags/1,
         atom_to_int_flag/1,
         int_to_bool/1,
         bool_to_int/1,
         err_to_atom/1,
         expand_error/1, expand_error/2]).

-export_type([
              utf8str/0,
              sqlite3/0,
              sqlite3_error_code/0,
              sqlite3_stmt/0,
              query_parameter/0
             ]).

%% @type utf8str(). A <code>iodata()</code> term which, when serialised, should
%% form a valid UTF8-encoded character string.
-type utf8str() :: iodata().

%% @type sqlite3(). A database connection handle.
-opaque sqlite3() :: sqlite3_nif:sqlite3().

%% @type sqlite3_error_code(). <em>sqlite3</em> error codes as atoms.
-type sqlite3_error_code() ::
        'SQLITE_OK'                      |
        'SQLITE_ERROR'                   |
        'SQLITE_INTERNAL'                |
        'SQLITE_PERM'                    |
        'SQLITE_ABORT'                   |
        'SQLITE_BUSY'                    |
        'SQLITE_LOCKED'                  |
        'SQLITE_NOMEM'                   |
        'SQLITE_READONLY'                |
        'SQLITE_INTERRUPT'               |
        'SQLITE_IOERR'                   |
        'SQLITE_CORRUPT'                 |
        'SQLITE_NOTFOUND'                |
        'SQLITE_FULL'                    |
        'SQLITE_CANTOPEN'                |
        'SQLITE_PROTOCOL'                |
        'SQLITE_EMPTY'                   |
        'SQLITE_SCHEMA'                  |
        'SQLITE_TOOBIG'                  |
        'SQLITE_CONSTRAINT'              |
        'SQLITE_MISMATCH'                |
        'SQLITE_MISUSE'                  |
        'SQLITE_NOLFS'                   |
        'SQLITE_AUTH'                    |
        'SQLITE_FORMAT'                  |
        'SQLITE_RANGE'                   |
        'SQLITE_NOTADB'                  |
        'SQLITE_NOTICE'                  |
        'SQLITE_WARNING'                 |
        'SQLITE_ROW'                     |
        'SQLITE_DONE'                    |
        'SQLITE_ERROR_MISSING_COLLSEQ'   |
        'SQLITE_ERROR_RETRY'             |
        'SQLITE_ERROR_SNAPSHOT'          |
        'SQLITE_IOERR_READ'              |
        'SQLITE_IOERR_SHORT_READ'        |
        'SQLITE_IOERR_WRITE'             |
        'SQLITE_IOERR_FSYNC'             |
        'SQLITE_IOERR_DIR_FSYNC'         |
        'SQLITE_IOERR_TRUNCATE'          |
        'SQLITE_IOERR_FSTAT'             |
        'SQLITE_IOERR_UNLOCK'            |
        'SQLITE_IOERR_RDLOCK'            |
        'SQLITE_IOERR_DELETE'            |
        'SQLITE_IOERR_BLOCKED'           |
        'SQLITE_IOERR_NOMEM'             |
        'SQLITE_IOERR_ACCESS'            |
        'SQLITE_IOERR_CHECKRESERVEDLOCK' |
        'SQLITE_IOERR_LOCK'              |
        'SQLITE_IOERR_CLOSE'             |
        'SQLITE_IOERR_DIR_CLOSE'         |
        'SQLITE_IOERR_SHMOPEN'           |
        'SQLITE_IOERR_SHMSIZE'           |
        'SQLITE_IOERR_SHMLOCK'           |
        'SQLITE_IOERR_SHMMAP'            |
        'SQLITE_IOERR_SEEK'              |
        'SQLITE_IOERR_DELETE_NOENT'      |
        'SQLITE_IOERR_MMAP'              |
        'SQLITE_IOERR_GETTEMPPATH'       |
        'SQLITE_IOERR_CONVPATH'          |
        'SQLITE_IOERR_VNODE'             |
        'SQLITE_IOERR_AUTH'              |
        'SQLITE_IOERR_BEGIN_ATOMIC'      |
        'SQLITE_IOERR_COMMIT_ATOMIC'     |
        'SQLITE_IOERR_ROLLBACK_ATOMIC'   |
        'SQLITE_IOERR_DATA'              |
        'SQLITE_IOERR_CORRUPTFS'         |
        'SQLITE_LOCKED_SHAREDCACHE'      |
        'SQLITE_LOCKED_VTAB'             |
        'SQLITE_BUSY_RECOVERY'           |
        'SQLITE_BUSY_SNAPSHOT'           |
        'SQLITE_BUSY_TIMEOUT'            |
        'SQLITE_CANTOPEN_NOTEMPDIR'      |
        'SQLITE_CANTOPEN_ISDIR'          |
        'SQLITE_CANTOPEN_FULLPATH'       |
        'SQLITE_CANTOPEN_CONVPATH'       |
        'SQLITE_CANTOPEN_DIRTYWAL'       |
        'SQLITE_CANTOPEN_SYMLINK'        |
        'SQLITE_CORRUPT_VTAB'            |
        'SQLITE_CORRUPT_SEQUENCE'        |
        'SQLITE_CORRUPT_INDEX'           |
        'SQLITE_READONLY_RECOVERY'       |
        'SQLITE_READONLY_CANTLOCK'       |
        'SQLITE_READONLY_ROLLBACK'       |
        'SQLITE_READONLY_DBMOVED'        |
        'SQLITE_READONLY_CANTINIT'       |
        'SQLITE_READONLY_DIRECTORY'      |
        'SQLITE_ABORT_ROLLBACK'          |
        'SQLITE_CONSTRAINT_CHECK'        |
        'SQLITE_CONSTRAINT_COMMITHOOK'   |
        'SQLITE_CONSTRAINT_FOREIGNKEY'   |
        'SQLITE_CONSTRAINT_FUNCTION'     |
        'SQLITE_CONSTRAINT_NOTNULL'      |
        'SQLITE_CONSTRAINT_PRIMARYKEY'   |
        'SQLITE_CONSTRAINT_TRIGGER'      |
        'SQLITE_CONSTRAINT_UNIQUE'       |
        'SQLITE_CONSTRAINT_VTAB'         |
        'SQLITE_CONSTRAINT_ROWID'        |
        'SQLITE_CONSTRAINT_PINNED'       |
        'SQLITE_CONSTRAINT_DATATYPE'     |
        'SQLITE_NOTICE_RECOVER_WAL'      |
        'SQLITE_NOTICE_RECOVER_ROLLBACK' |
        'SQLITE_WARNING_AUTOINDEX'       |
        'SQLITE_AUTH_USER'               |
        'SQLITE_OK_LOAD_PERMANENTLY'     |
        'SQLITE_OK_SYMLINK'              |
        'unknown_error'.

-spec open(DbFile) -> Result
              when DbFile :: utf8str(),
                   Result :: {ok, Db} | {error, Reason},
                   Db     :: sqlite3(),
                   Reason :: {sqlite3_error_code(), utf8str()}.
%% @doc Open a new database connection.
open(DbFile) ->
    open(DbFile, ?SQLITE_OPEN_READWRITE bor ?SQLITE_OPEN_CREATE).

-spec open(DbFile, Flags) -> Result
              when DbFile :: utf8str(),
                   Flags  :: non_neg_integer(),
                   Result :: {ok, Db} | {error, Reason},
                   Db     :: sqlite3(),
                   Reason :: {sqlite3_error_code(), utf8str()}.

%% @doc Open a new database connection with the specified open flags.
%% @param Flags is a combination of open flags <code>OR</code>-ed together.
%% For example <code>?SQLITE_OPEN_READWRITE bor ?SQLITE_OPEN_CREATE</code>.
%% @see sqlite3_nif:open_flag(). The list of possible flag values
open(DbFile, Flags) ->
    open(DbFile, Flags, "").

-spec open(DbFile, Flags, Vfs) -> Result
              when DbFile :: utf8str(),
                   Flags  :: non_neg_integer(),
                   Vfs    :: utf8str(),
                   Result :: {ok, Db} | {error, Reason},
                   Db     :: sqlite3(),
                   Reason :: {sqlite3_error_code(), utf8str()}.

%% @doc Open a new database connection with the specified open flags and the VFS
%% engine.
%% See <a href="https://sqlite.org/c3ref/open.html">sqlite3 documentation</a>
%% for details.
open(DbFile, Flags, Vfs) ->
    expand_error(sqlite3_nif:sqlite3_open_v2(DbFile, Flags, Vfs)).

-spec close(Db) -> Result
              when Db     :: sqlite3(),
                   Result :: ok | {error, sqlite3_error_code()}.
%% @doc Close database connection.
%% NOTE: Usually, it is not necessary to call the function because a garbage
%% collection will destroy the connection object. However, it may be useful to
%% have the explicit control over a connection handle in some situations.
close(Db) ->
    case sqlite3_nif:sqlite3_close_v2(Db) of
        ?SQLITE_OK ->
            ok;
        Err ->
            {error, expand_error(Err)}
    end.



%% @type sqlite3_stmt(). A prepared SQL statement.
-opaque sqlite3_stmt() :: sqlite3_nif:sqlite3_stmt().

-spec prepare(Db, Sql) -> Result
              when Db      :: sqlite3(),
                   Sql     :: utf8str(),
                   Result  :: {ok, Stmt} | {error, Reason},
                   Reason  :: sqlite3_error_code()
                            | {sqlite3_error_code(), ErrDesc},
                   Stmt    :: sqlite3_stmt(),
                   ErrDesc :: utf8str().
%% @doc Prepare a statement to use with bind/2, step/1, fetchall/1, fetchall2.
%% NOTE: Only the first statement contained in the query <code>Sql</code> will be prepared.
%% Use <code>sqlite3_nif:sqlite3_prepare_v2/2</code> if you want to have the remaining
%% statements returned.
prepare(Db, Sql) ->
    case sqlite3_nif:sqlite3_prepare_v2(Db, Sql) of
        {ok, Stmt, _Leftover} ->
            {ok, Stmt};
        {error, _} = Err ->
            expand_error(Db, Err)
    end.

-type query_parameter()  :: number()
                          | true
                          | false
                          | nil
                          | undefined
                          | {text, utf8str()}
                          | {blob, iodata()}.

%% @doc Bind parameters to a prepared statement.
-spec bind(Stmt, Params) -> Result
              when Stmt    :: sqlite3_stmt(),
                   Params  :: [query_parameter()],
                   Result  :: ok | {error, Reason},
                   Reason :: sqlite3_error_code() |
                             {wrong_parameter_type, pos_integer()} |
                             wrong_parameter_count.
bind(Stmt, Params) ->
    expand_error(sqlite3_nif:sqlite3_bind(Stmt, Params)).

-spec step(Stmt) -> Result
              when Stmt    :: sqlite3_stmt(),
                   Result  :: ok | Row | {error, {sqlite3_error_code(), ErrDesc}},
                   ErrDesc :: utf8str(),
                   Row     :: tuple().
%% @doc Evaluate a prepared statement and fetch one result. A statement should
%% be run till <code>ok</code> is returned.
%% NOTE: a fully evaluated statement is automatically reset if <code>sqlite3_step</code> is called
%% using the statement after it returns <code>ok</code>.
step(Stmt) ->
    expand_error(sqlite3_nif:sqlite3_step(Stmt)).

-spec reset(Stmt::sqlite3_stmt()) -> ok | {error, sqlite3_error_code()}.
%% @doc Reset a prepared statement.
reset(Stmt) ->
    case sqlite3_nif:sqlite3_reset(Stmt) of
        ?SQLITE_OK -> ok;
        Err -> {error, expand_error(Err)}
    end.

-spec fetchall(Stmt) -> Result
              when Stmt    :: sqlite3_stmt(),
                   Result  :: Rows | {error, {sqlite3_error_code(), ErrDesc}},
                   Rows    :: [tuple()],
                   ErrDesc :: utf8str().
%% @doc Evaluate a prepared statement and fetch all results.
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

-spec q(Db, Sql) -> Result
              when Db      :: sqlite3(),
                   Sql     :: utf8str(),
                   Result  :: Rows | {error, {sqlite3_error_code(), ErrDesc}},
                   Rows    :: [tuple()],
                   ErrDesc :: utf8str().

%% @doc Evaluate an SQL expression and fetch all results.
q(Db, Sql) ->
    q(Db, Sql, []).

-spec q(Db, Sql, Params) -> Result
              when Db      :: sqlite3(),
                   Sql     :: utf8str(),
                   Params  :: [query_parameter()],
                   Result  :: Rows | {error, {sqlite3_error_code(), ErrDesc}},
                   Rows    :: [tuple()],
                   ErrDesc :: utf8str().

%% @doc Evaluate an SQL expression, binding the parameters, and fetch all results.
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

-spec exec(Db, Sql) -> Result
              when Db      :: sqlite3(),
                   Sql     :: utf8str(),
                   Result  :: ok | {error, Reason},
                   Reason  :: sqlite3_error_code()
                            | {sqlite3_error_code(), ErrDesc},
                   ErrDesc :: utf8str().
%% @doc Evaluate DDL/DML SQL statement(s).
exec(Db, Sql) ->
    exec(Db, Sql, []).

-spec exec(Db, Sql, Params) -> Result
              when Db      :: sqlite3(),
                   Sql     :: utf8str(),
                   Params  :: [query_parameter()],
                   Result  :: ok | {error, Reason},
                   Reason  :: sqlite3_error_code()
                            | {sqlite3_error_code(), ErrDesc},
                   ErrDesc :: utf8str().

%% @doc Evaluate a DDL/DML SQL statement, binding query parameters.
%% NOTE: Using more than one SQL statement with non-empty parameter list will
%% most likely result in an error because the function will try to bind the
%% supplied parameter list to every statement.
exec(_Db, <<>>, _Params) ->
    ok;
exec(Db, <<$ , Rest/binary>>, Params) ->
    exec(Db, Rest, Params);
exec(Db, <<$\n, Rest/binary>>, Params) ->
    exec(Db, Rest, Params);
exec(Db, <<$\r, Rest/binary>>, Params) ->
    exec(Db, Rest, Params);
exec(Db, <<$\t, Rest/binary>>, Params) ->
    exec(Db, Rest, Params);
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

-spec insert_many(Db, Sql, [Params]) -> Result
              when Db      :: sqlite3(),
                   Sql     :: utf8str(),
                   Params  :: [query_parameter()],
                   Result  :: ok | {error, Reason},
                   Reason  :: sqlite3_error_code()
                            | {sqlite3_error_code(), ErrDesc},
                   ErrDesc :: utf8str().
%% @doc Insert many values.
%% The function prepares an DML SQL statement and binds/evaluates it to every
%% element in <code>ParameterList</code>.
insert_many(Db, Sql, ParameterList) ->
    case sqlite3_nif:sqlite3_prepare_v2(Db, Sql) of
        {ok, Stmt, _Leftover} ->
            expand_error(Db, insert_many(Stmt, ParameterList));
        {error, _} = Err ->
            expand_error(Db, Err)
    end.

insert_many(_Stmt, []) ->
    ok;
insert_many(Stmt, [Params | Rest]) ->
   case sqlite3_nif:sqlite3_bind(Stmt, Params) of
       ok ->
           case sqlite3_nif:sqlite3_step(Stmt) of
               ok ->
                   ?SQLITE_OK = sqlite3_nif:sqlite3_reset(Stmt),
                   insert_many(Stmt, Rest);
               Err ->
                   Err
           end;
       Err ->
           Err
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

-spec fold(Db, Sql, Fun, Acc0) -> Result
              when Db      :: sqlite3(),
                   Sql     :: utf8str(),
                   Fun     :: fun((Elem, AccIn) -> AccOut),
                   Elem    :: tuple(),
                   Result  :: Acc | {error, Reason},
                   Reason  :: sqlite3_error_code()
                            | {sqlite3_error_code(), ErrDesc},
                   Acc     :: term(),
                   ErrDesc :: utf8str(),
                   Acc0    :: Acc,
                   AccIn   :: Acc,
                   AccOut  :: Acc.

%% @doc Calls <code>Fun(Elem, Acc)</code> on successive elements of the list
%% formed by evaluating a SQL statement. <code>Fun/2</code> must return a new
%% accumulator, which is passed to the next call. The function returns the
%% final value of the accumulator or error.
fold(Db, Sql, Fun, Acc) ->
    fold(Db, Sql, [], Fun, Acc).

-spec fold(Db, Sql, Params, Fun, Acc0) -> Result
              when Db      :: sqlite3(),
                   Sql     :: utf8str(),
                   Params  :: [query_parameter()],
                   Fun     :: fun((Elem, AccIn) -> AccOut),
                   Elem    :: tuple(),
                   Result  :: Acc | {error, Reason},
                   Reason  :: sqlite3_error_code()
                            | {sqlite3_error_code(), ErrDesc},
                   Acc     :: term(),
                   ErrDesc :: utf8str(),
                   Acc0    :: Acc,
                   AccIn   :: Acc,
                   AccOut  :: Acc.

%% @doc Same as <code>fold/4</code> but accepts a parameter list.
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

-spec map(Db, Sql, Fun) -> Result
              when Db      :: sqlite3(),
                   Sql     :: utf8str(),
                   Fun     :: fun((tuple()) -> T),
                   Result  :: [T] | {error, Reason},
                   Reason  :: sqlite3_error_code()
                            | {sqlite3_error_code(), ErrDesc},
                   T       :: term(),
                   ErrDesc :: utf8str().
%% @doc Transform the result of evaluating a SQL query by applying
%% <code>Fun(Elem)</code>. Returns the transformed list or an error.
map(Db, Sql, Fun) ->
    map(Db, Sql, [], Fun).

-spec map(Db, Sql, Params, Fun) -> Result
              when Db      :: sqlite3(),
                   Sql     :: utf8str(),
                   Params  :: [query_parameter()],
                   Fun     :: fun((tuple()) -> T),
                   Result  :: [T] | {error, Reason},
                   Reason  :: sqlite3_error_code()
                            | {sqlite3_error_code(), ErrDesc},
                   T       :: term(),
                   ErrDesc :: utf8str().
%% @doc Same as <code>map/4</code> but accepts a parameter list.
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

-spec with_trxn(Db, F) -> Result
              when Db :: sqlite3(),
                   F  :: fun(() -> T),
                   Result :: T | {error, M},
                   T :: term(),
                   M :: #{reason => term(), stacktrace => term()}.
%% @doc Evaluate <code>F</code> in a transaction.
%% The function automatically rolls back if <code>F</code> throws an error.
-ifdef(OTP_RELEASE).
with_trxn(Db, F) ->
    case exec(Db, "BEGIN") of
        ok ->
            try
                Rv = F(),
                ok = exec(Db, "COMMIT"),
                Rv
            catch
               _:Reason:Stacktrace ->
                    ok = exec(Db, "ROLLBACK"),
                    {error, #{reason => Reason, stacktrace => Stacktrace}}
            end;
        {error, _} = Err -> Err
    end.
-else.
with_trxn(Db, F) ->
    case exec(Db, "BEGIN") of
        ok ->
            try
                Rv = F(),
                ok = exec(Db, "COMMIT"),
                Rv
            catch
                _:Reason ->
                    Stacktrace = erlang:get_stacktrace(),
                    {error, #{reason => Reason, stacktrace => Stacktrace}}
            end;
        {error, _} = Err -> Err
    end.
-endif.

%% @doc Convert a list of atoms into an integer flag.
-spec make_flags([atom()]) -> integer().
make_flags(Atoms) ->
    lists:foldl(fun(Flag, Acc) ->
                        atom_to_int_flag(Flag) bor Acc
                end, 0, Atoms).

%% @doc Convert boolean values into integers (SQlite uses 0 and 1 for boolean values).
-spec bool_to_int(boolean()) -> 0 | 1.
bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0.

%% @doc Convert integer values into booleans (SQlite uses 0 and 1 for boolean values).
-spec int_to_bool(integer()) -> boolean().
int_to_bool(0) ->
    false;
int_to_bool(X) when is_integer(X) ->
    true.

%% @doc Convert an atom into an integer flag.
-spec atom_to_int_flag(atom) -> integer().
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
atom_to_int_flag('SQLITE_STMTSTATUS_FULLSCAN_STEP') ->
    ?SQLITE_STMTSTATUS_FULLSCAN_STEP;
atom_to_int_flag('SQLITE_STMTSTATUS_SORT') ->
    ?SQLITE_STMTSTATUS_SORT;
atom_to_int_flag('SQLITE_STMTSTATUS_AUTOINDEX') ->
    ?SQLITE_STMTSTATUS_AUTOINDEX;
atom_to_int_flag('SQLITE_STMTSTATUS_VM_STEP') ->
    ?SQLITE_STMTSTATUS_VM_STEP;
atom_to_int_flag('SQLITE_STMTSTATUS_REPREPARE') ->
    ?SQLITE_STMTSTATUS_REPREPARE;
atom_to_int_flag('SQLITE_STMTSTATUS_RUN') ->
    ?SQLITE_STMTSTATUS_RUN;
atom_to_int_flag('SQLITE_STMTSTATUS_MEMUSED') ->
    ?SQLITE_STMTSTATUS_MEMUSED;
atom_to_int_flag('SQLITE_CHECKPOINT_PASSIVE') ->
    ?SQLITE_CHECKPOINT_PASSIVE;
atom_to_int_flag('SQLITE_CHECKPOINT_FULL') ->
    ?SQLITE_CHECKPOINT_FULL;
atom_to_int_flag('SQLITE_CHECKPOINT_RESTART') ->
    ?SQLITE_CHECKPOINT_RESTART;
atom_to_int_flag('SQLITE_CHECKPOINT_TRUNCATE') ->
    ?SQLITE_CHECKPOINT_TRUNCATE.

%% @doc Convert an integer error value into the corresponding error atom.
-spec err_to_atom(integer()) -> atom().
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
err_to_atom(?SQLITE_IOERR_CORRUPTFS) ->
    'SQLITE_IOERR_CORRUPTFS';
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
err_to_atom(?SQLITE_CONSTRAINT_DATATYPE) ->
    'SQLITE_CONSTRAINT_DATATYPE';
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

%% @doc Convert a sqlite3_nif error, if present, into an error atom.
expand_error({ok, _Result} = OK) ->
    OK;
expand_error({error, Err}) when is_integer(Err) ->
    {error, err_to_atom(Err)};
expand_error(Err) when is_integer(Err) ->
    err_to_atom(Err);
expand_error(Otherwise) ->
    Otherwise.

%% @doc Convert a sqlite3_nif error, if present, into an error atom and error
%% description.
expand_error(_Db, {ok, _Result} = OK) ->
    OK;
expand_error(Db, {error, Err}) when is_integer(Err) ->
    {error, {err_to_atom(Err), sqlite3_nif:sqlite3_errmsg(Db)}};
expand_error(_Db, Err) when is_integer(Err) ->
    err_to_atom(Err);
expand_error(_Db, Otherwise) ->
    Otherwise.

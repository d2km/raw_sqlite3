%% Error codes
-define(SQLITE_OK, 0).
-define(SQLITE_ERROR, 1).
-define(SQLITE_INTERNAL, 2).
-define(SQLITE_PERM, 3).
-define(SQLITE_ABORT, 4).
-define(SQLITE_BUSY, 5).
-define(SQLITE_LOCKED, 6).
-define(SQLITE_NOMEM, 7).
-define(SQLITE_READONLY, 8).
-define(SQLITE_INTERRUPT, 9).
-define(SQLITE_IOERR, 10).
-define(SQLITE_CORRUPT, 11).
-define(SQLITE_NOTFOUND, 12).
-define(SQLITE_FULL, 13).
-define(SQLITE_CANTOPEN, 14).
-define(SQLITE_PROTOCOL, 15).
-define(SQLITE_EMPTY, 16).
-define(SQLITE_SCHEMA, 17).
-define(SQLITE_TOOBIG, 18).
-define(SQLITE_CONSTRAINT, 19).
-define(SQLITE_MISMATCH, 20).
-define(SQLITE_MISUSE, 21).
-define(SQLITE_NOLFS, 22).
-define(SQLITE_AUTH, 23).
-define(SQLITE_FORMAT, 24).
-define(SQLITE_RANGE, 25).
-define(SQLITE_NOTADB, 26).
-define(SQLITE_NOTICE, 27).
-define(SQLITE_WARNING, 28).
-define(SQLITE_ROW, 100).
-define(SQLITE_DONE, 101).

%% Extended error codes
-define(SQLITE_ERROR_MISSING_COLLSEQ, (?SQLITE_ERROR bor (1 bsl 8))).
-define(SQLITE_ERROR_RETRY, (?SQLITE_ERROR bor (2 bsl 8))).
-define(SQLITE_ERROR_SNAPSHOT, (?SQLITE_ERROR bor (3 bsl 8))).
-define(SQLITE_IOERR_READ, (?SQLITE_IOERR bor (1 bsl 8))).
-define(SQLITE_IOERR_SHORT_READ, (?SQLITE_IOERR bor (2 bsl 8))).
-define(SQLITE_IOERR_WRITE, (?SQLITE_IOERR bor (3 bsl 8))).
-define(SQLITE_IOERR_FSYNC, (?SQLITE_IOERR bor (4 bsl 8))).
-define(SQLITE_IOERR_DIR_FSYNC, (?SQLITE_IOERR bor (5 bsl 8))).
-define(SQLITE_IOERR_TRUNCATE, (?SQLITE_IOERR bor (6 bsl 8))).
-define(SQLITE_IOERR_FSTAT, (?SQLITE_IOERR bor (7 bsl 8))).
-define(SQLITE_IOERR_UNLOCK, (?SQLITE_IOERR bor (8 bsl 8))).
-define(SQLITE_IOERR_RDLOCK, (?SQLITE_IOERR bor (9 bsl 8))).
-define(SQLITE_IOERR_DELETE, (?SQLITE_IOERR bor (10 bsl 8))).
-define(SQLITE_IOERR_BLOCKED, (?SQLITE_IOERR bor (11 bsl 8))).
-define(SQLITE_IOERR_NOMEM, (?SQLITE_IOERR bor (12 bsl 8))).
-define(SQLITE_IOERR_ACCESS, (?SQLITE_IOERR bor (13 bsl 8))).
-define(SQLITE_IOERR_CHECKRESERVEDLOCK, (?SQLITE_IOERR bor (14 bsl 8))).
-define(SQLITE_IOERR_LOCK, (?SQLITE_IOERR bor (15 bsl 8))).
-define(SQLITE_IOERR_CLOSE, (?SQLITE_IOERR bor (16 bsl 8))).
-define(SQLITE_IOERR_DIR_CLOSE, (?SQLITE_IOERR bor (17 bsl 8))).
-define(SQLITE_IOERR_SHMOPEN, (?SQLITE_IOERR bor (18 bsl 8))).
-define(SQLITE_IOERR_SHMSIZE, (?SQLITE_IOERR bor (19 bsl 8))).
-define(SQLITE_IOERR_SHMLOCK, (?SQLITE_IOERR bor (20 bsl 8))).
-define(SQLITE_IOERR_SHMMAP, (?SQLITE_IOERR bor (21 bsl 8))).
-define(SQLITE_IOERR_SEEK, (?SQLITE_IOERR bor (22 bsl 8))).
-define(SQLITE_IOERR_DELETE_NOENT, (?SQLITE_IOERR bor (23 bsl 8))).
-define(SQLITE_IOERR_MMAP, (?SQLITE_IOERR bor (24 bsl 8))).
-define(SQLITE_IOERR_GETTEMPPATH, (?SQLITE_IOERR bor (25 bsl 8))).
-define(SQLITE_IOERR_CONVPATH, (?SQLITE_IOERR bor (26 bsl 8))).
-define(SQLITE_IOERR_VNODE, (?SQLITE_IOERR bor (27 bsl 8))).
-define(SQLITE_IOERR_AUTH, (?SQLITE_IOERR bor (28 bsl 8))).
-define(SQLITE_IOERR_BEGIN_ATOMIC, (?SQLITE_IOERR bor (29 bsl 8))).
-define(SQLITE_IOERR_COMMIT_ATOMIC, (?SQLITE_IOERR bor (30 bsl 8))).
-define(SQLITE_IOERR_ROLLBACK_ATOMIC, (?SQLITE_IOERR bor (31 bsl 8))).
-define(SQLITE_IOERR_DATA, (?SQLITE_IOERR bor (32 bsl 8))).
-define(SQLITE_IOERR_CORRUPTFS, (?SQLITE_IOERR bor (33 bsl 8))).
-define(SQLITE_LOCKED_SHAREDCACHE, (?SQLITE_LOCKED bor  (1 bsl 8))).
-define(SQLITE_LOCKED_VTAB, (?SQLITE_LOCKED bor  (2 bsl 8))).
-define(SQLITE_BUSY_RECOVERY, (?SQLITE_BUSY   bor  (1 bsl 8))).
-define(SQLITE_BUSY_SNAPSHOT, (?SQLITE_BUSY   bor  (2 bsl 8))).
-define(SQLITE_BUSY_TIMEOUT, (?SQLITE_BUSY   bor  (3 bsl 8))).
-define(SQLITE_CANTOPEN_NOTEMPDIR, (?SQLITE_CANTOPEN bor (1 bsl 8))).
-define(SQLITE_CANTOPEN_ISDIR, (?SQLITE_CANTOPEN bor (2 bsl 8))).
-define(SQLITE_CANTOPEN_FULLPATH, (?SQLITE_CANTOPEN bor (3 bsl 8))).
-define(SQLITE_CANTOPEN_CONVPATH, (?SQLITE_CANTOPEN bor (4 bsl 8))).
-define(SQLITE_CANTOPEN_DIRTYWAL, (?SQLITE_CANTOPEN bor (5 bsl 8)) ).
-define(SQLITE_CANTOPEN_SYMLINK, (?SQLITE_CANTOPEN bor (6 bsl 8))).
-define(SQLITE_CORRUPT_VTAB, (?SQLITE_CORRUPT bor (1 bsl 8))).
-define(SQLITE_CORRUPT_SEQUENCE, (?SQLITE_CORRUPT bor (2 bsl 8))).
-define(SQLITE_CORRUPT_INDEX, (?SQLITE_CORRUPT bor (3 bsl 8))).
-define(SQLITE_READONLY_RECOVERY, (?SQLITE_READONLY bor (1 bsl 8))).
-define(SQLITE_READONLY_CANTLOCK, (?SQLITE_READONLY bor (2 bsl 8))).
-define(SQLITE_READONLY_ROLLBACK, (?SQLITE_READONLY bor (3 bsl 8))).
-define(SQLITE_READONLY_DBMOVED, (?SQLITE_READONLY bor (4 bsl 8))).
-define(SQLITE_READONLY_CANTINIT, (?SQLITE_READONLY bor (5 bsl 8))).
-define(SQLITE_READONLY_DIRECTORY, (?SQLITE_READONLY bor (6 bsl 8))).
-define(SQLITE_ABORT_ROLLBACK, (?SQLITE_ABORT bor (2 bsl 8))).
-define(SQLITE_CONSTRAINT_CHECK, (?SQLITE_CONSTRAINT bor (1 bsl 8))).
-define(SQLITE_CONSTRAINT_COMMITHOOK, (?SQLITE_CONSTRAINT bor (2 bsl 8))).
-define(SQLITE_CONSTRAINT_FOREIGNKEY, (?SQLITE_CONSTRAINT bor (3 bsl 8))).
-define(SQLITE_CONSTRAINT_FUNCTION, (?SQLITE_CONSTRAINT bor (4 bsl 8))).
-define(SQLITE_CONSTRAINT_NOTNULL, (?SQLITE_CONSTRAINT bor (5 bsl 8))).
-define(SQLITE_CONSTRAINT_PRIMARYKEY, (?SQLITE_CONSTRAINT bor (6 bsl 8))).
-define(SQLITE_CONSTRAINT_TRIGGER, (?SQLITE_CONSTRAINT bor (7 bsl 8))).
-define(SQLITE_CONSTRAINT_UNIQUE, (?SQLITE_CONSTRAINT bor (8 bsl 8))).
-define(SQLITE_CONSTRAINT_VTAB, (?SQLITE_CONSTRAINT bor (9 bsl 8))).
-define(SQLITE_CONSTRAINT_ROWID, (?SQLITE_CONSTRAINT bor(10 bsl 8))).
-define(SQLITE_CONSTRAINT_PINNED, (?SQLITE_CONSTRAINT bor(11 bsl 8))).
-define(SQLITE_CONSTRAINT_DATATYPE, (?SQLITE_CONSTRAINT bor(12 bsl 8))).
-define(SQLITE_NOTICE_RECOVER_WAL, (?SQLITE_NOTICE bor (1 bsl 8))).
-define(SQLITE_NOTICE_RECOVER_ROLLBACK, (?SQLITE_NOTICE bor (2 bsl 8))).
-define(SQLITE_WARNING_AUTOINDEX, (?SQLITE_WARNING bor (1 bsl 8))).
-define(SQLITE_AUTH_USER, (?SQLITE_AUTH bor (1 bsl 8))).
-define(SQLITE_OK_LOAD_PERMANENTLY, (?SQLITE_OK bor (1 bsl 8))).
-define(SQLITE_OK_SYMLINK, (?SQLITE_OK bor (2 bsl 8))).

%% Open flags
-define(SQLITE_OPEN_READONLY, 16#00000001).
-define(SQLITE_OPEN_READWRITE, 16#00000002).
-define(SQLITE_OPEN_CREATE, 16#00000004).
-define(SQLITE_OPEN_URI, 16#00000040).
-define(SQLITE_OPEN_MEMORY, 16#00000080).
-define(SQLITE_OPEN_NOMUTEX, 16#00008000).
-define(SQLITE_OPEN_FULLMUTEX, 16#00010000).
-define(SQLITE_OPEN_SHAREDCACHE, 16#00020000).
-define(SQLITE_OPEN_PRIVATECACHE, 16#00040000).
-define(SQLITE_OPEN_NOFOLLOW, 16#01000000).
-define(SQLITE_OPEN_EXRESCODE, 16#02000000).

%% DB config
-define(SQLITE_DBCONFIG_MAINDBNAME, 1000).
-define(SQLITE_DBCONFIG_ENABLE_FKEY, 1002).
-define(SQLITE_DBCONFIG_ENABLE_TRIGGER, 1003).
-define(SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER, 1004).
-define(SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION, 1005).
-define(SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE, 1006).
-define(SQLITE_DBCONFIG_ENABLE_QPSG, 1007).
-define(SQLITE_DBCONFIG_TRIGGER_EQP, 1008).
-define(SQLITE_DBCONFIG_RESET_DATABASE, 1009).
-define(SQLITE_DBCONFIG_DEFENSIVE, 1010).
-define(SQLITE_DBCONFIG_WRITABLE_SCHEMA, 1011).
-define(SQLITE_DBCONFIG_LEGACY_ALTER_TABLE, 1012).
-define(SQLITE_DBCONFIG_DQS_DML, 1013).
-define(SQLITE_DBCONFIG_DQS_DDL, 1014).
-define(SQLITE_DBCONFIG_ENABLE_VIEW, 1015).
-define(SQLITE_DBCONFIG_LEGACY_FILE_FORMAT, 1016).
-define(SQLITE_DBCONFIG_TRUSTED_SCHEMA, 1017).

%% Limits
-define(SQLITE_LIMIT_LENGTH, 0).
-define(SQLITE_LIMIT_SQL_LENGTH, 1).
-define(SQLITE_LIMIT_COLUMN, 2).
-define(SQLITE_LIMIT_EXPR_DEPTH, 3).
-define(SQLITE_LIMIT_COMPOUND_SELECT, 4).
-define(SQLITE_LIMIT_VDBE_OP, 5).
-define(SQLITE_LIMIT_FUNCTION_ARG, 6).
-define(SQLITE_LIMIT_ATTACHED, 7).
-define(SQLITE_LIMIT_LIKE_PATTERN_LENGTH, 8).
-define(SQLITE_LIMIT_VARIABLE_NUMBER, 9).
-define(SQLITE_LIMIT_TRIGGER_DEPTH, 10).
-define(SQLITE_LIMIT_WORKER_THREADS, 11).

%% DB Status
-define(SQLITE_DBSTATUS_LOOKASIDE_USED, 0).
-define(SQLITE_DBSTATUS_CACHE_USED, 1).
-define(SQLITE_DBSTATUS_SCHEMA_USED, 2).
-define(SQLITE_DBSTATUS_STMT_USED, 3).
-define(SQLITE_DBSTATUS_LOOKASIDE_HIT, 4).
-define(SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE, 5).
-define(SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL, 6).
-define(SQLITE_DBSTATUS_CACHE_HIT, 7).
-define(SQLITE_DBSTATUS_CACHE_MISS, 8).
-define(SQLITE_DBSTATUS_CACHE_WRITE, 9).
-define(SQLITE_DBSTATUS_DEFERRED_FKS, 10).
-define(SQLITE_DBSTATUS_CACHE_USED_SHARED, 11).
-define(SQLITE_DBSTATUS_CACHE_SPILL, 12).

%% Transaction state
-define(SQLITE_TXN_NONE, 0).
-define(SQLITE_TXN_READ, 1).
-define(SQLITE_TXN_WRITE, 2).

%% Statement status
-define(SQLITE_STMTSTATUS_FULLSCAN_STEP, 1).
-define(SQLITE_STMTSTATUS_SORT, 2).
-define(SQLITE_STMTSTATUS_AUTOINDEX, 3).
-define(SQLITE_STMTSTATUS_VM_STEP, 4).
-define(SQLITE_STMTSTATUS_REPREPARE, 5).
-define(SQLITE_STMTSTATUS_RUN, 6).
-define(SQLITE_STMTSTATUS_MEMUSED, 99).

%% WAL checkpoints flags
-define(SQLITE_CHECKPOINT_PASSIVE, 0).
-define(SQLITE_CHECKPOINT_FULL, 1).
-define(SQLITE_CHECKPOINT_RESTART, 2).
-define(SQLITE_CHECKPOINT_TRUNCATE, 3).

#include "sqlite3/sqlite3.h"
#include <erl_nif.h>
#include <string.h>
#include <assert.h>

#ifdef DEBUG
#include <stdio.h>
#define dbg(...) enif_fprintf(stderr, __VA_ARGS__)

#define CHECK_ARGC(what, n)                                                                \
    do {                                                                                   \
        if ((what) != (n))                                                                 \
            return enif_make_badarg(env);                                                  \
    } while (0)

#else
#define dbg(...)                                                                           \
    do {                                                                                   \
    } while (0)
#define CHECK_ARGC(what, n)                                                                \
    do {                                                                                   \
    } while (0)
#endif

static ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if (!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom);
    }

    return ret;
}

static ERL_NIF_TERM
make_error_tuple(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return enif_make_tuple2(env, make_atom(env, "error"), term);
}

static ERL_NIF_TERM
make_ok_tuple(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return enif_make_tuple2(env, make_atom(env, "ok"), term);
}

static ERL_NIF_TERM
make_binary(ErlNifEnv* env, const void* data, size_t size)
{
    ErlNifBinary bin;
    ERL_NIF_TERM term;

    if (!enif_alloc_binary(size, &bin))
        return make_atom(env, "allocation_error");

    memcpy(bin.data, data, size);
    term = enif_make_binary(env, &bin);
    enif_release_binary(&bin);

    return term;
}

/* Resources that correspond to various sqlite3 structures */
static ErlNifResourceType* sqlite3_r;
static ErlNifResourceType* sqlite3_stmt_r;
static ErlNifResourceType* sqlite3_blob_r;
static ErlNifResourceType* sqlite3_backup_r;
static ErlNifResourceType* sqlite3_snapshot_r;

/* sqlite3 connection resource type */
typedef struct sqlite3_t
{
    sqlite3* db;
    /* db_name must be stored here becaue SQLITE_DBCONFIG_MAINDBNAME doesn't
       copy the argument */
    char* db_name;
} sqlite3_t;

/* sqlite3 statement resource type */
typedef struct sqlite_stmt_t
{
    sqlite3_stmt* stmt;
} sqlite3_stmt_t;

/* sqlite3 blob resource type */
typedef struct sqlite_blob_t
{
    sqlite3_blob* blob;
} sqlite3_blob_t;

typedef struct sqlite_backup_t
{
    sqlite3_backup* backup;
} sqlite3_backup_t;

typedef struct sqlite_snapshot_t
{
    sqlite3_snapshot* snapshot;
} sqlite3_snapshot_t;

/* Resource destructors */
static void
sqlite3_r_dtor(ErlNifEnv* env, void* obj)
{
    sqlite3_close_v2(((sqlite3_t*)obj)->db);
    sqlite3_free(((sqlite3_t*)obj)->db_name);
}

static void
sqlite3_stmt_r_dtor(ErlNifEnv* env, void* obj)
{
    sqlite3_finalize(((sqlite3_stmt_t*)obj)->stmt);
}

static void
sqlite3_blob_r_dtor(ErlNifEnv* env, void* obj)
{
    sqlite3_blob_close(((sqlite3_blob_t*)obj)->blob);
}

static void
sqlite3_backup_r_dtor(ErlNifEnv* env, void* obj)
{
    sqlite3_backup_finish(((sqlite3_backup_t*)obj)->backup);
}

static void
sqlite3_snapshot_r_dtor(ErlNifEnv* env, void* obj)
{
    sqlite3_snapshot_free(((sqlite3_snapshot_t*)obj)->snapshot);
}

#define GET_DB(env, term, rdb)                                                             \
    do {                                                                                   \
        if (!enif_get_resource((env), (term), sqlite3_r, (void**)(rdb)))                   \
            return enif_make_badarg(env);                                                  \
    } while (0)

#define GET_STMT(env, term, rstmt)                                                         \
    do {                                                                                   \
        if (!enif_get_resource((env), (term), sqlite3_stmt_r, (void**)(rstmt)))            \
            return enif_make_badarg(env);                                                  \
    } while (0)

#define GET_C_STR(env, term, pbin)                                                         \
    do {                                                                                   \
        if (!iodata_to_c_str((env), (term), (pbin)))                                       \
            return enif_make_badarg(env);                                                  \
    } while (0)

#define GET_INT(env, term, pn)                                                             \
    do {                                                                                   \
        if (!enif_get_int((env), (term), (pn)))                                            \
            return enif_make_badarg(env);                                                  \
    } while (0)

#define GET_BACKUP(env, term, pbackup)                                                     \
    do {                                                                                   \
        if (!enif_get_resource((env), (term), sqlite3_backup_r, (void**)(pbackup)))        \
            return enif_make_badarg(env);                                                  \
    } while (0)

#define GET_BLOB(env, term, pblob)                                                         \
    do {                                                                                   \
        if (!enif_get_resource((env), (term), sqlite3_blob_r, (void**)(pblob)))            \
            return enif_make_badarg(env);                                                  \
    } while (0)

static int
iodata_to_c_str(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifBinary* pbin)
{
    ERL_NIF_TERM list = enif_make_list2(env, term, enif_make_int(env, 0));
    return enif_inspect_iolist_as_binary(env, list, pbin);
}

static int
iodata_to_binary(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifBinary* pbin)
{
    return enif_inspect_binary(env, term, pbin) ||
           enif_inspect_iolist_as_binary(env, term, pbin);
}

static ERL_NIF_TERM
impl_sqlite3_open_v2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 3);

    ErlNifBinary path;
    GET_C_STR(env, argv[0], &path);

    int flags;
    GET_INT(env, argv[1], &flags);

    ErlNifBinary vfs;
    GET_C_STR(env, argv[2], &vfs);

    char* vfs_str = strlen((char*)vfs.data) == 0 ? NULL : (char*)vfs.data;
    sqlite3* db = NULL;

    int rv = sqlite3_open_v2((char*)path.data, &db, flags, vfs_str);

    if (rv != SQLITE_OK)
        return make_error_tuple(env, enif_make_int(env, rv));

    sqlite3_t* rdb = enif_alloc_resource(sqlite3_r, sizeof(sqlite3_t));
    if (!rdb) {
        sqlite3_close_v2(db);
        return enif_raise_exception(env, make_atom(env, "alloc_resource"));
    }

    rdb->db = db;
    rdb->db_name = NULL;

    ERL_NIF_TERM ret = enif_make_resource(env, rdb);
    enif_release_resource(rdb);

    return make_ok_tuple(env, ret);
}

static ERL_NIF_TERM
impl_sqlite3_close_v2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    int rv = sqlite3_close_v2(rdb->db);
    rdb->db = NULL;
    if (rdb->db_name)
        sqlite3_free(rdb->db_name);
    rdb->db_name = NULL;

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_limit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 3);

    sqlite3_t* rdb = NULL;
    int limit_id, new_limit;

    GET_DB(env, argv[0], &rdb);
    GET_INT(env, argv[1], &limit_id);
    GET_INT(env, argv[2], &new_limit);

    int rv = sqlite3_limit(rdb->db, limit_id, new_limit);

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_load_extension(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 3);

    sqlite3_t* rdb;
    ErlNifBinary file, proc;

    GET_DB(env, argv[0], &rdb);
    GET_C_STR(env, argv[1], &file);
    GET_C_STR(env, argv[2], &proc);

    char* err_msg;
    char* proc_str = proc.size == 1 ? (char*)proc.data : NULL;
    int rv = sqlite3_load_extension(rdb->db, (char*)file.data, proc_str, &err_msg);

    if (rv != SQLITE_OK) {
        ERL_NIF_TERM err = enif_make_string(env, err_msg, ERL_NIF_LATIN1);
        sqlite3_free(err_msg);
        return make_error_tuple(env, err);
    }

    return make_atom(env, "ok");
}

static ERL_NIF_TERM
impl_sqlite3_db_cacheflush(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb;
    GET_DB(env, argv[0], &rdb);

    return enif_make_int(env, sqlite3_db_cacheflush(rdb->db));
}

static ERL_NIF_TERM
impl_sqlite3_db_release_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb;
    GET_DB(env, argv[0], &rdb);

    return enif_make_int(env, sqlite3_db_release_memory(rdb->db));
}

static ERL_NIF_TERM
impl_sqlite3_db_config(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    sqlite3_t* rdb = NULL;
    int rv, op, val, ret = 0;
    ErlNifBinary db_name;
    ERL_NIF_TERM retval;
    CHECK_ARGC(argc, 3);

    GET_DB(env, argv[0], &rdb);
    GET_INT(env, argv[1], &op);

    if (op == SQLITE_DBCONFIG_MAINDBNAME) {
        GET_C_STR(env, argv[2], &db_name);
        if (rdb->db_name)
            sqlite3_free(rdb->db_name);
        rdb->db_name = (char*)sqlite3_malloc64(db_name.size);
        if (!rdb->db_name)
            return enif_raise_exception(env, make_atom(env, "alloc_resource"));
        memcpy(rdb->db_name, db_name.data, db_name.size);
        rv = sqlite3_db_config(rdb->db, op, rdb->db_name);
        return enif_make_int(env, rv);
    } else {
        GET_INT(env, argv[2], &val);
        rv = sqlite3_db_config(rdb->db, op, val, &ret);
        retval = enif_make_int(env, ret);
        return enif_make_tuple2(env, enif_make_int(env, rv), retval);
    }
}

static ERL_NIF_TERM
impl_sqlite3_set_last_insert_rowid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    sqlite3_t* rdb = NULL;
    int new_rowid;

    CHECK_ARGC(argc, 2);

    GET_DB(env, argv[0], &rdb);
    GET_INT(env, argv[1], &new_rowid);

    sqlite3_set_last_insert_rowid(rdb->db, new_rowid);

    return make_atom(env, "ok");
}

static ERL_NIF_TERM
impl_sqlite3_busy_timeout(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    sqlite3_t* rdb = NULL;
    int rv, timeout;

    CHECK_ARGC(argc, 2);

    GET_DB(env, argv[0], &rdb);
    GET_INT(env, argv[1], &timeout);

    rv = sqlite3_busy_timeout(rdb->db, timeout);

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_extended_result_codes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    sqlite3_t* rdb = NULL;
    int rv, onoff;

    CHECK_ARGC(argc, 2);

    GET_DB(env, argv[0], &rdb);
    GET_INT(env, argv[1], &onoff);

    rv = sqlite3_extended_result_codes(rdb->db, onoff);

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_interrupt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    sqlite3_interrupt(rdb->db);

    return make_atom(env, "ok");
}

static ERL_NIF_TERM
impl_sqlite3_get_autocommit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    return enif_make_int(env, sqlite3_get_autocommit(rdb->db));
}

static ERL_NIF_TERM
impl_sqlite3_last_insert_rowid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    return enif_make_int(env, sqlite3_last_insert_rowid(rdb->db));
}

static ERL_NIF_TERM
impl_sqlite3_changes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    return enif_make_int(env, sqlite3_changes(rdb->db));
}

static ERL_NIF_TERM
impl_sqlite3_total_changes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    return enif_make_int(env, sqlite3_total_changes(rdb->db));
}

static ERL_NIF_TERM
impl_sqlite3_db_filename(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 2);

    sqlite3_t* rdb = NULL;
    ErlNifBinary db_name;

    GET_DB(env, argv[0], &rdb);
    GET_C_STR(env, argv[1], &db_name);

    const char* db_filename = sqlite3_db_filename(rdb->db, (char*)db_name.data);

    return make_binary(env, db_filename, strlen(db_filename));
}

static ERL_NIF_TERM
impl_sqlite3_db_readonly(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 2);
    sqlite3_t* rdb = NULL;
    ErlNifBinary db_name;

    GET_DB(env, argv[0], &rdb);
    GET_C_STR(env, argv[1], &db_name);

    int rv = sqlite3_db_readonly(rdb->db, (char*)db_name.data);

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_db_status(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    sqlite3_t* rdb = NULL;
    int rv, cur, hw, status_op, reset_flag;

    CHECK_ARGC(argc, 3);

    GET_DB(env, argv[0], &rdb);
    GET_INT(env, argv[1], &status_op);
    GET_INT(env, argv[2], &reset_flag);

    rv = sqlite3_db_status(rdb->db, status_op, &cur, &hw, reset_flag);
    if (rv != SQLITE_OK)
        return make_error_tuple(env, enif_make_int(env, rv));

    return enif_make_tuple2(env, enif_make_int(env, cur), enif_make_int(env, hw));
}

static ERL_NIF_TERM
impl_sqlite3_prepare_v2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    CHECK_ARGC(argc, 2);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    ErlNifBinary sql;
    if (!iodata_to_binary(env, argv[1], &sql))
        return enif_make_badarg(env);

    const char* leftover = NULL;
    sqlite3_stmt* stmt = NULL;
    int rv = sqlite3_prepare_v2(rdb->db, (char*)sql.data, sql.size, &stmt, &leftover);

    if (rv != SQLITE_OK)
        return make_error_tuple(env, enif_make_int(env, rv));

    sqlite3_stmt_t* rstmt = enif_alloc_resource(sqlite3_stmt_r, sizeof(sqlite3_stmt_t));
    if (!rstmt) {
        sqlite3_finalize(stmt);
        return enif_raise_exception(env, make_atom(env, "alloc_resource"));
    }

    rstmt->stmt = stmt;

    ERL_NIF_TERM ret_stmt = enif_make_resource(env, rstmt);
    enif_release_resource(rstmt);

    int leftover_size = leftover ? sql.size - (leftover - (char*)sql.data) : 0;
    ERL_NIF_TERM ret_leftover = make_binary(env, leftover, leftover_size);

    return enif_make_tuple3(env, make_atom(env, "ok"), ret_stmt, ret_leftover);
}

static int
bind_cell(ErlNifEnv* env, sqlite3_stmt* stmt, ERL_NIF_TERM term, int idx)
{
    if (enif_is_number(env, term)) {
        int an_int;
        if (enif_get_int(env, term, &an_int))
            return sqlite3_bind_int(stmt, idx, an_int);

        long int a_long_int;
        if (enif_get_int64(env, term, &a_long_int))
            return sqlite3_bind_int64(stmt, idx, a_long_int);

        double a_double;
        if (enif_get_double(env, term, &a_double))
            return sqlite3_bind_double(stmt, idx, a_double);

    } else if (enif_is_atom(env, term)) {
        char atom[256] = { 0 };
        if (!enif_get_atom(env, term, atom, sizeof(atom), ERL_NIF_LATIN1))
            return -1;

        if (strcmp("nil", atom) == 0 || strcmp("undefined", atom) == 0)
            return sqlite3_bind_null(stmt, idx);

        if (strcmp("true", atom) == 0)
            return sqlite3_bind_int(stmt, idx, 1);

        if (strcmp("false", atom) == 0)
            return sqlite3_bind_int(stmt, idx, 0);

    } else if (enif_is_tuple(env, term)) {
        const ERL_NIF_TERM* tuple;
        int arity;

        if (!enif_get_tuple(env, term, &arity, &tuple) || arity != 2)
            return -1;

        char atom[256] = { 0 };
        if (!enif_get_atom(env, tuple[0], atom, sizeof(atom), ERL_NIF_LATIN1))
            return -1;

        ErlNifBinary bin;
        if (strcmp("text", atom) == 0) {
            if (!iodata_to_binary(env, tuple[1], &bin))
                return -1;

            return sqlite3_bind_text(
              stmt, idx, (char*)bin.data, bin.size, SQLITE_TRANSIENT);
        } else if (strcmp("blob", atom) == 0) {
            if (!iodata_to_binary(env, tuple[1], &bin))
                return -1;

            return sqlite3_bind_blob(stmt, idx, bin.data, bin.size, SQLITE_TRANSIENT);
        }
    }

    return -1;
}

static ERL_NIF_TERM
impl_sqlite3_bind(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 2);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    int param_count = sqlite3_bind_parameter_count(rstmt->stmt);
    unsigned len;
    if (!enif_get_list_length(env, argv[1], &len))
        return enif_make_badarg(env);

    if (len != param_count)
        return make_error_tuple(env, make_atom(env, "wrong_parameter_count"));

    ERL_NIF_TERM list = argv[1], head, tail;

    for (int i = 0; i < len; i++) {
        if (!enif_get_list_cell(env, list, &head, &tail))
            return enif_make_badarg(env);
        int rv = bind_cell(env, rstmt->stmt, head, i + 1);

        if (rv == -1) {
            ERL_NIF_TERM desc = make_atom(env, "wrong_parameter_type");
            ERL_NIF_TERM err = enif_make_tuple2(env, desc, enif_make_int(env, i + 1));
            return make_error_tuple(env, err);
        }

        if (rv != SQLITE_OK)
            return make_error_tuple(env, enif_make_int(env, i + 1));

        list = tail;
    }

    return make_atom(env, "ok");
}

static ERL_NIF_TERM
make_cell(ErlNifEnv* env, sqlite3_stmt* stmt, int col)
{
    int bytes = 0;
    switch (sqlite3_column_type(stmt, col)) {
        case SQLITE_INTEGER:
            return enif_make_int64(env, sqlite3_column_int64(stmt, col));
        case SQLITE_FLOAT:
            return enif_make_double(env, sqlite3_column_double(stmt, col));
        case SQLITE_TEXT:
            bytes = sqlite3_column_bytes(stmt, col);
            return make_binary(env, sqlite3_column_text(stmt, col), bytes);
        case SQLITE_BLOB:
            bytes = sqlite3_column_bytes(stmt, col);
            return make_binary(env, sqlite3_column_blob(stmt, col), bytes);
        case SQLITE_NULL:
            return make_atom(env, "nil");
        default:
            return make_error_tuple(env, make_atom(env, "internal_error"));
    }
}

static ERL_NIF_TERM
make_row(ErlNifEnv* env, sqlite3_stmt* stmt, size_t sz)
{
    ERL_NIF_TERM terms[sz];

    for (int i = 0; i < sz; i++)
        terms[i] = make_cell(env, stmt, i);

    ERL_NIF_TERM ret = enif_make_tuple_from_array(env, terms, sz);

    return ret;
}

static ERL_NIF_TERM
impl_sqlite3_step(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    size_t sz = 0;

    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    int rv = sqlite3_step(rstmt->stmt);

    switch (rv) {
        case SQLITE_DONE:
            return make_atom(env, "ok");
        case SQLITE_ROW:
            sz = sqlite3_column_count(rstmt->stmt);
            if (sz > 0) {
                return make_row(env, rstmt->stmt, sz);
            } else {
                return enif_make_tuple(env, 0);
            }

        default:
            return make_error_tuple(env, enif_make_int(env, rv));
    }
}

static ERL_NIF_TERM
impl_sqlite3_reset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    return enif_make_int(env, sqlite3_reset(rstmt->stmt));
}

static ERL_NIF_TERM
impl_sqlite3_clear_bindings(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    return enif_make_int(env, sqlite3_clear_bindings(rstmt->stmt));
}

static ERL_NIF_TERM
impl_sqlite3_finalize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    int rv = sqlite3_finalize(rstmt->stmt);
    rstmt->stmt = NULL;

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_data_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    return enif_make_int(env, sqlite3_data_count(rstmt->stmt));
}

static ERL_NIF_TERM
impl_sqlite3_sql(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    const char* sql = sqlite3_sql(rstmt->stmt);
    if (!sql)
        return make_binary(env, "", 0);

    return make_binary(env, sql, strlen(sql));
}

static ERL_NIF_TERM
impl_sqlite3_expanded_sql(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    char* sql = sqlite3_expanded_sql(rstmt->stmt);
    if (!sql)
        return make_binary(env, "", 0);

    ERL_NIF_TERM ret = make_binary(env, sql, strlen(sql));
    sqlite3_free(sql);

    return ret;
}

static ERL_NIF_TERM
impl_sqlite3_normalized_sql(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    const char* sql = sqlite3_normalized_sql(rstmt->stmt);
    if (!sql)
        return make_binary(env, "", 0);

    return make_binary(env, sql, strlen(sql));
}

static ERL_NIF_TERM
impl_sqlite3_stmt_busy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    return enif_make_int(env, sqlite3_stmt_busy(rstmt->stmt));
}

static ERL_NIF_TERM
impl_sqlite3_stmt_isexplain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    return enif_make_int(env, sqlite3_stmt_isexplain(rstmt->stmt));
}

static ERL_NIF_TERM
impl_sqlite3_stmt_readonly(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    return enif_make_int(env, sqlite3_stmt_readonly(rstmt->stmt));
}

static ERL_NIF_TERM
impl_sqlite3_stmt_status(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 3);

    sqlite3_stmt_t* rstmt = NULL;
    GET_STMT(env, argv[0], &rstmt);

    int stmt_status, reset_flag;
    GET_INT(env, argv[1], &stmt_status);
    GET_INT(env, argv[2], &reset_flag);

    int rv = sqlite3_stmt_status(rstmt->stmt, stmt_status, reset_flag);

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_backup_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 4);

    sqlite3_t* rdst = NULL;
    GET_DB(env, argv[0], &rdst);

    ErlNifBinary dst_name;
    GET_C_STR(env, argv[1], &dst_name);

    sqlite3_t* rsrc = NULL;
    GET_DB(env, argv[2], &rsrc);

    ErlNifBinary src_name;
    GET_C_STR(env, argv[3], &src_name);

    sqlite3_backup* backup =
      sqlite3_backup_init(rdst->db, (char*)dst_name.data, rsrc->db, (char*)src_name.data);

    sqlite3_backup_t* rbackup =
      enif_alloc_resource(sqlite3_backup_r, sizeof(sqlite3_backup_t));
    if (!rbackup) {
        sqlite3_backup_finish(backup);
        return enif_raise_exception(env, make_atom(env, "alloc_resource"));
    }

    rbackup->backup = backup;

    ERL_NIF_TERM ret = enif_make_resource(env, rbackup);
    enif_release_resource(rbackup);

    return make_ok_tuple(env, ret);
}

static ERL_NIF_TERM
impl_sqlite3_backup_step(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 2);

    sqlite3_backup_t* rbackup = NULL;
    GET_BACKUP(env, argv[0], &rbackup);
    int n;
    GET_INT(env, argv[1], &n);

    return enif_make_int(env, sqlite3_backup_step(rbackup->backup, n));
}

static ERL_NIF_TERM
impl_sqlite3_backup_finish(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_backup_t* rbackup = NULL;
    GET_BACKUP(env, argv[0], &rbackup);
    int rv = sqlite3_backup_finish(rbackup->backup);
    rbackup->backup = NULL;
    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_backup_remaining(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_backup_t* rbackup = NULL;
    GET_BACKUP(env, argv[0], &rbackup);

    return enif_make_int(env, sqlite3_backup_remaining(rbackup->backup));
}

static ERL_NIF_TERM
impl_sqlite3_blob_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 6);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    ErlNifBinary db_name, tab_name, column_name;
    GET_C_STR(env, argv[1], &db_name);
    GET_C_STR(env, argv[2], &tab_name);
    GET_C_STR(env, argv[3], &column_name);

    ErlNifSInt64 rowid;
    if (!enif_get_int64(env, argv[4], &rowid))
        return enif_make_badarg(env);

    int flags;
    GET_INT(env, argv[5], &flags);

    sqlite3_blob* blob = NULL;
    int rv = sqlite3_blob_open(rdb->db,
                               (char*)db_name.data,
                               (char*)tab_name.data,
                               (char*)column_name.data,
                               rowid,
                               flags,
                               &blob);

    if (rv != SQLITE_OK)
        return make_error_tuple(env, enif_make_int(env, rv));

    sqlite3_blob_t* rblob = enif_alloc_resource(sqlite3_blob_r, sizeof(sqlite3_blob_t));

    if (!rblob) {
        sqlite3_blob_close(blob);
        return enif_raise_exception(env, make_atom(env, "alloc_resource"));
    }

    rblob->blob = blob;

    ERL_NIF_TERM ret = enif_make_resource(env, rblob);
    enif_release_resource(rblob);

    return make_ok_tuple(env, ret);
}

static ERL_NIF_TERM
impl_sqlite3_blob_bytes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);
    sqlite3_blob_t* rblob;
    GET_BLOB(env, argv[0], &rblob);

    return enif_make_int(env, sqlite3_blob_bytes(rblob->blob));
}

static ERL_NIF_TERM
impl_sqlite3_blob_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);
    sqlite3_blob_t* rblob;
    GET_BLOB(env, argv[0], &rblob);

    return enif_make_int(env, sqlite3_blob_close(rblob->blob));
}

static ERL_NIF_TERM
impl_sqlite3_blob_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 3);

    sqlite3_blob_t* rblob;
    GET_BLOB(env, argv[0], &rblob);

    int n, offset;
    GET_INT(env, argv[1], &n);
    GET_INT(env, argv[2], &offset);

    ErlNifBinary bin;

    if (!enif_alloc_binary(n, &bin))
        return make_atom(env, "allocation_error");

    int rv = sqlite3_blob_read(rblob->blob, bin.data, n, offset);
    if (rv != SQLITE_OK)
        return make_error_tuple(env, enif_make_int(env, rv));

    ERL_NIF_TERM ret = enif_make_binary(env, &bin);
    enif_release_binary(&bin);

    return make_ok_tuple(env, ret);
}

static ERL_NIF_TERM
impl_sqlite3_blob_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 3);

    sqlite3_blob_t* rblob;
    GET_BLOB(env, argv[0], &rblob);

    ErlNifBinary bin;
    if (!iodata_to_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    int offset;
    GET_INT(env, argv[2], &offset);

    int rv = sqlite3_blob_write(rblob->blob, bin.data, bin.size, offset);
    if (rv != SQLITE_OK)
        return make_error_tuple(env, enif_make_int(env, rv));

    return make_atom(env, "ok");
}

static ERL_NIF_TERM
impl_sqlite3_blob_reopen(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 2);

    sqlite3_blob_t* rblob;
    GET_BLOB(env, argv[0], &rblob);

    ErlNifSInt64 rowid;
    if (!enif_get_int64(env, argv[1], &rowid))
        return enif_make_badarg(env);

    int rv = sqlite3_blob_reopen(rblob->blob, rowid);
    if (rv != SQLITE_OK)
        return make_error_tuple(env, enif_make_int(env, rv));

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_backup_pagecount(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_backup_t* rbackup = NULL;
    GET_BACKUP(env, argv[0], &rbackup);

    return enif_make_int(env, sqlite3_backup_pagecount(rbackup->backup));
}

static ERL_NIF_TERM
impl_sqlite3_errcode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    return enif_make_int(env, sqlite3_errcode(rdb->db));
}

static ERL_NIF_TERM
impl_sqlite3_extended_errcode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    return enif_make_int(env, sqlite3_extended_errcode(rdb->db));
}

static ERL_NIF_TERM
impl_sqlite3_errmsg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    const char* msg = sqlite3_errmsg(rdb->db);

    return make_binary(env, msg, strlen(msg));
}

static ERL_NIF_TERM
impl_sqlite3_errstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    int code;
    GET_INT(env, argv[0], &code);

    const char* msg = sqlite3_errstr(code);

    return make_binary(env, msg, strlen(msg));
}

static ERL_NIF_TERM
impl_sqlite3_wal_autocheckpoint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 2);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    int n;
    GET_INT(env, argv[1], &n);

    return enif_make_int(env, sqlite3_wal_autocheckpoint(rdb->db, n));
}

static ERL_NIF_TERM
impl_sqlite3_wal_checkpoint_v2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 3);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    ErlNifBinary db_name;
    GET_C_STR(env, argv[1], &db_name);

    int mode;
    GET_INT(env, argv[2], &mode);

    int log_size = 0, total_frames = 0;
    int rv = sqlite3_wal_checkpoint_v2(
      rdb->db, (char*)db_name.data, mode, &log_size, &total_frames);

    if (rv != SQLITE_OK)
        return make_error_tuple(env, enif_make_int(env, rv));

    ERL_NIF_TERM t =
      enif_make_tuple2(env, enif_make_int(env, log_size), enif_make_int(env, total_frames));

    return make_ok_tuple(env, t);
}

static ERL_NIF_TERM
impl_sqlite3_compileoption_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    int opt;
    GET_INT(env, argv[0], &opt);

    const char* opt_val = sqlite3_compileoption_get(opt);
    if (opt_val != NULL)
        return make_binary(env, opt_val, strlen(opt_val));
    else
        return make_atom(env, "nil");
}

static ERL_NIF_TERM
impl_sqlite3_compileoption_used(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    ErlNifBinary bin;
    GET_C_STR(env, argv[0], &bin);

    int rv = sqlite3_compileoption_used((char*)bin.data);

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_libversion(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 0);

    const char* libversion = sqlite3_libversion();
    return make_binary(env, libversion, strlen(libversion));
}

static ERL_NIF_TERM
impl_sqlite3_sourceid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 0);

    const char* sourceid = sqlite3_sourceid();
    return make_binary(env, sourceid, strlen(sourceid));
}

static ERL_NIF_TERM
impl_sqlite3_libversion_number(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 0);

    return enif_make_int(env, sqlite3_libversion_number());
}

static ERL_NIF_TERM
impl_sqlite3_complete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    ErlNifBinary sql;
    GET_C_STR(env, argv[0], &sql);

    return enif_make_int(env, sqlite3_complete((char*)sql.data));
}

static ERL_NIF_TERM
impl_sqlite3_snapshot_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 2);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    ErlNifBinary db_name;
    GET_C_STR(env, argv[1], &db_name);

    sqlite3_snapshot* snapshot = NULL;
    int rv = sqlite3_snapshot_get(rdb->db, (char*)db_name.data, &snapshot);
    if (rv != SQLITE_OK)
        return make_error_tuple(env, enif_make_int(env, rv));

    sqlite3_snapshot_t* rsnapshot =
      enif_alloc_resource(sqlite3_snapshot_r, sizeof(sqlite3_snapshot_t));
    if (!rsnapshot) {
        sqlite3_snapshot_free(snapshot);
        return enif_raise_exception(env, make_atom(env, "alloc_resource"));
    }

    rsnapshot->snapshot = snapshot;

    ERL_NIF_TERM ret = enif_make_resource(env, rsnapshot);
    enif_release_resource(rsnapshot);

    return make_ok_tuple(env, ret);
}

static ERL_NIF_TERM
impl_sqlite3_snapshot_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 3);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    ErlNifBinary db_name;
    GET_C_STR(env, argv[1], &db_name);

    sqlite3_snapshot_t* rsnapshot = NULL;
    if (!enif_get_resource(env, argv[2], sqlite3_snapshot_r, (void**)&rsnapshot))
        return enif_make_badarg(env);

    int rv = sqlite3_snapshot_open(rdb->db, (char*)db_name.data, rsnapshot->snapshot);

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_snapshot_cmp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 2);

    sqlite3_snapshot_t* rsnapshot1 = NULL;
    if (!enif_get_resource(env, argv[0], sqlite3_snapshot_r, (void**)&rsnapshot1))
        return enif_make_badarg(env);

    sqlite3_snapshot_t* rsnapshot2 = NULL;
    if (!enif_get_resource(env, argv[1], sqlite3_snapshot_r, (void**)&rsnapshot2))
        return enif_make_badarg(env);

    int rv = sqlite3_snapshot_cmp(rsnapshot1->snapshot, rsnapshot2->snapshot);

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_snapshot_free(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 1);

    sqlite3_snapshot_t* rsnapshot = NULL;
    if (!enif_get_resource(env, argv[0], sqlite3_snapshot_r, (void**)&rsnapshot))
        return enif_make_badarg(env);

    sqlite3_snapshot_free(rsnapshot->snapshot);
    rsnapshot->snapshot = NULL;

    return make_atom(env, "ok");
}

static ERL_NIF_TERM
impl_sqlite3_snapshot_recover(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 2);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    ErlNifBinary db_name;
    GET_C_STR(env, argv[1], &db_name);

    int rv = sqlite3_snapshot_recover(rdb->db, (char*)db_name.data);

    return enif_make_int(env, rv);
}

static ERL_NIF_TERM
impl_sqlite3_serialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 2);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    ErlNifBinary db_name;
    GET_C_STR(env, argv[1], &db_name);

    sqlite3_int64 size = 0;
    unsigned char* s = sqlite3_serialize(rdb->db, (char*)db_name.data, &size, 0);

    if (!s)
        return make_error_tuple(env, make_atom(env, "alloc_memory"));

    ERL_NIF_TERM ret = make_binary(env, s, size);

    return make_ok_tuple(env, ret);
}

static ERL_NIF_TERM
impl_sqlite3_deserialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGC(argc, 3);

    sqlite3_t* rdb = NULL;
    GET_DB(env, argv[0], &rdb);

    ErlNifBinary db_name;
    GET_C_STR(env, argv[1], &db_name);

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[2], &bin))
        return enif_make_badarg(env);

    int rv = sqlite3_deserialize(rdb->db,
                                 (char*)db_name.data,
                                 (unsigned char*)bin.data,
                                 bin.size,
                                 bin.size,
                                 SQLITE_DESERIALIZE_READONLY);

    return enif_make_int(env, rv);
}

#define DIRTY_IO ERL_NIF_DIRTY_JOB_IO_BOUND
static ErlNifFunc nif_funcs[] = {
    { "sqlite3_open_v2", 3, impl_sqlite3_open_v2, DIRTY_IO },
    { "sqlite3_close_v2", 1, impl_sqlite3_close_v2, DIRTY_IO },
    { "sqlite3_limit", 3, impl_sqlite3_limit, 0 },
    { "sqlite3_load_extension", 3, impl_sqlite3_load_extension, DIRTY_IO },
    { "sqlite3_db_cacheflush", 1, impl_sqlite3_db_cacheflush, DIRTY_IO },
    { "sqlite3_db_release_memory", 1, impl_sqlite3_db_release_memory, 0 },
    { "sqlite3_db_config", 3, impl_sqlite3_db_config, 0 },
    { "sqlite3_set_last_insert_rowid", 2, impl_sqlite3_set_last_insert_rowid, 0 },
    { "sqlite3_busy_timeout", 2, impl_sqlite3_busy_timeout, 0 },
    { "sqlite3_extended_result_codes", 2, impl_sqlite3_extended_result_codes, 0 },
    { "sqlite3_interrupt", 1, impl_sqlite3_interrupt, DIRTY_IO },
    { "sqlite3_get_autocommit", 1, impl_sqlite3_get_autocommit, 0 },
    { "sqlite3_last_insert_rowid", 1, impl_sqlite3_last_insert_rowid, 0 },
    { "sqlite3_changes", 1, impl_sqlite3_changes, 0 },
    { "sqlite3_total_changes", 1, impl_sqlite3_total_changes, 0 },
    { "sqlite3_db_filename", 2, impl_sqlite3_db_filename, 0 },
    { "sqlite3_db_readonly", 2, impl_sqlite3_db_readonly, 0 },
    { "sqlite3_db_status", 3, impl_sqlite3_db_status, 0 },
    { "sqlite3_prepare_v2", 2, impl_sqlite3_prepare_v2, DIRTY_IO },
    { "sqlite3_bind", 2, impl_sqlite3_bind, 0 },
    { "sqlite3_step", 1, impl_sqlite3_step, DIRTY_IO },
    { "sqlite3_reset", 1, impl_sqlite3_reset, 0 },
    { "sqlite3_clear_bindings", 1, impl_sqlite3_clear_bindings, 0 },
    { "sqlite3_finalize", 1, impl_sqlite3_finalize, 0 },
    { "sqlite3_data_count", 1, impl_sqlite3_data_count, DIRTY_IO },
    { "sqlite3_sql", 1, impl_sqlite3_sql, 0 },
    { "sqlite3_expanded_sql", 1, impl_sqlite3_expanded_sql, 0 },
    { "sqlite3_normalized_sql", 1, impl_sqlite3_normalized_sql, 0 },
    { "sqlite3_stmt_busy", 1, impl_sqlite3_stmt_busy, 0 },
    { "sqlite3_stmt_isexplain", 1, impl_sqlite3_stmt_isexplain, 0 },
    { "sqlite3_stmt_readonly", 1, impl_sqlite3_stmt_readonly, 0 },
    { "sqlite3_stmt_status", 3, impl_sqlite3_stmt_status, 0 },
    { "sqlite3_backup_init", 4, impl_sqlite3_backup_init, DIRTY_IO },
    { "sqlite3_backup_step", 2, impl_sqlite3_backup_step, DIRTY_IO },
    { "sqlite3_backup_finish", 1, impl_sqlite3_backup_finish, DIRTY_IO },
    { "sqlite3_backup_pagecount", 1, impl_sqlite3_backup_pagecount, 0 },
    { "sqlite3_backup_remaining", 1, impl_sqlite3_backup_remaining, 0 },
    { "sqlite3_blob_open", 6, impl_sqlite3_blob_open, DIRTY_IO },
    { "sqlite3_blob_close", 1, impl_sqlite3_blob_close, DIRTY_IO },
    { "sqlite3_blob_read", 3, impl_sqlite3_blob_read, DIRTY_IO },
    { "sqlite3_blob_write", 3, impl_sqlite3_blob_write, DIRTY_IO },
    { "sqlite3_blob_bytes", 1, impl_sqlite3_blob_bytes, DIRTY_IO },
    { "sqlite3_blob_reopen", 2, impl_sqlite3_blob_reopen, DIRTY_IO },
    { "sqlite3_errcode", 1, impl_sqlite3_errcode, 0 },
    { "sqlite3_extended_errcode", 1, impl_sqlite3_extended_errcode, 0 },
    { "sqlite3_errmsg", 1, impl_sqlite3_errmsg, 0 },
    { "sqlite3_errstr", 1, impl_sqlite3_errstr, 0 },
    { "sqlite3_wal_autocheckpoint", 2, impl_sqlite3_wal_autocheckpoint, 0 },
    { "sqlite3_wal_checkpoint_v2", 3, impl_sqlite3_wal_checkpoint_v2, DIRTY_IO },
    { "sqlite3_compileoption_get", 1, impl_sqlite3_compileoption_get, 0 },
    { "sqlite3_compileoption_used", 1, impl_sqlite3_compileoption_used, 0 },
    { "sqlite3_libversion", 0, impl_sqlite3_libversion, 0 },
    { "sqlite3_sourceid", 0, impl_sqlite3_sourceid, 0 },
    { "sqlite3_libversion_number", 0, impl_sqlite3_libversion_number, 0 },
    { "sqlite3_complete", 1, impl_sqlite3_complete, 0 },
    { "sqlite3_snapshot_get", 2, impl_sqlite3_snapshot_get, DIRTY_IO },
    { "sqlite3_snapshot_open", 3, impl_sqlite3_snapshot_open, DIRTY_IO },
    { "sqlite3_snapshot_cmp", 2, impl_sqlite3_snapshot_cmp, DIRTY_IO },
    { "sqlite3_snapshot_recover", 2, impl_sqlite3_snapshot_recover, DIRTY_IO },
    { "sqlite3_snapshot_free", 1, impl_sqlite3_snapshot_free, DIRTY_IO },
    { "sqlite3_serialize", 2, impl_sqlite3_serialize, DIRTY_IO },
    { "sqlite3_deserialize", 3, impl_sqlite3_deserialize, DIRTY_IO }
};

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    assert(sqlite3_libversion_number() == SQLITE_VERSION_NUMBER);
    assert(strncmp(sqlite3_sourceid(), SQLITE_SOURCE_ID, 80) == 0);
    assert(strcmp(sqlite3_libversion(), SQLITE_VERSION) == 0);

    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    sqlite3_r =
      enif_open_resource_type(env, NULL, "sqlite3_r", sqlite3_r_dtor, flags, NULL);

    sqlite3_stmt_r = enif_open_resource_type(
      env, NULL, "sqlite3_stmt_r", sqlite3_stmt_r_dtor, flags, NULL);

    sqlite3_blob_r = enif_open_resource_type(
      env, NULL, "sqlite3_blob_r", sqlite3_blob_r_dtor, flags, NULL);

    sqlite3_backup_r = enif_open_resource_type(
      env, NULL, "sqlite3_backup_r", sqlite3_backup_r_dtor, flags, NULL);

    sqlite3_snapshot_r = enif_open_resource_type(
      env, NULL, "sqlite3_snapshot_r", sqlite3_snapshot_r_dtor, flags, NULL);

    return sqlite3_initialize();
}

static int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return sqlite3_initialize();
}

static void
unload(ErlNifEnv* env, void* priv_data)
{
    sqlite3_shutdown();
}

ERL_NIF_INIT(sqlite3_nif, nif_funcs, load, NULL, upgrade, unload);

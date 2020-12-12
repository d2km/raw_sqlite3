raw_sqlite3
=====

`raw_sqlite3` is a thin, low-level NIF wrapper for [SQLite database](https://sqlite.org/index.html) C API.

The main motivation for the library is to have a low-level wrapper which
exposes as much as of SQLite C API as possible, excluding anything that is
outdated, dangerous, or just makes little or no sense for using in Erlang code.

One of the design goals was to have many, potentially tens of thousands DB
connections in one Erlang node. To achieve this, the library avoids spawning
extra OS threads by making relevant functions to run on dirty schedulers.

While the library is believed to be safe to use (including automatic object
de-allocation), it might still be possible to crash the VM because there is no
built-in validation for passed parameters (e.g. nothing will prevent you from
passing `SQLITE_OPEN_NOMUTEX` flag which will likely crash the VM).

With great power comes great responsibility. Use with caution!

The `sqlite3_nif` module provides wrappers for the every SQLite C API function,
except:

 * Deprecated functions;
 * Potentially dangerous, global state mutating functions, such as `sqlite3_config`;
 * Functions which require a function pointer;
 * `sqlite3_bind_*` functions which are replaced by single `sqlite3_bind/2`;
 * `sqlite3_column_*` functions which are merged into `sqlite3_step/1`;
 * `sqlite3_mutex_*` functions which make no sense in Erlang code;
 * API related to user functions creation;
 * Unicode variants of the C API (only UTF-8 versions are allowed);
 * Memory managing functions;
 * `_vfs_*` functions;
 * ...

The `raw_sqlite3` module contains high-level wrappers for common usage
patterns.  See [TUTORIAL](TUTORIAL.md) for some examples. EDoc-generated
documentation available at [https://hexdocs.pm/raw_sqlite3](https://hexdocs.pm/raw_sqlite3/).

Build
-----

    $ rebar3 compile

Test
----

    $ rebar3 eunit

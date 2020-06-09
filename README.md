raw_sqlite3
=====

The library provides a NIF interface to `sqlite3`.

The motivation behind the library is to have a low-level wrapper which exposes
as much as of SQLite C API as possible, excluding anything that is outdated,
dangerous, or just makes little or no sense to use in Erlang code.

While it is believed to be safe to use (including automatic object
de-allocation), it might still be possible to crash the VM because there is no
built-in validity checks for passed parameters. So use with caution!

Hopefully, other libraries may utilise `raw_sqlite3` to build a safer,
higher-level interface.

As opposed to [esqlite3](https://github.com/mmzeeman/esqlite) the NIF does not
spawn any extra OS threads, since most of the NIF functions are marked as
IO-heavy to run on dirty schedulers. While it works fine most of the time, some
edge cases are not impossible.

Interface
----

The `sqlite3_nif` module is assumed as the main entry point for the library. It provides
wrappers for the every SQLite C API function, except:

 * Deprecated functions;
 * Potentially dangerous, global state mutating functions, such as `sqlite3_config`;
 * Functions which require a function pointer;
 * `sqlite3_bind_*` functions which are replaced by single `sqlite3_bind/2`;
 * `sqlite3_column_*` functions which are merged into `sqlite3_step/1`;
 * `sqlite3_mutex_*` functions which make no sense in Erlang code;
 * API related to creating of user functions;
 * Unicode variants of the C API (only UTF-8 version are allowed);
 * Memory managing functions;
 * `_vfs_*` functions;
 * ...


Most of the exposed functions follow the C API to the letter so [the official
documentation](https://sqlite.org/c3ref/funclist.html) should be considered as
the primary source of information, while [the C API
introduction](https://sqlite.org/cintro.html) should be read as a "getting
started guide".

File `include/sqlite3_nif.hrl` defines macros for C constants.

The `raw_sqlite3` module contains a few convenience and utility functions to
convert between C constants and Erlang atoms, as well as provide human-readable
error messages.


Build
-----

    $ rebar3 compile

Test
----

    $ rebar3 eunit

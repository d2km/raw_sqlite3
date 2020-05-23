# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT := sqlite3_nif

# The flag combination may or may not make sense for your application.
#  YMMV, feel free to change if required.
SQLITE_CFLAGS = -DSQLITE_DOS=0 \
		-DSQLITE_THREADSAFE=1 \
		-DSQLITE_DEFAULT_MEMSTATUS=0 \
		-DSQLITE_DEFAULT_WAL_SYNCHRONOUS=1 \
		-DSQLITE_LIKE_DOESNT_MATCH_BLOBS \
		-DSQLITE_OMIT_DEPRECATED \
		-DSQLITE_OMIT_PROGRESS_CALLBACK \
		-DSQLITE_OMIT_AUTOINIT \
		-DHAVE_USLEEP \
		-DHAVE_GMTIME_R \
		-DHAVE_LOCALTIME_R \
		-DSQLITE_USE_URI \
		-DSQLITE_ENABLE_JSON1 \
		-DSQLITE_OMIT_GET_TABLE \
		-DSQLITE_ENABLE_NORMALIZE \
		-DSQLITE_OMIT_DEPRECATED \
		-I$(CURDIR)/sqlite3

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT).so

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O2 -std=c99 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes -pthread
	LDFLAGS ?= -arch x86_64  -flat_namespace  -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O2 -std=c99 -finline-functions -Wall -Wmissing-prototypes
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O2 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	LDFLAGS ?= -pthread
endif

CFLAGS += -fPIC -I$(CURDIR) -DDEBUG -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) $(SQLITE_CFLAGS)
CXXFLAGS += -fPIC -I$(CURDIR) -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei -ldl
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

$(C_SRC_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS)

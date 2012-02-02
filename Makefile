MODULES = enumset

EXTENSION = enumset
DATA = enumset--1.0.sql enumset--unpackaged--1.0.sql

REGRESS = enumset

ifdef USE_PGXS
PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
else
subdir = contrib/enumset
top_builddir = ../..
include $(top_builddir)/src/Makefile.global
include $(top_srcdir)/contrib/contrib-global.mk
endif

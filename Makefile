# Copyright (c) 2018 Simon Hudon. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Simon Hudon, Sebastian Ullrich, Leonardo de Moura
LEAN ?= lean
LEANC ?= leanc
# Even though we copy the sources into a new directory for stage2/3, we
# read the list of files to compile from the original directory to avoid
# issues with stale copied files
SRCS = $(shell find . -name '*.lean' | grep -v '\#')
DEPS = $(SRCS:.lean=.depend)
OPTS = 
OBJS = $(SRCS:.lean=.olean)
OBJS2 = $(SRCS:.lean=.o)
CS = $(SRCS:.lean=.c)

SHELL = /usr/bin/env bash -eo pipefail

.PHONY: all clean

all: $(OBJS) import

depends: $(DEPS)

%.depend: %.lean
# use separate assignment to ensure failure propagation
	@deps=`$(LEAN) --deps $< | python relative.py`; echo $(<:.lean=.olean): $$deps > $@

%.olean: %.lean %.depend $(MORE_DEPS)
	@echo "[    ] Building $<"
	@mkdir -p $(*D)
	env LEAN_PATH=import=$(PWD) \
	$(LEAN) $(OPTS) --make --c="$*.c.tmp" $<
# create the .c file atomically
	mv "$*.c.tmp" "$*.c"
# make sure the .olean file is newer than the .depend file to prevent infinite make cycles
	@touch $@

%.c: %.olean
	@

LEANC_OPTS = -O3

%.o: %.c
	@echo "[    ] Building $<"
	@mkdir -p "$(@D)"
	$(LEANC) -c -o $@ $< $(LEANC_OPTS)

%.S: %.c
	@mkdir -p "$(@D)"
	$(LEANC) -S -o $@ $< $(LEANC_OPTS)

import: $(OBJS2)
	$(LEANC) -o $@ $+ $(LEANC_OPTS)

clean:
	$(RM) $(DEPS) $(OBJS) $(CS) $(OBJS2)

.PRECIOUS: %.depend %.c

include $(DEPS)

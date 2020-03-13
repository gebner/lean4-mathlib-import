# Copyright (c) 2018 Simon Hudon. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Simon Hudon, Sebastian Ullrich, Leonardo de Moura
LEAN ?= lean
LEANC ?= leanc
# Even though we copy the sources into a new directory for stage2/3, we
# read the list of files to compile from the original directory to avoid
# issues with stale copied files
SRCS = ExportParser.lean Import.lean Main.lean OldRecursor.lean
DEPS = $(SRCS:.lean=.depend)
OPTS = 
OBJS = $(SRCS:.lean=.olean)
OBJS2 = $(SRCS:.lean=.o)
CS = $(SRCS:.lean=.c)

export LEAN_PATH = Import=$(PWD)

SHELL = /usr/bin/env bash -eo pipefail

.PHONY: all clean emacs

all: $(OBJS) import Mathlib.olean

depends: $(DEPS)

%.depend: %.lean
# use separate assignment to ensure failure propagation
	deps=`$(LEAN) --deps $< | python relative.py`; echo $(<:.lean=.olean): $$deps > $@

%.olean: %.lean %.depend $(MORE_DEPS)
	mkdir -p $(*D)
	$(LEAN) $(OPTS) --make --c="$*.c.tmp" $<
# create the .c file atomically
	mv "$*.c.tmp" "$*.c"
# make sure the .olean file is newer than the .depend file to prevent infinite make cycles
	touch $@

%.c: %.olean
	@

LEANC_OPTS = -O3

%.o: %.c
	mkdir -p "$(@D)"
	$(LEANC) -c -o $@ $< $(LEANC_OPTS)

%.S: %.c
	mkdir -p "$(@D)"
	$(LEANC) -S -o $@ $< $(LEANC_OPTS)

import: $(OBJS2)
	$(LEANC) -o $@ $+ $(LEANC_OPTS)

LEAN_INIT_PATH=$(shell echo 'import Init.Lean def main (args : List String) : IO UInt32 := do sp <- Lean.getBuiltinSearchPath; IO.println (sp.find! "Init"); pure 0' | lean --run --stdin)

Mathlib.olean: import export.txt attrs.txt
	env LEAN_PATH=$(LEAN_PATH):Init=$(LEAN_INIT_PATH) time -v ./$+ $@

clean:
	$(RM) $(DEPS) $(OBJS) $(CS) $(OBJS2)

export.txt attrs.txt:
	leanproject get mathlib || true
	cd mathlib; bash ./scripts/rm_all.sh || true
	cd mathlib; bash ./scripts/mk_all.sh
	cd mathlib; env -u LEAN_PATH lean --make src/all.lean
	cd mathlib; env -u LEAN_PATH lean src/all.lean --export=../export.txt
	cd mathlib; env -u LEAN_PATH lean ../list_attrs.lean 2>../attrs.txt

emacs:
	emacs

.PRECIOUS: %.depend %.c

include $(DEPS)

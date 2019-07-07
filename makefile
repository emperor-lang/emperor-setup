#!/usr/bin/make

CC = gcc-8
GENERIC_CFLAGS := -Wall -Werror -Wpedantic -pedantic-errors -O3 -I . -g # $(shell python3-config --cflags)
CFLAGS_EXECUTABLE := -fPIC $(GENERIC_CFLAGS)
CFLAGS := -c -fPIC $(GENERIC_CFLAGS)
OUTPUT_FILE := ./emperor-setup
# MAKEFLAGS := $(MAKEFLAGS) s
EXECUTABLE_INSTALL_LOCATION := /usr/bin/emperor-setup
COMPRESSED_MAN_OUTPUT := emperor-setup.1.gz
MAN_OUTPUT := emperor-setup.1.gz
MAN_INSTALL_LOCATION := /usr/share/man/man1/emperor-setup.1.gz
COMPLETION_INSTALL_LOCATION := /usr/share/bash-completion/completions/emperor-setup

.DEFAULT_TARGET = $(OUTPUT_FILE)

$(OUTPUT_FILE): ./emperor-setup.o ./emperor-setup-args.o
	$(CC) $(CFLAGS_EXECUTABLE) $^ -o $@

./emperor-setup.o: ./emperor-setup.c
	$(CC) $(CFLAGS) $^ -o $@

./emperor-setup.c ./emperor-setup.h: ./emperor-setup-args.c;

./emperor-setup-args.o: ./emperor-setup-args.c
	$(CC) $(CFLAGS) $^ -o $@

./emperor-setup-args.c ./emperor-setup-args.h: ./emperor-setup.json;
	arggen_c -i $^ -H ./emperor-setup-args.h -o ./emperor-setup-args.c


$(MAN_OUTPUT) : ./emperor-setup.json
	(mangen | gzip --best) < $^ > $@

install: $(EXECUTABLE_INSTALL_LOCATION) $(MAN_INSTALL_LOCATION) $(COMPLETION_INSTALL_LOCATION)
.PHONY: install

$(EXECUTABLE_INSTALL_LOCATION): $(OUTPUT_FILE)
	sudo install $(OUTPUT_FILE) $(EXECUTABLE_INSTALL_LOCATION)

$(MAN_INSTALL_LOCATION): $(MAN_OUTPUT)
	sudo install -m 0644 $(MAN_OUTPUT) $(MAN_INSTALL_LOCATION)

$(COMPLETION_INSTALL_LOCATION): ./emperor-setup_completions.sh;
	sudo install -m 644 $^ $@

./emperor-setup_completions.sh: ./emperor-setup.json
	argcompgen < $< > $@
.DELETE_ON_ERROR: ./emperor-setup_completions.sh

clean-installation:
	sudo $(RM) $(EXECUTABLE_INSTALL_LOCATION) 	2>/dev/null || true
	sudo $(RM) $(MAN_INSTALL_LOCATION) 			2>/dev/null || true
	sudo $(RM) $(COMPLETION_INSTALL_LOCATION) 	2>/dev/null || true
.PHONY: clean-installation

clean:
	-@$(RM) $(MAN_OUTPUT)			2>/dev/null	|| true
	-@$(RM) $(OUTPUT_FILE)			2>/dev/null	|| true
	-@$(RM) *.o						2>/dev/null	|| true
	-@$(RM) emperor-setup-args.* 	2>/dev/null	|| true
	-@$(RM) *_completions.sh		2>/dev/null || true
.PHONY: clean

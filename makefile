#!/usr/bin/make

CC = gcc-8
CFLAGS := $(CFLAGS) -Wall -Werror -Wpedantic -pedantic-errors -O3 -I . -g
OUTPUT_FILE := ./emperor-setup
MAKEFLAGS := $(MAKEFLAGS) s
EXECUTABLE_INSTALL_LOCATION := /usr/bin/emperor-setup
COMPRESSED_MAN_OUTPUT := emperor-setup.1.gz
MAN_OUTPUT := emperor-setup.1.gz
MAN_INSTALL_LOCATION := /usr/share/man/man1/emperor-setup.1.gz

.DEFAULT_TARGET = $(OUTPUT_FILE)

$(OUTPUT_FILE): ./emperor-setup.c
	$(CC) $(CFLAGS) ./emperor-setup.c -o $(OUTPUT_FILE)

$(CYTHON_OUTPUT): ./mangen.pyx
	$(CYTHON) $(CYTHON_FLAGS) ./mangen.pyx -o $(CYTHON_OUTPUT)

$(MAN_OUTPUT) : ./emperor-setup.spec.json
	(jq -Mc . | mangen - | gzip --best) < $^ > $@

install: $(OUTPUT_FILE) $(MAN_OUTPUT)
	sudo install $(OUTPUT_FILE) $(EXECUTABLE_INSTALL_LOCATION)
	sudo install -m 0644 $(MAN_OUTPUT) $(MAN_INSTALL_LOCATION)

clean:
	-@$(RM) $(MAN_OUTPUT)		2>/dev/null	|| true
	-@$(RM) $(OUTPUT_FILE)		2>/dev/null	|| true
.PHONY: clean

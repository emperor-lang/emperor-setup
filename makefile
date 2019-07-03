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

install: $(OUTPUT_FILE) $(MAN_OUTPUT)
	sudo install $(OUTPUT_FILE) $(EXECUTABLE_INSTALL_LOCATION)
	sudo install -m 0644 $(MAN_OUTPUT) $(MAN_INSTALL_LOCATION)

clean:
	-@$(RM) $(MAN_OUTPUT)			2>/dev/null	|| true
	-@$(RM) $(OUTPUT_FILE)			2>/dev/null	|| true
	-@$(RM) *.o						2>/dev/null	|| true
	-@$(RM) emperor-setup-args.* 	2>/dev/null	|| true
.PHONY: clean

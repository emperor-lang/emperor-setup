#!/usr/bin/make
SHELL := /bin/bash

# CC = gcc-8
# CFLAGS := $(shell emperor-setup --cflags) # $(CFLAGS) -Wall -Os -I . -I /usr/include/python3.6m -g
# CLIBS := $(shell emperor-setup --libs)

# Apply debug options if specified
ifdef DEBUG
PARSER_DEBUG_FLAGS = -d
endif

OPEN := xdg-open

# Code generation commands
SOFT_LINK_COMMAND = [[ ! -f $@ ]] && ln -s $^ $@

# Code up-keep commands
LINTER := hlint
LINTER_FLAGS := -s
FORMATTER := stylish-haskell
FORMATTER_FLAGS := -i -c ./stylish-haskell.yaml

COMPLETION_INSTALL_LOCATION = /usr/share/bash-completion/completions/emperor-setup

.DEFAULT_GOAL := all

all: build ## Build everything
.PHONY: all

build: ./emperor-setup ## Build everything, explicitly
.PHONY: build

./emperor-setup: ./dist/build/emperor-setup/emperor-setup
	@echo "[[ ! -f $@ ]] && ln -s $^ $@"
	$(shell [[ ! -f $@ ]] && ln -s $^ $@)
.DELETE_ON_ERROR: ./emperor-setup

./dist/build/emperor-setup/emperor-setup: ./Args.hs $(shell find . -name '*.hs')
	cabal build

./Args.hs: emperor-setup.json
	arggen_haskell < $^ > $@

%.hs:;

./emperor-setup.json:;

install: /usr/bin/emperor-setup /usr/share/man/man1/emperor-setup.1.gz $(COMPLETION_INSTALL_LOCATION) ## Install binaries, libraries and documentation
.PHONY: install

/usr/bin/emperor-setup: ./dist/build/emperor-setup/emperor-setup
	sudo install -m 755 $^ $@

man: ./dist/doc/man/emperor-setup.1.gz; ## Make the man page
.PHONY: man

/usr/share/man/man1/emperor-setup.1.gz: ./dist/doc/man/emperor-setup.1.gz
	sudo install -m 644 $^ $@

./dist/doc/man/emperor-setup.1.gz: emperor-setup.json
	mkdir -p ./dist/doc/man/ 2>/dev/null || true
	(mangen | gzip --best) < $^ > $@
.DELETE_ON_ERROR: ./dist/doc/man/emperor-setup.1.gz

$(COMPLETION_INSTALL_LOCATION): ./emperor-setup_completions.sh;
	sudo install -m 644 $^ $@

./emperor-setup_completions.sh: ./emperor-setup.json
	argcompgen < $< > $@
.DELETE_ON_ERROR: ./emperor-setup_completions.sh

format: $(shell find . -name '*.hs' | grep -v dist | grep -v Args) ## Run the formatter on all non-generated source files
	@echo $(shell find . -name '*.hs' | grep -v dist | grep -v Args)
	$(FORMATTER) $(FORMATTER_FLAGS) $^
.PHONY: format

lint: ./Args.hs $(shell find . -name '*.hs') ## Run the linter on all non-generated source files
	$(LINTER) $(LINTER_FLAGS) $^
.PHONY: lint

doc: dist/doc/html/emperor-setup/emperor-setup/index.html ## Make the documentation
.PHONY: doc

open-doc: dist/doc/html/emperor-setup/emperor-setup/index.html ## Open the documentationin the default browser
	$(OPEN) $<
.PHONY: open-doc

dist/doc/html/emperor-setup/emperor-setup/index.html: $(SOURCE_FILES)
	cabal haddock --executables

clean-installation: ## Remove installed executables, libraries and documentation
	sudo $(RM) /usr/bin/emperor-setup
	sudo $(RM) /usr/share/man/man1/emperor-setup.1.gz
	sudo $(RM) /usr/share/bash-completion/completions/emperor-setup 2>/dev/null || true
.PHONY: clean-installation

clean: ## Delete all generated files
	cabal clean --verbose=0
	$(RM) cabal.config Args.hs *_completions.sh ./emperor-setup $(shell find . -name '*.orig') $(shell find . -name '*.info') $(shell find . -name '*.hi') *.eh*
.PHONY: clean

# Thanks, François Zaninotto! https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
help: ## Output this help summary
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help

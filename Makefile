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

./emperor-setup: ./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/bin/emperor-setup
	ln -sf $^ $@
.DELETE_ON_ERROR: ./emperor-setup

./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/bin/emperor-setup: ./Args.hs $(shell find . -name '*.hs')
	stack build

./Args.hs: emperor-setup.json
	arggen_haskell < $^ > $@

%.hs:;

./emperor-setup.json:;

install: /usr/bin/emperor-setup /usr/share/man/man1/emperor-setup.1.gz $(COMPLETION_INSTALL_LOCATION) ## Install binaries, libraries and documentation
.PHONY: install

/usr/bin/emperor-setup: ./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/bin/emperor-setup
	sudo install -m 755 $^ $@

man: ./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/doc/emperor-setup-0.1.0.0/emperor-setup.1.gz; ## Make the man page
.PHONY: man

/usr/share/man/man1/emperor-setup.1.gz: ./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/doc/emperor-setup-0.1.0.0/emperor-setup.1.gz
	sudo install -m 644 $^ $@

./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/doc/emperor-setup-0.1.0.0/man/emperor-setup.1.gz: emperor-setup.json
	mkdir -p ./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/doc/emperor-setup-0.1.0.0/man/ 2>/dev/null || true
	(mangen | gzip --best) < $^ > $@
.DELETE_ON_ERROR: ./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/doc/emperor-setup-0.1.0.0/man/emperor-setup.1.gz

$(COMPLETION_INSTALL_LOCATION): ./emperor-setup_completions.sh;
	sudo install -m 644 $^ $@

./emperor-setup_completions.sh: ./emperor-setup.json
	argcompgen < $< > $@
.DELETE_ON_ERROR: ./emperor-setup_completions.sh

format: $(shell find . -name '*.hs' | grep -v .stack-work | grep -v Args) ## Run the formatter on all non-generated source files
	@echo $(shell find . -name '*.hs' | grep -v .stack-work | grep -v Args)
	$(FORMATTER) $(FORMATTER_FLAGS) $^
.PHONY: format

lint: ./Args.hs $(shell find . -name '*.hs') ## Run the linter on all non-generated source files
	$(LINTER) $(LINTER_FLAGS) $^
.PHONY: lint

doc: ./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/doc/emperor-setup-0.1.0.0/html/index.html ## Make the documentation
.PHONY: doc

open-doc: ./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/doc/emperor-setup-0.1.0.0/html/index.html ## Open the documentationin the default browser
	$(OPEN) $<
.PHONY: open-doc

./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/doc/emperor-setup-0.1.0.0/html/index.html: $(shell find . -name '*.hs') ./Args.hs

./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/doc/emperor-setup-0.1.0.0/html/index.html: $(SOURCE_FILES)
	stack haddock

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

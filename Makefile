## Makefile

all:

REPO_USER    := conao3
PACKAGE_NAME := keg
REPO_NAME    := keg.el

EMACS        ?= emacs

##################################################

.PHONY: all help build test clean

all: help

help:
	$(info )
	$(info Commands)
	$(info ========)
	$(info   - make          # Show this help)
	$(info   - make build    # Compile Elisp files)
	$(info   - make test     # Compile Elisp files and test $(PACKAGE_NAME))
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info   - make clean    # Clean compiled files)
	$(info )
	$(info This Makefile required `keg`)
	$(info See https://github.com/$(REPO_USER)/$(REPO_NAME)#contribution)
	$(info )

##############################

build:
	keg build

test: build
	keg exec $(EMACS) --batch -l $(PACKAGE_NAME)-tests.el -f cort-test-run

clean:
	keg clean-elc
	rm -rf .keg

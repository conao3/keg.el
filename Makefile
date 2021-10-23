## Makefile

all:

REPO_USER    := conao3
PACKAGE_NAME := keg
REPO_NAME    := keg.el

EMACS        ?= emacs

##################################################

.PHONY: all help build test lint clean

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

test: test-keg test-keg-mode test-flycheck-keg

test-keg:
	KEGINSTALLPACKAGES="keg" keg build keg
	KEGINSTALLPACKAGES="keg" keg exec $(EMACS) --batch -l $(PACKAGE_NAME)-tests.el -f cort-test-run

test-keg-mode:
	KEGINSTALLPACKAGES="keg-mode" keg build keg-mode

test-flycheck-keg:
	KEGINSTALLPACKAGES="flycheck-keg" keg build flycheck-keg

lint:
	keg lint

clean:
	keg clean

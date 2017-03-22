SHELL = /bin/bash
.RECIPEPREFIX = >
git_repo_root ?= $(PWD)/..
local_bin_path ?= $(HOME)/.local/bin

dev-all : check int-test-basic

ci-all : check
# TODO: add stack exec to run shell tests
> stack --install-ghc --resolver nightly build --test --only-dependencies
> stack --resolver nightly build --test --haddock --no-haddock-deps
> stack --install-ghc --resolver lts-8 build --test --only-dependencies
> stack --resolver lts-8 build --test --haddock --no-haddock-deps
> stack --install-ghc --resolver lts-7 build --test --only-dependencies
> stack --resolver lts-7 build --test --haddock --no-haddock-deps
> stack --install-ghc --resolver lts-6 build --test --only-dependencies
> stack --resolver lts-6 build --test --haddock --no-haddock-deps
# lts3 passes, lts2?

install : check
> stack --resolver nightly --local-bin-path="$(local_bin_path)" $(STACK_ARGS) build --copy-bins

int-test-full :

int-test-basic :

check : build
> stack --resolver nightly build --test

build :
> stack --install-ghc --resolver nightly build --test --only-dependencies
> stack --resolver nightly build --haddock --no-haddock-deps

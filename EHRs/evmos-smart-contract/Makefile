#!/usr/bin/make -f

BINDIR ?= $(GOPATH)/bin
BUILDDIR ?= $(CURDIR)/build
# SHELL := /bin/bash

export GO111MODULE = on

# Default target executed when no arguments are given to make.
default_target: all

.PHONY: default_target

###############################################################################
###                                  Build                                  ###
###############################################################################

BUILD_TARGETS := build install

build: BUILD_ARGS=-o $(BUILDDIR)/
build-linux:
	GOOS=linux GOARCH=amd64 LEDGER_ENABLED=false $(MAKE) build

$(BUILD_TARGETS): go.sum $(BUILDDIR)/
	go $@ $(BUILD_FLAGS) $(BUILD_ARGS) ./...

$(BUILDDIR)/:
	mkdir -p $(BUILDDIR)/

distclean: clean tools-clean

clean:
	rm -rf \
    $(BUILDDIR)/ \
    artifacts/ \
    tmp-swagger-gen/

all: build

build-all: tools build lint test

.PHONY: distclean clean build-all

###############################################################################
###                          Tools & Dependencies                           ###
###############################################################################

TOOLS_DESTDIR  ?= $(GOPATH)/bin

contract-tools:
ifeq (, $(shell which stringer))
	@echo "Installing stringer..."
	@go install golang.org/x/tools/cmd/stringer@latest
else
	@echo "stringer already installed; skipping..."
endif

ifeq (, $(shell which go-bindata))
	@echo "Installing go-bindata..."
	@go install github.com/kevinburke/go-bindata/go-bindata@latest
else
	@echo "go-bindata already installed; skipping..."
endif

ifeq (, $(shell which gencodec))
	@echo "Installing gencodec..."
	@go install github.com/fjl/gencodec@latest
else
	@echo "gencodec already installed; skipping..."
endif

ifeq (, $(shell which protoc-gen-go))
	@echo "Installing protoc-gen-go..."
	@go install github.com/fjl/gencodeclatest
	@go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
else
	@echo "protoc-gen-go already installed; skipping..."
endif

ifeq (, $(shell which protoc-gen-go-grpc))
	@go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
else
	@echo "protoc-gen-go-grpc already installed; skipping..."
endif

ifeq (, $(shell which solhint))
	@echo "Installing solhint..."
	@npm install -g solhint
else
	@echo "solhint already installed; skipping..."
endif

ifeq (, $(shell which solcjs))
	@echo "Installing solcjs..."
	@npm install -g solc@0.5.11
else
	@echo "solcjs already installed; skipping..."
endif

ifeq (, $(shell which solc))
	@echo "Installing solc..."
	@snap install solc
else
	@echo "solc already installed; skipping..."
endif

# ifeq (, $(shell which abigen))
# 	@echo "Installing abigen..."
# 	@add-apt-repository -y ppa:ethereum/ethereum
# 	@apt-get install -y ethereum
# else
# 	@echo "abigen already installed; skipping..."
# endif

go.sum: go.mod
	echo "Ensure dependencies have not been modified ..." >&2
	go mod verify
	go mod tidy

###############################################################################
###                              Documentation                              ###
###############################################################################

godocs:
	@echo "--> Wait a few seconds and visit http://localhost:6060/pkg/github.com/evmos/evmos/types"
	godoc -http=:6060

###############################################################################
###                                   Tests                                 ###
###############################################################################

test: test-unit
test-all: test-unit test-race
PACKAGES_UNIT=$(shell go list ./...)
TEST_PACKAGES=./...
TEST_TARGETS := test-unit test-unit-cover test-race

# Test runs-specific rules. To add a new test target, just add
# a new rule, customise ARGS or TEST_PACKAGES ad libitum, and
# append the new rule to the TEST_TARGETS list.
test-unit: ARGS=-timeout=10m -race
test-unit: TEST_PACKAGES=$(PACKAGES_UNIT)

test-race: ARGS=-race
test-race: TEST_PACKAGES=$(PACKAGES_NOSIMULATION)
$(TEST_TARGETS): run-tests

test-unit-cover: ARGS=-timeout=10m -race -coverprofile=coverage.txt -covermode=atomic
test-unit-cover: TEST_PACKAGES=$(PACKAGES_UNIT)

run-tests:
ifneq (,$(shell which tparse 2>/dev/null))
	go test -mod=readonly -json $(ARGS) $(EXTRA_ARGS) $(TEST_PACKAGES) | tparse
else
	go test -mod=readonly $(ARGS)  $(EXTRA_ARGS) $(TEST_PACKAGES)
endif

start-node:
#	go clean -testcache
	./scripts/start-evmos-local-node.sh

stop-node:
	pkill evmosd
	rm -rf /tmp/evmos-*

integration-test: start-node run-tests stop-node

.PHONY: run-tests test test-all test-import test-rpc start-node stop-node integration-test $(TEST_TARGETS)

###############################################################################
###                                Linting                                  ###
###############################################################################

lint:
	golangci-lint run --out-format=tab
	solhint contracts/**/*.sol

lint-contracts:
	@cd contracts && \
	npm i && \
	npm run lint

lint-fix:
	golangci-lint run --fix --out-format=tab --issues-exit-code=0

lint-fix-contracts:
	@cd contracts && \
	npm i && \
	npm run lint-fix
	solhint --fix contracts/**/*.sol

.PHONY: lint lint-fix

format:
	find . -name '*.go' -type f -not -path "./vendor*" -not -path "*.git*" -not -path "./client/docs/statik/statik.go" -not -name '*.pb.go' | xargs gofmt -w -s
	find . -name '*.go' -type f -not -path "./vendor*" -not -path "*.git*" -not -path "./client/docs/statik/statik.go" -not -name '*.pb.go' | xargs misspell -w
	find . -name '*.go' -type f -not -path "./vendor*" -not -path "*.git*" -not -path "./client/docs/statik/statik.go" -not -name '*.pb.go' | xargs goimports -w -local github.com/evmos/evmos
.PHONY: format

###############################################################################
###                        Compile Solidity Contracts                       ###
###############################################################################

CONTRACTS_DIR := contracts
COMPILED_DIR := contracts/compiled_contracts
TMP := tmp
TMP_CONTRACTS := $(TMP).contracts
TMP_COMPILED := $(TMP)/compiled.json
TMP_JSON := $(TMP)/tmp.json

# Compile and format solidity contracts for the erc20 module. Also install
# openzeppeling as the contracts are build on top of openzeppelin templates.
contracts-compile: contracts-clean openzeppelin create-contracts-json

# Install openzeppelin solidity contracts
openzeppelin:
	@echo "Importing openzeppelin contracts..."
	@cd $(CONTRACTS_DIR)
	@npm install
	@cd ../../../../
	@mv node_modules $(TMP)
	@mv package-lock.json $(TMP)
	@mv $(TMP)/@openzeppelin $(CONTRACTS_DIR)

# Clean tmp files
contracts-clean:
	@rm -rf tmp
	@rm -rf node_modules
	@rm -rf $(COMPILED_DIR)
	@rm -rf $(CONTRACTS_DIR)/@openzeppelin
	@rm -rf $(CONTRACTS_DIR)/bin
	@rm -rf $(CONTRACTS_DIR)/abi
	@rm -rf $(CONTRACTS_DIR)/*.go

# Compile, filter out and format contracts into the following format.
# {
# 	"abi": "[{\"inpu 			# JSON string
# 	"bin": "60806040
# 	"contractName": 			# filename without .sol
# }
create-contracts-json:
	@for c in $(shell ls $(CONTRACTS_DIR) | grep '\.sol' | sed 's/.sol//g'); do \
		command -v jq > /dev/null 2>&1 || { echo >&2 "jq not installed."; exit 1; } ;\
		command -v solc > /dev/null 2>&1 || { echo >&2 "solc not installed."; exit 1; } ;\
		mkdir -p $(COMPILED_DIR) ;\
		mkdir -p $(TMP) ;\
		echo "\nCompiling solidity contract $${c}..." ;\
		solc --combined-json abi,bin $(CONTRACTS_DIR)/$${c}.sol > $(TMP_COMPILED) ;\
		# solc --abi $(CONTRACTS_DIR)/$${c}.sol -o $(CONTRACTS_DIR)/abi/ ;\
		# solc --bin $(CONTRACTS_DIR)/$${c}.sol -o $(CONTRACTS_DIR)/bin/ ;\
		# abigen --bin=$(CONTRACTS_DIR)/bin/$${c}.bin --abi=$(CONTRACTS_DIR)/abi/$${c}.abi --pkg=store --out=$(CONTRACTS_DIR)/$${c}.go ;\
		echo "Formatting JSON..." ;\
		get_contract=$$(jq '.contracts["$(CONTRACTS_DIR)/'$$c'.sol:'$$c'"]' $(TMP_COMPILED)) ;\
		add_contract_name=$$(echo $$get_contract | jq '. + { "contractName": "'$$c'" }') ;\
		echo $$add_contract_name | jq > $(TMP_JSON) ;\
		abi_string=$$(echo $$add_contract_name | jq -cr '.abi') ;\
		echo $$add_contract_name | jq --arg newval "$$abi_string" '.abi = $$newval' > $(TMP_JSON) ;\
		mv $(TMP_JSON) $(COMPILED_DIR)/$${c}.json ;\
	done
	@rm -rf tmp
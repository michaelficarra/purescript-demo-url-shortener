default: build
all: build doc

MODULE = UrlShortener

build: dist/$(MODULE).js
deps: bower_components
doc: docs/README.md

BOWER_SRC = $(shell find bower_components/purescript-*/src -name '*.purs' -type f | sort)
SRC = $(shell find src -name '*.purs' -type f | sort)
BOWER_FFI = $(shell find bower_components/purescript-*/src -name '*.js' -type f | sort)
FFI = $(shell find src -name '*.js' -type f | sort)

NPM = $(shell command -v npm || { echo "npm not found."; exit 1; })
PSC = $(shell command -v psc || { echo "PureScript compiler (psc) not found."; exit 1; })
PSCDOCS = $(shell command -v psc-docs)
PSCBUNDLE = $(shell command -v psc-bundle)

NPM_BIN = $(shell npm bin)
BOWER = $(NPM_BIN)/bower

$(BOWER):
	npm install bower

dist/$(MODULE).js: bower_components $(SRC) $(FFI)
	@mkdir -p '$(@D)'
	$(PSC) \
	  $(BOWER_SRC) $(SRC) \
		$(BOWER_FFI:%=--ffi %) $(FFI:%=--ffi %) \
		--verbose-errors \
		--comments
	$(PSCBUNDLE) \
		$(shell find output -name '*.js' -type f | sort) \
	  --output dist/$(MODULE).js \
	  --module $(MODULE) \
	  --main $(MODULE)

.PHONY: default all build deps doc clean

docs/README.md: bower_components $(SRC)
	@mkdir -p '$(@D)'
	$(PSCDOCS) \
		--docgen $(MODULE) \
		$(BOWER_SRC) $(SRC) >'$@'

node_modules:
	$(NPM) install
	touch -cm node_modules

bower_components: $(BOWER)
	$(BOWER) install
	touch -cm bower_components

clean:
	rm -rf dist coverage bower_components node_modules .psci_modules

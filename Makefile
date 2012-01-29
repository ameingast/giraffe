all: build

configure:
	@cabal configure --enable-tests

build: configure
	@cabal build
	@hlint -c src

test: configure
	@cabal test

clean:
	@cabal clean

doc:
	@cabal haddock --executables
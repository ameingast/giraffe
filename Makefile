all: build

sdist: bench test
	@cabal sdist

configure:
	@cabal configure --enable-tests --enable-benchmarks

build: configure
	@cabal build
	@hlint -c bm src test
	@strip dist/build/giraffe/giraffe

repl:
	@cabal repl

deps:
	@cabal sandbox init
	@cabal install --only-dependencies --enable-tests --enable-benchmarks --force-reinstalls

test: configure
	@cabal test

bench: build
	@cabal bench

clean:
	@cabal clean

cleanall: clean
	@cabal sandbox delete

style:
	@find bm src test -name "*.hs" | xargs stylish-haskell -i

doc:
	@cabal haddock --executables

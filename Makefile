sandbox:
	cabal sandbox init 
	cabal update

deps:
	cabal install --only-dependencies

bootstrap: sandbox deps 

build: 
	cabal build 

clean:
	rm -rf .cabal-sandbox cabal.sandbox.config dist

all:
	ghcjs -package-db="./.cabal-sandbox/x86_64-linux-ghcjs-0.2.0-ghc7_10_2-packages.conf.d/" Main.hs 

init:
	cabal sandbox init
	cabal install --ghcjs ghcjs-dom

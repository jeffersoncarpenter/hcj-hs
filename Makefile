all: compile uglify clean

compile:
	ghcjs -package-db="./.cabal-sandbox/x86_64-linux-ghcjs-0.2.0-ghc7_10_2-packages.conf.d/" src/Main.hs -o build/Main.jsexe

clean:
	rm -rf build/Main.jsexe

uglify:
	uglifyjs build/Main.jsexe/rts.js \
	         build/Main.jsexe/lib.js \
	         build/Main.jsexe/out.js \
	         build/Main.jsexe/runmain.js \
	         -o dist/out.js

init:
	cabal sandbox init
	cabal install --ghcjs ghcjs-dom

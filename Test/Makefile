debug:
	ghc -o test -DDEBUG -O -package-db ../dist/package.conf.inplace --make test.hs

onepage:
	ghc -o test -DDEBUG -O -package-db ../dist/package.conf.inplace --make onepage.hs

profile:
	ghc -o test -DDEBUG -prof -fprof-auto -rtsopts -O -package-db ../dist/package.conf.inplace --make test.hs
	
runprof:
	./test +RTS -p -hy
		
demo:
	ghc -o test -O2 --make test.hs
	
clean:
	rm -f test
	rm -f *.o
	rm -f *.hi
	rm -f *.exe
	rm -f test.prof
	rm -f demo.pdf
	

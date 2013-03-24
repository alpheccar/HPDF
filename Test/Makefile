debug:
	ghc -o test -O -package-db ../dist/package.conf.inplace --make test.hs

profile:
	ghc -o test -prof -fprof-auto -rtsopts -O -package-db ../dist/package.conf.inplace --make test.hs
	
runprof:
	./test +RTS -p -hy
		
demo:
	ghc -o test -O2 --make test.hs
	
buildafm:
	ghc -o afm --make AFMParser.hs
	
metrics:
	@./afm Courier-Bold.afm

clean:
	rm -f test
	rm -f afm
	rm -f *.o
	rm -f *.hi
	rm -f *.exe
	rm -f test.prof
	rm -f *.pdf
	rm -rf bin
	rm -rf interfaces
	rm -rf c
	mkdir bin
	mkdir c
	mkdir interfaces
	

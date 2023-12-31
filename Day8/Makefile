GHC=ghc
FLAGS=-O3 -with-rtsopts="-M2G" -prof -fprof-auto -rtsopts -threaded

navigate: Main.hs Navigate.hs General.hs
	$(GHC) $(FLAGS) -o $@ $?

clean:
	rm -vf *.o *.hi navigate
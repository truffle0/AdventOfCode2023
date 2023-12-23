GHC=ghc
FLAGS=-O

navigate: Main.hs Navigate.hs Useful.hs
	$(GHC) $(FLAGS) -o $@ $?

clean:
	rm -vf *.o *.hi navigate
ODIR = build/

SOURCES = $(wildcard *.hs)
SOLVERS = $(patsubst %.hs,%.solver,${SOURCES})
SOLUTIONS = $(patsubst %.hs,%.solution,${SOURCES})

.PHONY: solve
solve: ${SOLUTIONS}
	@echo $? | \
		tr ' ' '\n' | \
		sort -g | \
		xargs head -v -n -0

# Make an empty input file, if one doesn't already exist
%.input:
	@touch "$@"

# GHC runs in --make mode and handles recompilation dependencies.
# This makefile is just here for convenience.
%.solver: %.hs
	@ghc --make $< -outputdir "${ODIR}/$@" -o "$@"

%.solution: %.solver %.input
	@"./$<" < "$*.input" > "$@"

.PHONY: clean
clean:
	@rm -rf ${ODIR}
	@rm -f ${SOLUTIONS} ${SOLVERS}

ODIR = build/

SOURCES = $(wildcard *.hs)
SOLVERS = $(patsubst %.hs,%.solver,${SOURCES})
SOLUTIONS = $(patsubst %.hs,%.solution,${SOURCES})

.PHONY: solve
solve: ${SOLUTIONS}
	@echo $? | \
		tr ' ' '\n' | \
		sort -g | \
		xargs head -v

# GHC runs in --make mode and handles recompilation dependencies.
# This makefile is just here for convenience.
%.solver: %.hs
	@ghc --make $< -outputdir ${ODIR} -o $@

%.solution: %.solver
#Optional input file
	@if [ -f "$(basename $<).input" ]; \
		then cat "$(basename $<).input" | "./$<" > $@; \
		else "./$<" > $@; \
		fi

.PHONY: clean
clean:
	@rm -rf ${ODIR}
	@rm -f ${SOLUTIONS} ${SOLVERS}

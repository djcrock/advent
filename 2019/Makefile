ODIR = build/

SOURCES = $(filter-out Advent.hs,$(wildcard *.hs))
SOLVERS = $(patsubst %.hs,%.solver,${SOURCES})
SOLUTIONS = $(patsubst %.hs,%.solution,${SOURCES})
EXPECTED = $(wildcard *.solution.expected)
TESTED = $(patsubst %.solution.expected,%.solution,${EXPECTED})

.PHONY: solve
solve: ${SOLUTIONS}
	@echo $? | \
		tr ' ' '\n' | \
		sort -g | \
		xargs head -v -n -0

.PHONY: test
test: ${TESTED}
	@echo $? | \
		tr ' ' '\n' | \
		sort -g | \
		xargs -I '{}' sh -c 'echo "" && diff -u --color "{}.expected" "{}" && echo "{} OK!"'

# Make an empty input file, if one doesn't already exist
%.input:
	@touch "$@"

# GHC runs in --make mode and handles recompilation dependencies.
# This makefile is just here for convenience.
%.solver: %.hs Advent.hs
	@ghc -O2 --make $< -outputdir "${ODIR}/$@" -o "$@"

%.solution: %.solver %.input
	@"./$<" < "$*.input" > "$@"

.PHONY: clean
clean:
	@rm -rf ${ODIR}
	@rm -f ${SOLUTIONS} ${SOLVERS}

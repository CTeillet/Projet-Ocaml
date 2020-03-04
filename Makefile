.PHONY: all test clean

all: test

test:
	@dune exec src/test_tableur.exe -f

clean:
	@dune clean

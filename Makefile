all:
	ghc --make -o parser parser.hs
	./parser libmv.v top.v test.mv > test.v
	iverilog test.v

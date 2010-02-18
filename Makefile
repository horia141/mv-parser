all: parser rickroll test

parser:
	ghc --make -o parser parser.hs
	rm -f parser.o
	rm -f parser.hi

rickroll: parser
	./parser -o ./projects/RickRoll/rickRoll.v ./projects/RickRoll/rickRoll.mv ./projects/RickRoll/top.v ./common/libmv.v
	iverilog -o ./projects/RickRoll/top.icv -s top ./projects/RickRoll/rickRoll.v

test: parser
	./parser -o ./projects/Test/test.v ./projects/Test/test.mv ./projects/Test/top.v ./common/libmv.v
	iverilog -o ./projects/Test/top.icv -s top ./projects/Test/test.v

clean:
	rm -f parser
	rm -f ./projects/RickRoll/top.icv
	rm -f ./projects/Test/top.icv

distclean: clean

realclean: clean

all: parser rickroll test

parser: parser.hs
	ghc --make -o parser parser.hs
	rm -f parser.o
	rm -f parser.hi

rickroll: parser
	./parser -o ./projects/RickRoll/RickRoll.v ./projects/RickRoll/rickRoll.mv ./projects/RickRoll/top.v ./common/counter.v ./common/mux.v ./common/reg.v
	iverilog -o ./projects/RickRoll/RickRoll.vvp -s top -Wall ./projects/RickRoll/RickRoll.v
	vvp ./projects/RickRoll/RickRoll.vvp

test: parser
	./parser -o ./projects/Test/Test.v ./projects/Test/test.mv ./projects/Test/top.v ./common/counter.v ./common/mux.v ./common/reg.v
	iverilog -o ./projects/Test/Test.vvp -s top -Wall ./projects/Test/Test.v
	vvp ./projects/Test/Test.vvp

clean:
	rm -f parser
	rm -f ./projects/RickRoll/RickRoll.v
	rm -f ./projects/RickRoll/RickRoll.vvp
	rm -f ./projects/RickRoll/RickRoll.vcd
	rm -f ./projects/Test/Test.v
	rm -f ./projects/Test/Test.vvp
	rm -f ./projects/Test/Test.vcd

distclean: clean

realclean: clean

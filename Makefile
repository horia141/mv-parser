LIBCOMMON = ./common/counter.v ./common/mux.v ./common/reg.v ./common/dcm.v

all: parser rickroll testdcm

parser: parser.hs
	ghc --make -o parser parser.hs
	rm -f parser.o
	rm -f parser.hi

rickroll: parser
	./parser -o ./projects/RickRoll/RickRoll.v ./projects/RickRoll/rickRoll.mv ./projects/RickRoll/top.v $(LIBCOMMON)
	iverilog -o ./projects/RickRoll/RickRoll.vvp -s top -Wall ./projects/RickRoll/RickRoll.v
	vvp ./projects/RickRoll/RickRoll.vvp

testdcm: parser
	./parser -o ./projects/Test/DCM/TestDCM.v ./projects/Test/DCM/TestDCM.mv $(LIBCOMMON)

clean:
	rm -f parser
	rm -f ./projects/RickRoll/RickRoll.v
	rm -f ./projects/RickRoll/RickRoll.vvp
	rm -f ./projects/RickRoll/RickRoll.vcd
	rm -f ./projects/Test/DCM/TestDCM.v

distclean: clean

realclean: clean

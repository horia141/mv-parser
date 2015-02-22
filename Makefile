LIBCOMMON = ./common/counter.v ./common/mux.v ./common/reg.v ./common/dcm.v

all: parser ledpattern rickroll suplimit vgavideo testdcm

dist:
	ghc --make -optl-static -optl-pthread -o parser.d parser.hs
	rm -f parser.o
	rm -f parser.hi

parser: parser.hs
	ghc --make -o parser parser.hs
	rm -f parser.o
	rm -f parser.hi

ledpattern: parser
	./parser -o ./projects/LedPattern/LedPattern.v ./projects/LedPattern/LedPattern.mv ./projects/LedPattern/MainTest.v $(LIBCOMMON)
	iverilog -o ./projects/LedPattern/LedPattern.vvp -s MainTest -Wall ./projects/LedPattern/LedPattern.v
	vvp ./projects/LedPattern/LedPattern.vvp

rickroll: parser
	./parser -o ./projects/RickRoll/RickRoll.v ./projects/RickRoll/RickRoll.mv ./projects/RickRoll/top.v $(LIBCOMMON)
	#iverilog -o ./projects/RickRoll/RickRoll.vvp -s top -Wall ./projects/RickRoll/RickRoll.v
	#vvp ./projects/RickRoll/RickRoll.vvp

suplimit: parser
	./parser -o ./projects/SupLimit/SupLimit.v ./projects/SupLimit/SupLimit.mv ./projects/SupLimit/MainTest.v $(LIBCOMMON)
	iverilog -o ./projects/SupLimit/SupLimit.vvp -s MainTest -Wall ./projects/SupLimit/SupLimit.v
	vvp ./projects/SupLimit/SupLimit.vvp

vgavideo: parser
	./parser -o ./projects/VGAVideo/VGAVideo.v ./projects/VGAVideo/VGAVideo.mv $(LIBCOMMON)

testdcm: parser
	./parser -o ./projects/Test/DCM/TestDCM.v ./projects/Test/DCM/TestDCM.mv $(LIBCOMMON)

clean:
	rm -f parser
	rm -f ./projects/LedPattern/LedPattern.v
	rm -f ./projects/LedPattern/LedPattern.vvp
	rm -f ./projects/LedPattern/LedPattern.vcd
	rm -f ./projects/RickRoll/RickRoll.v
	rm -f ./projects/RickRoll/RickRoll.vvp
	rm -f ./projects/RickRoll/RickRoll.vcd
	rm -f ./projects/SupLimit/SupLimit.v
	rm -f ./projects/SupLimit/SupLimit.vvp
	rm -f ./projects/SupLimit/SupLimit.vcd
	rm -f ./projects/VGAVideo/VGAVideo.v
	rm -f ./projects/Test/DCM/TestDCM.v

distclean: clean

realclean: clean

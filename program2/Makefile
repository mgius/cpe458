FSC=mono /home/mgius/fsharp/bin/fsc.exe --resident
PROGRAM=deltahedging
PRE=
POST=
INCLUDE=-I:/home/mgius/programs/cpe458/testing
REFERENCE=-r:xunit.dll -r:FsTest.dll
all: $(PROGRAM)

$(PROGRAM):
	$(FSC) $(INCLUDE) $(REFERENCE) $(PRE) $@.fs $(POST)

test: $(PROGRAM)
	$(FSC) $(INCLUDE) $(REFERENCE) $(PRE) $(PROGRAM).fs tests.fs $(POST)
	cp ../testing/xunit.dll .
	mono ../testing/xunit.console.exe ./tests.exe
	rm xunit.dll

clean:
	rm -f *.exe *.dll

PROGRAM=lab2
PRE=
POST=
INCLUDE=-I:/home/mgius/programs/cpe458/testing
REFERENCE=-r:xunit.dll -r:FsTest.dll
all: $(PROGRAM)

$(PROGRAM):
	bash -i -c 'fsc $(INCLUDE) $(REFERENCE) $(PRE) $@.fs $(POST)'

test: $(PROGRAM)
	cp ../testing/xunit.dll .
	bash -c 'mono ../testing/xunit.console.exe ./$(PROGRAM).exe'
	rm xunit.dll

clean:
	rm -f *.exe

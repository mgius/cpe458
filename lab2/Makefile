PROGRAM=lab2
PRE=
POST=
INCLUDE=-I:/home/mgius/programs/cpe458/testing
REFERENCE=-r:xunit.dll -r:FsTest.dll
all: $(PROGRAM)

$(PROGRAM):
	bash -i -c 'fsc $(INCLUDE) $(REFERENCE) $(PRE) $@.fs $(POST)'

test: $(PROGRAM)
	bash -c 'mono ../testing/xunit.console.exe ./$(PROGRAM).exe'

clean:
	rm -f *.exe

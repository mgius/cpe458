HOME=/home/mgius
FSHARP=$(HOME)/fsharp

FSC=mono $(FSHARP)/bin/fsc.exe --resident
XUNIT=mono $(FSHARP)/testing/xunit.console.exe
XUNITDLL=$(FSHARP)/testing/xunit.dll

PROGRAM=deltahedging
TESTS=tests
PRE=
POST=
INCLUDE=-I:$(FSHARP)/testing/
REFERENCE=
TESTREFERENCE=-r:xunit.dll -r:FsTest.dll

all: $(PROGRAM).dll

$(PROGRAM).dll: $(PROGRAM).fs
	$(FSC) -a $(INCLUDE) $(REFERENCE) $(PRE) $(PROGRAM).fs $(POST)

test: $(PROGRAM).dll
	$(FSC) $(INCLUDE) $(TESTREFERENCE) $(PRE) -r $(PROGRAM).dll $(TESTS).fs $(POST)
	cp $(XUNITDLL) .
	mono $(FSHARP)/testing/xunit.console.exe ./tests.exe
	rm xunit.dll

clean:
	rm -f *.exe *.dll

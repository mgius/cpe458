HOME=/home/mgius
FSHARP=$(HOME)/fsharp

FSC=mono $(FSHARP)/bin/fsc.exe --resident
XUNIT=mono $(FSHARP)/testing/xunit.console.exe
XUNITDLL=$(FSHARP)/testing/xunit.dll
TESTINGDLLS=$(FSHARP)/testing

PROGRAM=program3
TESTS=tests
PRE=
POST=
TESTINCLUDE=-I:$(TESTINGDLLS)
REFERENCE=
TESTREFERENCE=-r:xunit.dll -r:FsTest.dll

all: $(PROGRAM)

$(PROGRAM): $(PROGRAM).fs
	$(FSC) $(REFERENCE) $(PRE) $(PROGRAM).fs $(POST)

dll: $(PROGRAM).dll
$(PROGRAM).dll: $(PROGRAM).fs
	$(FSC) -a $(REFERENCE) $(PRE) $(PROGRAM).fs $(POST) -o $@

run: $(PROGRAM)
	mono ./$(PROGRAM).exe

test: $(PROGRAM).dll
	$(FSC) $(TESTINCLUDE) $(TESTREFERENCE) $(PRE) -r $^ $(TESTS).fs $(POST)
	cp $(XUNITDLL) .
	mono $(FSHARP)/testing/xunit.console.exe ./tests.exe
	rm xunit.dll

clean:
	rm -f *.exe *.dll
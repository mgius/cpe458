HOME=/home/mgius
FSHARP=$(HOME)/fsharp

FSC=mono $(FSHARP)/bin/fsc.exe --resident
XUNIT=mono $(FSHARP)/testing/xunit.console.exe
XUNITDLL=$(FSHARP)/testing/xunit.dll

PROGRAM=lab2
TESTS=tests
PRE=
POST=
INCLUDE=-I:$(FSHARP)/testing/
REFERENCE=-r:xunit.dll -r:FsTest.dll

all: $(PROGRAM)

$(PROGRAM):
	$(FSC) $(INCLUDE) $(REFERENCE) $(PRE) $@.fs $(POST)

test:
	$(FSC) $(INCLUDE) $(REFERENCE) $(PRE) $(PROGRAM).fs $(POST)
	cp $(XUNITDLL) .
	mono $(FSHARP)/testing/xunit.console.exe ./$(PROGRAM).exe
	rm xunit.dll

clean:
	rm -f *.exe *.dll

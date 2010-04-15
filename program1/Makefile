PROGRAM=Game
PRE=Player.fs MyPlayers.fs
POST=
all: $(PROGRAM)

$(PROGRAM):
	bash -i -c 'fsc $(PRE) $@.fs $(POST)'
	mono $@.exe

clean:
	rm -f *.exe

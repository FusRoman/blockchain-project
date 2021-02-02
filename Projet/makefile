OC = ocamlc
BUILD = _build/
EXEC = _exec/
MINER = _miner/
WALLER = _waller/
LIB = -I +threads -I +digest -I +marshal unix.cma threads.cma

all: miner_serv.exe waller.exe
	clear

# miner_serv rules

miner_serv.exe: miner.cmo command.cmo block.cmo _build _exec
	$(OC) $(LIB) -I $(BUILD) $(BUILD)miner.cmo $(BUILD)command.cmo $(BUILD)block.cmo $(MINER)miner_serv.ml -o $(EXEC)miner_serv.exe
	mv $(MINER)*.cmo $(MINER)*.cmi $(BUILD)

miner.cmo: _build 
	$(OC) -c $(MINER)miner.ml
	mv $(MINER)*.cmo $(MINER)*.cmi $(BUILD)

#waller rules
waller.exe: _build miner_serv.exe
	$(OC) $(LIB) -I $(BUILD) $(BUILD)miner.cmo $(BUILD)command.cmo $(WALLER)waller.ml -o $(EXEC)waller.exe
	mv $(WALLER)*.cmi $(WALLER)*.cmo $(BUILD)

# command rules
command.cmo: _build miner.cmo
	$(OC) -c -I $(BUILD) $(WALLER)command.ml
	mv $(WALLER)*.cmo $(WALLER)*.cmi $(BUILD)

# block rules
block.cmo: _build
	$(OC) -c $(LIB) $(MINER)block.ml
	mv $(MINER)*.cmo $(MINER)*.cmi $(BUILD)


# directories creation rules
_build:
	mkdir _build

_exec:
	mkdir _exec


# clean rules
clean:
	rm $(BUILD)*.cmi $(BUILD)*.cmo
	rm $(EXEC)*.exe
	rm -r _build _exec


# running rules
new_miner:
	clear
	ocamlrun $(EXEC)miner_serv.exe $(ARGS)

new_waller:
	clear
	ocamlrun $(EXEC)waller.exe $(ARGS)
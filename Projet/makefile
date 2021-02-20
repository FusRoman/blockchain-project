OC = ocamlc
BUILD = _build/
EXEC = _exec/
MINER = _miner/
WALLER = _waller/
UTILS = _utils/
LIB = -I +threads threads.cma
PACK = -package cryptokit -linkpkg
CMO =  $(BUILD)miscellaneous.cmo $(BUILD)node.cmo $(BUILD)miner.cmo $(BUILD)command.cmo $(BUILD)block.cmo

all: miner_serv.exe waller.exe
	clear

# miner_serv rules

miner_serv.exe: node.cmo miner.cmo command.cmo block.cmo _build _exec
	ocamlfind $(OC) $(PACK) $(LIB) -I $(BUILD) $(CMO) $(MINER)miner_serv.ml -o $(EXEC)miner_serv.exe

	mv $(MINER)*.cmo $(MINER)*.cmi $(BUILD)

miner.cmo: _build node.cmo
	ocamlfind $(OC) -c -I $(BUILD) $(PACK) $(MINER)miner.ml

	mv $(MINER)*.cmo $(MINER)*.cmi $(BUILD)

#waller rules
waller.exe: _build miner_serv.exe
	ocamlfind $(OC) $(PACK) $(LIB) -I $(BUILD) $(CMO) $(WALLER)waller.ml -o $(EXEC)waller.exe

	mv $(WALLER)*.cmi $(WALLER)*.cmo $(BUILD)

# command rules
command.cmo: _build miner.cmo
	$(OC) -c -I $(BUILD) $(UTILS)command.ml

	mv $(UTILS)*.cmo $(UTILS)*.cmi $(BUILD)

# node rules
node.cmo: _build block.cmo
	ocamlfind $(OC) -c -I $(BUILD) $(PACK) $(UTILS)node.ml

	mv $(UTILS)*.cmo $(UTILS)*.cmi $(BUILD)

# block rules
block.cmo: _build miscellaneous.cmo
	ocamlfind $(OC) -c $(PACK) $(LIB) -I $(BUILD) $(BUILD)miscellaneous.cmo $(UTILS)block.ml

	mv $(UTILS)*.cmo $(UTILS)*.cmi $(BUILD)

#miscellaneous rules
miscellaneous.cmo: _build
	ocamlfind $(OC) -c $(PACK) $(UTILS)miscellaneous.ml

	mv $(UTILS)*.cmo $(UTILS)*.cmi $(BUILD)


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
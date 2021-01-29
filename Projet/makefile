OC = ocamlc
BUILD = _build/
EXEC = _exec/
MINER = _miner/
WALLER = _waller/
LIB = -thread unix.cma threads.cma

all: miner_serv.exe waller.exe
	clear

# miner_serv rules

miner_serv.exe: miner.cmo command.cmo
	$(OC) $(LIB) -I $(BUILD) $(BUILD)miner.cmo $(BUILD)command.cmo $(MINER)miner_serv.ml -o $(EXEC)miner_serv.exe
	mv $(MINER)*.cmo $(MINER)*.cmi $(BUILD)

miner.cmo:
	$(OC) -c $(MINER)miner.ml
	mv $(MINER)*.cmo $(MINER)*.cmi $(BUILD)

#waller rules
waller.exe: miner_serv.exe
	$(OC) $(LIB) -I $(BUILD) $(BUILD)miner.cmo $(BUILD)command.cmo $(WALLER)waller.ml -o $(EXEC)waller.exe
	mv $(WALLER)*.cmi $(WALLER)*.cmo $(BUILD)

command.cmo: miner.cmo
	$(OC) -c -I $(BUILD) $(WALLER)command.ml
	mv $(WALLER)*.cmo $(WALLER)*.cmi $(BUILD)


clean:
	rm $(BUILD)*.cmi $(BUILD)*.cmo
	rm $(EXEC)*.exe

new_miner:
	clear
	ocamlrun $(EXEC)miner_serv.exe $(ARGS)

new_waller:
	clear
	ocamlrun $(EXEC)waller.exe $(ARGS)
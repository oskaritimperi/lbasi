FPC_FLAGS = -Mobjfpc -FUobj -g

calc1: calc1.pas | obj
	fpc ${FPC_FLAGS} $<

obj:
	mkdir obj

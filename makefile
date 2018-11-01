all: getinp

getinp:
	gfortran -c sub_compro.f90 sub_reawerout.f90 mod_precmod.f90 mod_stringmod.f90 mod_var.f90 getinp.f90
	gfortran -o getinp sub_compro.o sub_reawerout.o mod_precmod.o mod_stringmod.o mod_var.o getinp.o
	rm *.o

clean:
	rm getinp build vertex werami
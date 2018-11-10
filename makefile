getinp:
	gfortran -c mod* sub_reawerout.f90 sub_compro.f90 getinp.f90
	gfortran *.o -o getinp
	rm *.o *.mod

clean:
	rm getinp
	rm build vertex werami

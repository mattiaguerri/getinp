# getinp

## Fortran routine to get input thermodynamic dataset for LitMod.

### Input

Input parameters are defined in the file parameters.h

### Output

+ Properties of the system (function of P and T):
output *_sys.dat
The first line has the number of PT nodes.
Temperature[K]   Pressure[Bar]   Vp(T)   Vs(T)   Dens.   Vp   Vs

+ Properties of the single phases (fucntion of P and T):
Output *_phases.dat:
1   Temperature[K]   Pressure[Bar]
0   Phase   Weight %    Vol. %    Mol. %    SiO2   Al2O3    FeO (wt%)    MgO (wt%)

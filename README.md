# getinp

## Fortran routine to get input look-up tables for LitMod.

### Output

+ Data about the properties of the system
output *_sys.dat
The first line has the number of PT nodes.
Temperature[K]   Pressure[Bar]   Vp(T)   Vs(T)   Dens.   Vp   Vs


+ Data about the properties of the single phases
Output *_phases.dat:
1   Temperature[K]   Pressure[Bar]
0   Phase   Weight %    Vol. %    Mol. %    SiO2   Al2O3    FeO (wt%)    MgO (wt%)

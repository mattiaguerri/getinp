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


### Input Examples

Composition examples:

Harzburgite (Cammarano et al., 2011).
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
36.04   00.65    05.97  56.54  00.79  00.00   00.00  00.00  00.00    00.00    00.00   00.00

Pyrolite (Xu et al., 2008).
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
38.71   02.22    06.17  49.85  02.94  00.00   00.00  00.00  00.00    00.00    00.00   00.00

PUM MS (Afonso et al., 2008).
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
45.00   04.50    08.10  37.80  03.60  00.36   00.00  00.00  00.38    00.20    00.14   00.25

Proton sub continental lithospheric mantle. (Afonso et al., 2008).
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
44.60   01.90    07.90  42.60  01.70  00.12   00.00  00.00  00.40    00.07    00.12   00.26

Average tecton garnet peridotite (Afonso et al., 2008).
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
45.00   03.90    08.10  38.70  03.20  00.28   00.00  00.00  00.41    00.16    00.07   00.24

Composition from ziberna file.
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
42.11   03.55    09.03  41.94  01.23  00.04   02.40  00.50  00.82    00.00

Average central Mongolia. Fullea et al., 2012.
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
44.59   03.48    08.25  39.56  02.85  00.31   00.00  00.00  00.00    00.00    00.00   00.00

Rudnick and Gao 2014.
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
66.62   15.40    15.40  02.48  03.59  03.27   02.80  00.00  00.00    00.64    00.10   00.00

bulk_TMMF.
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
53.82   16.32    11.81  07.04  09.40  02.96   01.00  00.00  00.00    00.00    00.00   00.00

bulk_TM.
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
57.10   15.90    09.10  05.30  07.40  03.10   01.30  00.00  00.00    00.00    00.00   00.00

bulk_IN.
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
60.80   15.50    07.40  04.25  06.10  03.25   01.85  00.00  00.00    0.00     00.00   00.00

bulk_SH.
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
64.50   15.10    05.70  03.20  04.80  03.40   02.40  00.00  00.00    00.00    00.00   00.00

bulk_SHFS.
1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
68.68   14.72    04.64  02.57  03.96  03.56   03.41  00.00  00.00    00.00    00.00   00.00

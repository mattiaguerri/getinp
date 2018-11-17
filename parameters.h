! Path to the perplex folder.
perpat = '/home/mattiaguerri/Dropbox/perplex_676_src1'

! Name of the Perple_X project.
nampro = 'bulk_sh_000_met'

! Equilibruim (0). Metastability (1).
met = 1

! Thermodynamic database index.
inddat = 6

! Pmin(bar)   Pmax   Tmin(K)   Tmax
premin = 1e3
premax = 7e3
temmin = 300.
temmax = 800.

! Number of P nodes and T nodes.
nP = 5
nT = 5

! P(bar) and T(K) for the reference mineralogy.
preref = 5000.
temref = 700.

! Insert composition for all the oxides (wt. %), if you do not need one just insert 00.00.
! 1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
ox1 = 64.50
ox2 = 15.10
ox3 = 5.70
ox4 = 3.2
ox5 = 4.8
ox6 = 3.4
ox7 = 2.4
ox8 = 00.00
ox9 = 00.00
ox10 = 00.00
ox11 = 00.00
ox12 = 00.00

! Thermodynamic Database options.
! 0. stx08ver.dat
! 1. stx11ver.dat _________[SiO2 Al2O3 FeO MgO CaO Na2O][best for the mantle].
! 2. hp02ver.dat
! 3. hp02ver_jca.dat ______[file from Afonso][OUT OF DATE].
! 4. cr_hp02ver.dat
! 5. cr_hp17ver.dat _______[cr_hp02ver.dat + some param from hp02ver.dat].
! 6. hpha02ver.dat ________[best for the crust].
! 7. hpha02ver.dat ________[best for the crust][with titan].  Modified, .
! 8. cr_hp02_ZK1.dat ______[file from Ziberna][OUT OF DATE].





!!!=============================================================================


!!!#############################################################################
!!!--------------------------------------------------------------------------
!!! Input example for the modeling of the entire mantle.
!!! --------------------------------------------------------------------------
!!! '/mnt/home_geo/mguerri/Dropbox/perplex_676_src1' ! Path to the perplex folder.
!!! har                     ! Name of the Perple_X project.
!!! 0                       ! Equilibruim (0). Metastability (1).
!!! 1                       ! Thermodynamic database index.
!!! 1 140E+4 300 3000       ! Pmin(bar)   Pmax   Tmin(K)   Tmax
!!! 2000 135                ! Number of P nodes and T nodes.
!!! 5000 700                ! P(bar) and T(K) for the reference mineralogy.

!!! Insert composition for all the oxides (wt. %), if you don't need one just insert 00.00.
!!! 1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
!!!  36.04   00.65    05.97  56.54  00.79  00.00   00.00  00.00  00.00    00.00    00.00   00.00
!!!#############################################################################


!!!#############################################################################
!!!-----------------------------------------------------------------------------
!!! Input example for the modeling of the entire crust.
!!!-----------------------------------------------------------------------------
!!! '/mnt/home_geo/mguerri/Dropbox/perplex_676_src1' ! Path to the perplex folder.
!!! bulk_rg                 ! Name of the Perple_X project.
!!! 0                       ! Equilibruim (0). Metastability (1).
!!! 6                       ! Thermodynamic database index.
!!! 1 3E+4 300 2000         ! Pmin(bar)   Pmax   Tmin(K)   Tmax
!!! 2000 135                ! Number of P nodes and T nodes.
!!! 5000 700                ! P(bar) and T(K) for the reference mineralogy.

!!! Insert composition for all the oxides (wt. %), if you don't need one just insert 00.00.
!!! 1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
!!!  66.62	 15.40    15.40  2.48   3.59	 3.27    2.80   00.00  00.00    00.00    00.10   00.00
!!!#############################################################################


!!!#############################################################################
!!!-----------------------------------------------------------------------------
!!! Input example for the modeling of the entire crust.
!!!-----------------------------------------------------------------------------
!!!#############################################################################

!!! '/mnt/home_geo/mguerri/Dropbox/perplex_676_src1' ! Path to the perplex folder.

!!! Name of the Perple_X project.
!!! bulk_TMMFequ

!!! Equilibruim (0). Metastability (1).
!!! 0

!!! Thermodynamic database index.
!!! 6

!!! Pmin(bar)   Pmax   Tmin(K)   Tmax
!!! 1 3E+4 300 2000

!!! Number of P nodes and T nodes.
!!! 2000 135

!!! P(bar) and T(K) for the reference mineralogy.
!!! 5000 700

!!! Insert composition for all the oxides (wt. %), if you don't need one just insert 00.00.
!!! 1]SiO2  2]Al2O3  3]FeO  4]MgO  5]CaO  6]Na2O  7]K2O  8]H2O  9]Cr2O3  10]TiO2  11]MnO  12]NiO
!!! 53.82   16.32    11.81  07.04  09.40  02.96   01.00  00.00  00.00    00.00    00.00   00.00
!!!#############################################################################








!!! Output from Perple_X


!!! 1]Name        2]Counter   3]T(K)           4]P(bar)         5]V,J/bar/mol    6]H,J/mol
!!! 7]Gruneisen_T    8]Ks,bar   9]Gs,bar         10]v0,km/s        11]vp,km/s        12]vs,km/s
!!! 13]vp/vs          14]rho,kg/m3      15]G,J/mol
!!! 16]cp,J/K/mol     17]alpha,1/K      18]beta,1/bar     19]S,J/K/mol      20]n,mol          21]N,g            22]Ks_T,bar/K
!!! 23]Gs_T,bar/K     24]Ks_P           25]Gs_P           26]v0_T           27]vp_T           28]vs_T           29]v0_P
!!! 30]vp_P           31]vs_P           32]cp/cv          33]vol,%          34]wt,%           35]mol,%          36]SIO2,wt%
!!! 37]AL2O3,wt%      38]FEO,wt%        39]MGO,wt%        CAO,wt%        NA2O,wt%       mu_SIO2,J/mol  mu_AL2O3,J/mol
!!! mu_FEO,J/mol   mu_MGO,J/mol   mu_CAO,J/mol   mu_NA2O,J/mol




!!! Name        Counter   1]T(K)           2]P(bar)         3]V,J/bar/mol    4]H,J/mol        5]Gruneisen_T    6]Ks,bar         7]Gs,bar
!!! 8]v0,km/s        9]vp,km/s        10]vs,km/s        11]vp/vs          12]rho,kg/m3      13]G,J/mol        14]cp,J/K/mol
!!! 15]alpha,1/K      16]beta,1/bar     17]S,J/K/mol      18]n,mol          19]N,g            20]Ks_T,bar/K     21]Gs_T,bar/K
!!! 22]Ks_P           23]Gs_P           24]v0_T           25]vp_T           26]vs_T           27]v0_P           28]vp_P           29]vs_P
!!! 30]cp/cv          31]vol,%          32]wt,%           33]mol,%          34]SIO2,wt%       35]AL2O3,wt%      36]FEO,wt%        37]MGO,wt%
!!! 38]CAO,wt%        NA2O,wt%       mu_SIO2,J/mol  mu_AL2O3,J/mol mu_FEO,J/mol   mu_MGO,J/mol   mu_CAO,J/mol   mu_NA2O,J/mol

! Interface to create the property tables used in the LitMod codes.
!     - It runs perple_x version 6.7.6.
!     - Input parameters from par.inp file.
!     - No correction for NaNs.
!     - In case you are modelling metastable mineralogies, check that the phases stable
!         at the chosen PT conditions are recognized in the subroutine 'compro'.

program getinp

use var

implicit none


! Parameters.
integer, parameter:: flag1 = 1 ! Switch running Perple_X off or on.
integer inddat, nT, nP, nx, ny, is, co1, met
integer numpha1, numpha2
character(100)nam1,nam2,nam3
character(100) nampro, head(13)
real(8) premin,premax,temmin,temmax,preref,temref
character(len=100), dimension(:), allocatable :: nampro_all
character(100) cwd,perpat,path1,path2
character(100) file1,file11,file2,file22
character(100) str1,str2,str11,str22
character(len=10), dimension(:), allocatable :: nampha,nampha1,nampha2
real(8) siowei,siofac,feofac,mgofac,sioper,feoper,mgoper,totwei1,totwei2
real(8) pre, tem, vpsys, vssys, densys
real*8, dimension(:), allocatable :: ar1,ar2
real*8, dimension(:,:), allocatable :: syspro
real*8, dimension(:,:), allocatable :: phapro1, phapro2
integer l, ll, m
integer nox, nox1, nox2



include 'parameters.h'
! include 'thermodynamic_dataset.h'


! Set path to cwd and link the perple_x executables.
call getcwd(cwd)
path1=trim(perpat)//'/build'
path2=trim(cwd)//'/build'
call symlnk(path1, path2, status=is)
path1=trim(perpat)//'/vertex'
path2=trim(cwd)//'/vertex'
call symlnk(path1, path2, status=is)
path1=trim(perpat)//'/werami'
path2=trim(cwd)//'/werami'
call symlnk(path1, path2, status=is)


	! Clean the cd from previous perple_x output with the same name of the input one.
	if(flag1==1)then
	str1='.arf'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='.blk'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='.dat'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='.plt'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='.tof'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='_VERTEX_options.txt'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='_WERAMI_options.txt'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='_1.tab'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='_1.txt'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='_1.phm'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='_phases.dat'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	str1='_sys.dat'
	file1 = trim(nampro) // trim(str1)
	open(unit=1,iostat=is,file=file1,status='old')
	if (is==0) close(1,status='delete')
	endif


! Write the input for build.
open(1,file='inpbui.txt')


select case (inddat)


!*************************************************************************
	! CASE 0.
	! Thermodynamic database: stx08ver.dat
	case(0)

	!	Create symbolic links.
	path1=trim(perpat)//'/stx08ver.dat'
	path2=trim(cwd)//'/stx08ver.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/perplex_option.dat'
	path2=trim(cwd)//'/perplex_option.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/stx11_solution_model.dat'
	path2=trim(cwd)//'/stx08_solution_model.dat'
	call symlnk(path1, path2, status=is)

write(1,1000)nampro
write(1,3301)
write(1,3380) temmin, temmax, premin, premax
print*, temmin, temmax, premin, premax
write(1,3300) ox1,ox2,ox3,ox4,ox5,ox6
write(1,3302)
3301  format('stx08ver.dat',/ &
             'perplex_option.dat',/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'SIO2',/ &
             'AL2O3',/ &
             'FEO',/ &
             'MGO',/ &
             'CAO',/ &
             'NA2O',/ &
             ,/ &
             '2',/ &
             'n',/ &
             '2',/)
3380  format(2F15.3,/ &
             2F15.3,/ &
             'y')
3300  format(6F10.4)
3302  format('n',/ &
             'y',/ &
             'n',/ &
             'foGL',/ &
             'faGL',/ &
             'nasGL',/ &
             ,/ &
             'y'/ &
             'stx08_solution_model.dat',/ &
             'C2/c',/ &
             'Wus',/ &
             'Pv',/ &
             'Pl',/ &
             'Sp',/ &
             'O',/ &
             'Wad',/ &
             'Ring',/ &
             'Opx',/ &
             'Cpx',/ &
             'Gt',/ &
             'Gt_maj',/ &
             ,/ &
             'pro',/)

	! CASE 1.
	! Thermodynamic database: stx11ver.dat
	case(1)

	!	Create symbolic links.
	path1=trim(perpat)//'/stx11ver.dat'
	path2=trim(cwd)//'/stx11ver.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/perplex_option.dat'
	path2=trim(cwd)//'/perplex_option.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/stx11_solution_model.dat'
	path2=trim(cwd)//'/stx11_solution_model.dat'
	call symlnk(path1, path2, status=is)

	write(1,1000)nampro
	write(1,3001)
	write(1,3080) temmin,temmax,premin,premax
	write(1,3200) ox1,ox2,ox3,ox4,ox5,ox6
	write(1,3202)
3001  format('stx11ver.dat',/ &
             'perplex_option.dat',/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'SIO2',/ &
             'AL2O3',/ &
             'FEO',/ &
             'MGO',/ &
             'CAO',/ &
             'NA2O',/ &
             ,/ &
             '2',/ &
             'n',/ &
             '2',/)
3080  format(2F15.3,/ &
             2F15.3,/ &
             'y')
3200  format(6F10.4)
3202  format('n',/ &
             'y',/ &
             'n',/ &
             'foGL',/ &
             'faGL',/ &
             'nasGL',/ &
             ,/ &
             'y'/ &
             'stx11_solution_model.dat',/ &
             'C2/c',/ &
             'Wus',/ &
             'Pv',/ &
             'Pl',/ &
             'Sp',/ &
             'O',/ &
             'Wad',/ &
             'Ring',/ &
             'Opx',/ &
             'Cpx',/ &
             'Gt',/ &
             'Gt_maj',/ &
             ,/ &
             'pro',/)

	! CASE 2.
	! Thermodynamic database: hp02ver.dat
	case(2)

!	Create symbolic links.
	path1=trim(perpat)//'/hp02ver.dat'
	path2=trim(cwd)//'/hp02ver.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/perplex_option.dat'
	path2=trim(cwd)//'/perplex_option.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/solution_model.dat'
	path2=trim(cwd)//'/solution_model.dat'
	call symlnk(path1, path2, status=is)

	if(ox8.eq.0.0E0)then ! In case of 0% H2O.
		write(1,11128)nampro
		write(1,11129)
		write(1,11130)temmin,temmax,premin,premax
		write(1,11131)ox1,ox2,ox3,ox4,ox5,ox6,ox9
		write(1,11132)
11128 format(A20)
11129	format('hp02ver.dat',/ &
             ,/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'SIO2',/ &
             'AL2O3',/ &
             'FEO',/ &
             'MGO',/ &
             'CAO',/ &
             'NA2O',/ &
             'K2O',/ &
             ,/ &
             '2',/ &
             'n',/ &
             '2',/)
11130	format(2F15.3,/ &
             2F15.3,/ &
             'y',/)
11131	format(7F10.4)
11132	format('n',/ &
             'y',/ &
             'n',/ &
             'ne',/ &
             'ab',/ &
             'abL',/ &
             'qL',/ &
             'q8L',/ &
             'qGL',/ &
             'coGL',/ &
             'foL',/ &
             'fo8L',/ &
             'foGL',/ &
             'woGL',/ &
             'nasGL',/ &
             'kalGL',/ &
             'crGL',/ &
             'diL',/ &
             'spr4',/&
             'jd',/ &
             'mgts',/ &
             'opx',/ & ! no effect
             ,/ &
             'y',/ &
             'solution_model.dat',/ &
             'Cpx(HP)',/ &
             'melt(HP)',/ &
             'O(HP)',/ &
             'Sp(HP)',/ &
             'Gt(HP)',/ &
             'Opx(HP)',/ &
             'feldspar',/ &
             ,/ &
             'pro',/)


	elseif(ox8.gt.0.0E0)then ! In case of >0 % H2O.
		write(1,11123)nampro
		write(1,11124)
		write(1,11125)temmin,temmax,premin,premax
		write(1,11126) ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox8
		write(1,11127)
11123 format(A20)
11124 format('hp02ver.dat',/ &
             'perplex_option.dat',/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'SIO2',/ &
             'AL2O3',/ &
             'FEO',/ &
             'MGO',/ &
             'CAO',/ &
             'NA2O',/ &
             'K2O',/ &
             'H2O',/ &
             ,/ &
             '5',/ &
             '2',/ &
             'n',/ &
             '2',/)
11125 format(2F15.3,/ &
             2F15.3,/ &
             'y')
11126 format(8F8.4)
11127 format('n',/ &
             'y',/ &
             'n',/ &
             'ne',/ &
             'abL',/ &
             'qL',/ &
             'q8L',/ &
             'qGL',/ &
             'coGL',/ &
             'faL',/ &
             'fa8L',/ &
             'faGL',/ &
             'foL',/ &
             'fo8L',/ &
             'foGL',/ &
             'woGL',/ &
             'nasGL',/ &
             'kalGL',/ &
             'h2oGL',/ &
             'gl',/ &
             'crGL',/ &
             'spr4',/&
             'diL',/ &
             'jd',/ &
             'mgts',/ &
             ,/ &
             'y',/ &
             'solution_model.dat',/ &
             'Cpx(HP)',/ &
             'melt(HP)',/ &
             'Chl(HP)',/ &
             'O(HP)',/ &
             'Sp(HP)',/ &
             'Gt(HP)',/ &
             'Opx(HP)',/ &
             'Pheng(HP)',/ &
             'Bio(HP)',/ &
             'feldspar',/ &
             ,/ &
             'pro',/)
	endif
	!*************************************************************************





	!*************************************************************************
	! CASE 3.
	! Thermodynamic database: hp02ver_jca.dat
	case(3)

!	Create symbolic links.
	path1=trim(perpat)//'/hp02ver_jca.dat'
	path2=trim(cwd)//'/hp02ver_jca.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/perplex_option.dat'
	path2=trim(cwd)//'/perplex_option.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/solut_08.dat'
	path2=trim(cwd)//'/solut_08.dat'
	call symlnk(path1, path2, status=is)


     	if(ox8.gt.0.0E0)then ! In case of >0% H2O.
      write(1,1110)nampro
	write(1,1011)
	write(1,1880)temmin,temmax,premin,premax
	write(1,2011)ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox8
   	write(1,1012)
1110  format(A20)
1011  format('hp02ver_jca.dat',/ &
            ,/ &
            'n',/ &
            'n',/ &
            'n',/ &
            'n',/ &
            'SIO2',/ &
            'AL2O3',/ &
            'FEO',/ &
            'MGO',/ &
            'CAO',/ &
            'NA2O',/ &
            'K2O',/ &
            'H2O',/ &
             ,/ &
            '5',/ &
            '2',/ &
            'n',/ &
            '2',/)
1880  format(2F15.3,/ &
             2F15.3,/ &
            'y')
2011  format(F7.3,7F7.3)
1012  format('n',/ &
            'y',/ &
            'n',/ &
            'ne',/ &
            'feo',/ &
            'na2o',/ &
            'k2o',/ &
            'cao',/ &
            'al2o3',/ &
            'ab',/ &
            ,/ &
            'y',/ &
            'solut_08.dat',/ &
            'O(HP)',/ &
            'Sp(HP)',/ &
            'Gt(stx8)',/ &
            'C2/c(jca)',/ &
            'Opx(HP)',/ &
            'Cpx(HP)',/ &
            'Pl(h)',/ &
            'Wad(stx8)',/ &
            'San(TH)',/ &
            'GlTrTsPg',/ &
            'B',/ &
            'Chl(HP)',/ &
            'KN-Phen',/ &
            'T',/ &
            'A-phase',/ &
            'Atg',/ &
            ,/ &
            'pro',/)


      elseif(ox8.eq.0.0E0)then ! In case of 0% H2O.
      write(1,1117)nampro
			write(1,1711)
			write(1,1887)temmin,temmax,premin,premax
			write(1,2511)ox1,ox2,ox3,ox4,ox5,ox6
      write(1,1312)
1117  format(A20)
1711  format('hp02ver_jca.dat',/ &
            ,/ &
            'n',/ &
            'n',/ &
            'n',/ &
            'n',/ &
            'SIO2',/ &
            'AL2O3',/ &
            'FEO',/ &
            'MGO',/ &
            'CAO',/ &
            'NA2O',/ &
             ,/ &
            '2',/ &
            'n',/ &
            '2',/)
1887  format(2F15.3,/ &
             2F15.3,/ &
            'y')
2511  format(F7.3,5F7.3)
1312  format('y',/ &
            'printfile_pr',/ &
            'y',/ &
            'plotfile_pl',/ &
            'y',/ &
            'n',/ &
            'ne',/ &
            'feo',/ &
            'na2o',/ &
            'k2o',/ &
            'cao',/ &
            'al2o3',/ &
            'ab',/ &
            ,/ &
            'y',/ &
            'solut_08.dat',/ &
            'O(HP)',/ &
            'Sp(HP)',/ &
            'Gt(stx8)',/ &
            'C2/c(jca)',/ &
            'Opx(HP)',/ &
            'Cpx(HP)',/ &
            'Pl(h)',/ &
            'Wad(stx8)',/ &
            ,/ &
            'pro',/)
	endif
	!*************************************************************************





	!*************************************************************************
	! case 4.
	! cr_hp02ver database.
	case(4)


	! Create symbolic links.
	path1=trim(perpat)//'/cr_hp02ver.dat'
	path2=trim(cwd)//'/cr_hp02ver.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/perplex_option.dat'
	path2=trim(cwd)//'/perplex_option.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/solution_model.dat'
	path2=trim(cwd)//'/solution_model.dat'
	call symlnk(path1, path2, status=is)

	if(ox8.eq.0.0E0)then ! In case of 0% H2O.
		write(1,9117)nampro
		write(1,1611)
		write(1,1687)temmin,temmax,premin,premax
		write(1,2411)ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox9
   	write(1,1612)
9117  format(A20)
1611  format('cr_hp02ver.dat',/ &
             ,/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'SIO2',/ &
             'AL2O3',/ &
             'FEO',/ &
             'MGO',/ &
             'CAO',/ &
             'NA2O',/ &
             'K2O',/ &
             'CR2O3',/&
             ,/ &
             '2',/ &
             'n',/ &
             '2',/)
1687  format(2F15.3,/ &
             2F15.3,/ &
             'y')
2611  format(8F8.4)
1612  format('n',/ &
             'y',/ &
             'n',/ &
             'ne',/ &
             'abL',/ &
             'q8L',/ &
             'qGL',/ &
             'coGL',/ &
             'faGL',/ &
             'fo8L',/ &
             'foL',/ &
             'foGL',/ &
             'woGL',/ &
             'nasGL',/ &
             'kalGL',/ &
             'crGL',/ &
             'diL',/ &
             'spr4',/&
             'fcrm',/ &
             'fcrm_i',/ &
             'mcrm',/ &
             'esk',/ &
             ,/ &
             'y',/ &
             'solution_model.dat',/ &
             'Cpx(HP)',/ &
             'Omph(HP)',/ &
             'O(HP)',/ &
             'CrSp',/ &
             'CrGt',/ &
             'CrOpx(HP)',/ &
             'feldspar',/ &
             'Eskol(C)',/ &
             !'melt(HP)',/ &
            ,/ &
            'pro',/)
	close (1)


	elseif(ox8.gt.0.0E0)then ! In case of >0 % H2O.
		write(1,1114)nampro
		write(1,1411)
		write(1,1884)temmin,temmax,premin,premax
		write(1,2411)ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox8,ox9
		write(1,1412)
1114	format(A20)
1411	format('cr_hp02ver.dat',/ &
             ,/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'SIO2',/ &
             'AL2O3',/ &
             'FEO',/ &
             'MGO',/ &
             'CAO',/ &
             'NA2O',/ &
             'K2O',/ &
             'H2O',/ &
             'CR2O3',/&
             ,/ &
             '5',/ &
             '2',/ &
             'n',/ &
             '2',/)
1884  format(2F15.3,/ &
             2F15.3,/ &
            'y')
2411  format(9F8.4)
1412  format('n',/ &
             'y',/ &
             'ne',/ &
             'abL',/ &
             'q8L',/ &
             'qGL',/ &
             'coGL',/ &
             'faGL',/ &
             'foL',/ &
             'fo8L',/ &
             'foGL',/ &
             'woGL',/ &
             'nasGL',/ &
             'kalGL',/ &
             'h2oGL',/ &
             'gl',/ &
             'crGL',/ &
             'spr4',/&
             'diL',/ &
             'fcrm',/ &
             'fcrm_i',/ &
             'mcrm',/ &
             'esk',/ &
             ,/ &
             'y',/ &
             'solution_model.dat',/ &
             'Cpx(HP)',/ &
             'Omph(HP)',/ &
             !'melt(HP)',/ &
             'Chl(HP)',/ &
             'O(HP)',/ &
             'CrSp',/ &
             'CrGt',/ &
             'CrOpx(HP)',/ &
             'Pheng(HP)',/ &
             'Bio(HP)',/ &
             'feldspar',/ &
             ,/ &
             'pro',/)
	endif
	!*************************************************************************





	!*************************************************************************
	! CASE 5
	!	cr_hp17ver database.
	case(5)

!	Create symbolic links.
	path1=trim(perpat)//'/cr_hp17ver.dat'
	path2=trim(cwd)//'/cr_hp17ver.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/perplex_option.dat'
	path2=trim(cwd)//'/perplex_option.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/solution_model.dat'
	path2=trim(cwd)//'/solution_model.dat'
	call symlnk(path1, path2, status=is)

	if(ox8.eq.0.0E0)then ! In case of 0% H2O.
		write(1,19117)nampro
		write(1,11611)
		write(1,11687)temmin,temmax,premin,premax
		write(1,12411)ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox9,ox10,ox11,ox12
   	write(1,11612)
19117  format(A20)
11611  format('cr_hp17ver.dat',/ &
       ,/ &
       'n',/ &
       'n',/ &
       'n',/ &
       'n',/ &
       'SIO2',/ &
       'AL2O3',/ &
       'FEO',/ &
       'MGO',/ &
       'CAO',/ &
       'NA2O',/ &
       'K2O',/ &
       'CR2O3',/ &
       'TIO2',/ &
       'MNO',/ &
       'NIO',/ &
       ,/ &
       '2',/ &
       'n',/ &
       '2',/)
11687  format(2F15.3,/ &
             2F15.3,/ &
              'y')
12411  format(12F8.4)
11612  format('n',/ &
       'y',/ &
       'n',/ &
       'ne',/ &
       'abL',/ &
       'q8L',/ &
       'qGL',/ &
       'coGL',/ &
       'faGL',/ &
       'fo8L',/ &
       'foL',/ &
       'foGL',/ &
       'woGL',/ &
       'nasGL',/ &
       'kalGL',/ &
       'crGL',/ &
       'diL',/ &
       'spr4',/&
       'fcrm',/ &
       'fcrm_i',/ &
       'mcrm',/ &
       'esk',/ &
       'cor',/ &
       'tiGL',/ &
       ,/ &
       'y',/ &
       'solution_model.dat',/ &
       'Omph(HP)',/ &
       'Cpx(HP)',/ &
       'O(HP)',/ &
       'CrSp',/ &
       'Pl(JH)',/ &
       'CrGt',/ &
       'CrOpx(HP)',/ &
       'feldspar',/ &
       'Eskol(C)',/ &
       ,/ &
       'pro',/)
		close (1)

	elseif(ox8.gt.0.0E0)then ! In case of >0 % H2O.
		write(1,11414)nampro
		write(1,11411)
		write(1,11884)temmin,temmax,premin,premax
		write(1,12511)ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox8,ox9
		write(1,11412)
11414	format(A20)
11411	format('cr_hp17ver.dat',/ &
             ,/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'SIO2',/ &
             'AL2O3',/ &
             'FEO',/ &
             'MGO',/ &
             'CAO',/ &
             'NA2O',/ &
             'K2O',/ &
             'H2O',/ &
             'CR2O3',/&
             ,/ &
             '5',/ &
             '2',/ &
             'n',/ &
             '2',/)
11884  format(2F15.3,/ &
             2F15.3,/ &
            'y')
12511  format(9F8.4)
11412  format('n',/ &
             'y',/ &
             'ne',/ &
             'abL',/ &
             'q8L',/ &
             'qGL',/ &
             'coGL',/ &
             'faGL',/ &
             'foL',/ &
             'fo8L',/ &
             'foGL',/ &
             'woGL',/ &
             'nasGL',/ &
             'kalGL',/ &
             'h2oGL',/ &
             'gl',/ &
             'crGL',/ &
             'spr4',/&
             'diL',/ &
             'fcrm',/ &
             'fcrm_i',/ &
             'mcrm',/ &
             'esk',/ &
             ,/ &
             'y',/ &
             'solution_model.dat',/ &
             'Cpx(HP)',/ &
             'Omph(HP)',/ &
             !'melt(HP)',/ &
             'Chl(HP)',/ &
             'O(HP)',/ &
             'CrSp',/ &
             'CrGt',/ &
             'CrOpx(HP)',/ &
             'Pheng(HP)',/ &
             'Bio(HP)',/ &
             'feldspar',/ &
             ,/ &
             'pro',/)
	endif
	!*************************************************************************





	!*************************************************************************
	! CASE 6.
	! hpha02 database.
	case(6)


	! Create symbolic links.
	path1=trim(perpat)//'/hpha02ver.dat'
	path2=trim(cwd)//'/hpha02ver.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/perplex_option.dat'
	path2=trim(cwd)//'/perplex_option.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/solution_model.dat'
	path2=trim(cwd)//'/solution_model.dat'
	call symlnk(path1, path2, status=is)


	if(ox8.eq.0.0E0)then ! In case of 0% H2O.
	write(1,11112)nampro
	write(1,11113)
	write(1,11114) temmin,temmax,premin,premax
	write(1,11115) ox1,ox2,ox3,ox4,ox5,ox6,ox7
   	write(1,11116)
11112 format(A20)
11113 format('hpha02ver.dat',/ &
           'perplex_option.dat',/ &
           'n',/ &
           'n',/ &
           'n',/ &
           'n',/ &
           'SIO2',/ &
           'AL2O3',/ &
           'FEO',/ &
           'MGO',/ &
           'CAO',/ &
           'NA2O',/ &
           'K2O',/ &
           ,/ &
           '2',/ &
           'n',/ &
           '2',/)
11114  format(2F15.3,/ &
             2F15.3,/ &
            'y')
11115  format(7F10.4)
11116  format('n',/ &
              'y',/ &
              'n',/ &
              'ne',/ &
              'abL',/ &
              'ky',/ &
              'and',/ &
              'sil8L',/ &
              'qL',/ &
              'q8L',/ &
              'qGL',/ &
              'coGL',/ &
              'fa',/ &
              'faL',/ &
              'fa8L',/ &
              'faGL',/ &
              'fo',/ &
              'foL',/ &
              'fo8L',/ &
              'foGL',/ &
              'alm',/ &
              'ab',/ &
              'mic',/ &
              'gr',/ &
              'jd',/ &
              'mgts',/ &
              'woGL',/ &
              'nasGL',/ &
              'kalGL',/ &
              'diL',/ &
              'spr4',/ &
              'cor',/ &
              'kals',/ &
              'py',/ &
              'kspL',/ &
              'crst',/ &
              'k2o',/ &
              'na2o',/ &
              ,/ &
              'y',/ &
              'solution_model.dat',/ &
              'Cpx(HP)',/ &
              'O(HP)',/ &
              'Sp(HP)',/ &
              'Pl(JH)',/ &
              'Gt(HP)',/ &
              'Opx(HP)',/ &
              'Omph(HP)',/ &
              'feldspar',/ &
              ,/ &
              'pro',/)

	elseif(ox8.gt.0.0E0)then ! In case of >0% H2O.
	write(1,11117)nampro
	write(1,11118)
	write(1,11119) temmin,temmax,premin,premax
	write(1,11121) ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox8
  write(1,11122)
11117 format(A20)
11118 format('hpha02ver.dat',/ &
           'perplex_option.dat',/ &
           'n',/ &
           'n',/ &
           'n',/ &
           'n',/ &
           'SIO2',/ &
           'AL2O3',/ &
           'FEO',/ &
           'MGO',/ &
           'CAO',/ &
           'NA2O',/ &
           'K2O',/ &
           'H2O',/ &
           ,/ &
           '5',/ &
           '2',/ &
           'n',/ &
           '2',/)
11119  format(2F15.3,/ &
             2F15.3,/ &
            'y')
11121  format(8F10.4)
11122  format('n',/ &
            'y',/ &
            'n',/ &
            'ne',/ &
            'abL',/ &
            'ky',/ &
            'and',/ &
            'sil8L',/ &
            'qL',/ &
            'q8L',/ &
            'qGL',/ &
            'coGL',/ &
            'fa',/ &
            'faL',/ &
            'fa8L',/ &
            'faGL',/ &
            'fo',/ &
            'foL',/ &
            'fo8L',/ &
            'foGL',/ &
            'alm',/ &
            'ab',/ &
            'mic',/ &
            'gr',/ &
            'jd',/ &
            'mgts',/ &
            'woGL',/ &
            'nasGL',/ &
            'kalGL',/ &
            'diL',/ &
            'spr4',/&
            'fgl',/ &
            'gl',/ &
            'parg',/ &
            'ts',/ &
            'east',/ &
            'per',/ &
            'k2o',/ &
            'na2o',/ &
            'fanth',/ &
            'kspL',/ &
            'acti',/ &
            'gl_dqf',/ &
            'osm1',/ &
            'H2O',/ &
            'sud',/ &
            'fst',/ &
            'zo',/ &
            'pa',/ &
            'cals',/ &
            'cor',/ &
            'fanth_dq',/ &
            'ogl_dqf',/ &
            'cz',/ &
            'py',/ &
            'mst',/ &
            'kals',/ &
            'h2oL',/ &
            'fsud',/ &
            'anl',/ &
            'lmt',/ &
            'fctd',/ &
            ,/ &
            'y',/ &
            'solution_model.dat',/ &
            'Omph(HP)',/ &
            'Cpx(HP)',/ &
            'Amph(DHP)',/ &
            'chl(HP)',/ &
            'O(HP)',/ &
            'Sp(HP)',/ &
            ! 'Pl(JH)',/ &
            'Gt(HP)',/ &
            'Opx(HP)',/ &
            ! 'Ctd(HP)',/ &
            'Bio(HP)',/ &
            'feldspar',/ &
            'Pheng(HP)',/ &
            'Bio(TCC)',/ &
            ,/ &
            'pro',/)
	endif
	!*************************************************************************





	!*************************************************************************
	! CASE 7.
	! hpha02 database.
	case(7)


	!	Create symbolic links.
	path1=trim(perpat)//'/hpha02ver.dat'
	path2=trim(cwd)//'/hpha02ver.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/perplex_option.dat'
	path2=trim(cwd)//'/perplex_option.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/solution_model.dat'
	path2=trim(cwd)//'/solution_model.dat'
	call symlnk(path1, path2, status=is)




	if(ox8.eq.0.0E0)then ! In case of 0% H2O.
		write(1,11141)nampro ! name of the project
		write(1,11142)
		write(1,11143) temmin,temmax,premin,premax
		write(1,11144) ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox10
   	write(1,11145)
11141 format(A20)
11142 format('hpha02ver.dat',/ &
             'perplex_option.dat',/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'n',/ &
             'SIO2',/ &
             'AL2O3',/ &
             'FEO',/ &
             'MGO',/ &
             'CAO',/ &
             'NA2O',/ &
             'K2O',/ &
             'TIO2',/&
             ,/ &
             '2',/ &
             'n',/ &
             '2',/)
11143 format(2F15.3,/ &
             2F15.3,/ &
             'y')
11144 format(8F10.4)
11145 format('n',/ &
             'y',/ &
             'n',/ &
             'sil8L',/ &
             'foL',/ &
             'fo8L',/ &
             'foGL',/ &
             'fa8L',/ &
             'q8L',/ &
             'qL',/ &
             'alm',/ &
             'ab',/ &
             'mic',/ &
             'gr',/ &
             'fo',/ &
             'fa',/ &
             'jd',/ &
             'faL',/ &
             'anL',/ &
             'enL',/ &
             'diL',/ &
             'silL',/ &
             'kspL',/ &
             'faGL',/ &
             'nasGL',/ &
             'kalGL',/ &
             'ne',/ &
             ! 'mgts',/ &
             'osm1',/ &
             'per',/ &
             '',/ &
               ,/ &
             'y',/ &
             'solution_model.dat',/ &
             'O(HP)',/ &
             'Opx(HP)',/ &
             'Cpx(HP)',/ &
             'feldspar',/ &
             'Gt(HP)',/ &
             'Sp(HP)',/ &
             'IlHm(A)',/& !Eldar: magnetite
             'Mt(W)',/&   !Eldar: magnetite
             'MtUl(A)',/&   !Eldar: magnetite
             ,/ &
             'pro',/)




	! ! Part added to hanle mantle compositions.
	! if(ox8.eq.0.0E0)then ! In case of 0% H2O.
		! write(1,11141)nampro ! name of the project
		! write(1,11142)
		! write(1,11143) temmin,temmax,premin,premax
		! write(1,11144) ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox10
   	! write(1,11145)
! 11141 format(A20)
! 11142 format('hpha02ver.dat',/ &
             ! 'perplex_option.dat',/ &
             ! 'n',/ &
             ! 'n',/ &
             ! 'n',/ &
             ! 'n',/ &
             ! 'SIO2',/ &
             ! 'AL2O3',/ &
             ! 'FEO',/ &
             ! 'MGO',/ &
             ! 'CAO',/ &
             ! 'NA2O',/ &
             ! 'K2O',/ &
             ! 'TIO2',/&
             ! ,/ &
             ! '2',/ &
             ! 'n',/ &
             ! '2',/)
! 11143 format(a14,1X,a14,/ &
             ! a14,1X,a14,/ &
             ! 'y')
! 11144 format(8F10.4)
! 11145 format('n',/ &
             ! 'y',/ &
             ! 'n',/ &
             ! 'sil8L',/ &
             ! 'foL',/ &
             ! 'fo8L',/ &
             ! 'foGL',/ &
             ! 'fa8L',/ &
             ! 'q8L',/ &
             ! 'qL',/ &
             ! 'faL',/ &
             ! 'anL',/ &
             ! 'enL',/ &
             ! 'diL',/ &
             ! 'silL',/ &
             ! 'kspL',/ &
             ! 'faGL',/ &
             ! 'nasGL',/ &
             ! 'kalGL',/ &
             ! 'ne',/ &
             ! 'per',/ &
             ! '',/ &
               ! ,/ &
             ! 'y',/ &
             ! 'solution_model.dat',/ &
             ! 'Cpx(HP)',/ &
             ! 'O(HP)',/ &
             ! 'Sp(HP)',/ &
             ! 'Pl(JH)',/ &
             ! 'Gt(HP)',/ &
             ! 'Opx(HP)',/ &
             ! 'Sapp(HP)',/&
             ! 'Osm(HP)',/&
             ! 'feldspar',/&
             ! ,/ &
             ! 'pro',/)



      elseif(ox8.gt.0.0E0)then ! In case of >0% H2O.
         write(1,11146)nampro
         write(1,11147)
         write(1,11148) temmin,temmax,premin,premax
         write(1,11149) ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox8
         write(1,11150)
11146    format(A20)
11147    format('hpha02ver.dat',/ &
                'perplex_option.dat',/ &
                'n',/ &
                'n',/ &
                'n',/ &
                'n',/ &
                'SIO2',/ &
                'AL2O3',/ &
                'FEO',/ &
                'MGO',/ &
                'CAO',/ &
                'NA2O',/ &
                'K2O',/ &
                'H2O',/ &
                ,/ &
                '5',/ &
                '2',/ &
                'n',/ &
                '2',/)
11148    format(2F15.3,/ &
             2F15.3,/ &
                'y')
11149    format(8F10.4)
11150    format('n',/ &
                'y',/ &
             'n',/ &
             'sil8L',/ &
             'foL',/ &
             'fo8L',/ &
             'fa8L',/ &
             'q8L',/ &
             'qL',/ &
             'alm',/ &
             'ab',/ &
             'mic',/ &
             'ne',/ &
             'gr',/ &
             'fo',/ &
             'fa',/ &
             'jd',/ &
             'fgl',/ &
             'gl',/ &
             'parg',/ &
             'ts',/ &
             'east',/ &
             'faL',/ & !Eldar
             'anL',/ & !Eldar
             'enL',/ & !Eldar
             'diL',/ & !Eldar
             'silL',/ & !Eldar
             'kspL',/ &
             'H2O',/ &
             'abL',/ &
             'qGL',/ &
             'faGL',/ &
             'h2oL',/ &
             'ne',/ &
             ! 'mgts',/ &
             'osm1',/ &
             ,/ &
             'y',/ &
             'solution_model.dat',/ &
             'O(HP)',/ &
             ! 'Opx(HP)',/ &
             'Opx(JH)',/ &
             'Cpx(HP)',/ &
             'feldspar',/ &
             'Gt(HP)',/ &
             'Sp(HP)',/ &
             'Amph(DHP)',/ &
             'Pheng(HP)',/ &
             'Bio(TCC)',/ &
             'IlHm(A)',/& !Eldar: magnetite
             'Mt(W)',/&   !Eldar: magnetite
             'MtUl(A)',/&   !Eldar: magnetite
             ,/ &
             'pro',/)
      endif
	!*************************************************************************





!*************************************************************************
!	cr_hp02_ZK1.dat     (database).
!	crsol_ZK1.dat       (solution model).
	case(8)

!	Create symbolic links.
	path1=trim(perpat)//'/cr_hp02_ZK1.dat'
	path2=trim(cwd)//'/cr_hp02_ZK1.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/perplex_option.dat'
	path2=trim(cwd)//'/perplex_option.dat'
	call symlnk(path1, path2, status=is)
	path1=trim(perpat)//'/crsol_ZK1.dat'
	path2=trim(cwd)//'/crsol_ZK1.dat'
	call symlnk(path1, path2, status=is)

	if(ox8.eq.0.0E0)then ! In case of 0% H2O.
		write(1,9118)nampro
		write(1,1712)
		write(1,1688)temmin,temmax,premin,premax
		write(1,2412)ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox9
   	write(1,1613)
9118  format(A20)
1712  format('cr_hp02_ZK1.dat',/ &
            ,/ &
            'n',/ &
            'n',/ &
            'n',/ &
            'n',/ &
            'SIO2',/ &
            'AL2O3',/ &
            'FEO',/ &
            'MGO',/ &
            'CAO',/ &
            'NA2O',/ &
            'K2O',/ &
            'CR2O3',/&
             ,/ &
            '2',/ &
            'n',/ &
            '2',/)
1688	format(2F15.3,/ &
             2F15.3,/ &
             'y')
2412	format(8F8.4)
1613	format('n',/ &
             'y',/ &
             'n',/ &
             'fcrm',/ &
             ,/ &
             'y',/ &
             'crsol_ZK1.dat',/ &
             'CrGt',/ &
             'CrOpx(HP)',/ &
             'cCpx(HP)',/ &
             'CrSp',/ &
             'O(HP)',/ &
             ,/ &
             'pro',/)
	endif
	!*************************************************************************


	end select




	close(1)




		!	Write input for vertex.
      open(2,file='inpver.txt')
      write(2,1000)nampro
1000  format(A20)
      write (*,*)''
      close(2)




      ! Write the input for werami. First call.
		if(ox8.eq.0.0E0)then ! In case of 0% H2O.
			open(4,file="inpwer1.txt")
			write(4,1000)nampro
			write(4,2010)
			write(4,2111)nT,nP
			write(4,2112)
2010		format('2',/ &
                '36',/ &
                '3',/ &
                'n',/)
2111		format(2I7) ! Number of nodes in T and P directions.
2112		format('0',/)
			close(4)
		elseif(ox8.gt.0.0E0)then ! In case of >0% H20.
			open(4,file="inpwer1.txt")
			write(4,1000)nampro
			write(4,2017)
			write(4,2018)nT,nP
			write(4,2119)
2017		format('2',/ &
                '36',/ &
                '3',/ &
                !'n',/ &
                'n',/)
2018		format(2I7) ! Number of nodes in T and P directions.
2119		format('0',/)
			close(4)
		endif




		! Write the input for werami. Second call.
		open(4,file="inpwer2.txt")
		write(4,1000) nampro
		write(4,11111)
		write(4,11151) temref, preref
		write(4,11152)
11111 format('1')
11151 format(2F15.3)
11152 format('99 99',/ &
                '0',/)
		close(4)




      ! Write the input for werami. Third call.
      open(4,file="inpwer3.txt")
      if(ox8.eq.0.0E0)then ! In case of 0% H2O.
				write(4,1000)nampro
				write(4,2150)
				write(4,2151)nT,nP
				write(4,2152)
2150    format('2',/ &
               '2',/ &
               'n',/ &
               '13',/ &
               'n',/ &
               '14',/ &
               'n',/ &
               '15',/ &
               'n',/ &
               '0',/ &
               'n',/)
2151    format(2I7) ! Number of nodes in T and P directions.
2152    format('0',/)
		  elseif(ox8.gt.0.0E0)then ! In case of >0% H20.
        write(4,1000)nampro
        write(4,2057)
        write(4,2058)nT,nP
        write(4,2159)
2057		format('2',/ &
               '2',/ &
               'n',/ &
               'n',/ &
               '13',/ &
               'n',/ &
               'n',/ &
               '14',/ &
               'n',/ &
               'n',/ &
               '15',/ &
               'n',/ &
               'n',/ &
               '0',/ &
               'n',/)
2058		format(2I7) ! Number of nodes in T and P directions.
2159		format('0',/)
      endif
      close(4)





	!*************************************************************************
	! Run peplex_x
	if(flag1==1)then ! mattia
	   call system('./build < inpbui.txt')
	   call system('./vertex < inpver.txt')
	   call system('./werami < inpwer1.txt')
	   call system('./werami < inpwer2.txt')
	   call system('./werami < inpwer3.txt')
	endif
	!*************************************************************************





	! Get the _sys LitMod table.
	str1='_1.phm'
	str11='_sys.dat'
	file1 = trim(nampro)//trim(str1)
	file11 = trim(nampro)//trim(str11)
	open(50,file=file1)
	open(51,file=file11)
	ny=nP*nT
	write(51,1066)ny
1066  format(I7)
	write(51,*)''
	read(50,*)head(1:11)
	read(50,*)nx
	nx=nx-2 ! exclude first two colums (name phases, number of phases).
	read(50,*)head(13)
	allocate(syspro(nx,ny))
	if(met==0)then ! EQUILIBRIUM CONDITION =========================
		do ll=1,ny
			read(50,'(A12)',advance='no')nam1
			read(50,*)co1,syspro(:,ll)
			do m=1,co1 ! skip the lines with phases properties
				read(50,*)
			enddo
			if(isnan(syspro(26,ll)))then ! in case of NaN
				write(51,1099)syspro(1,ll),syspro(2,ll),'0','0','0','0','0'
1099  		format(1F11.3,1F17.4,5A2)
			else  ! in case of not NaN
				write(51,1067)syspro(1,ll),syspro(2,ll),syspro(25,ll),syspro(26,ll),&
								  syspro(12,ll),syspro(9,ll),syspro(10,ll)
1067			format(1F11.3,1F17.4,2F14.10,1F10.3,2F7.3)
			endif
		enddo
	elseif(met==1)then ! METASTABLE CONDITION =========================
		call reawerout(nampro)
		do ll=1,ny
			read(50,'(A12)',advance='no')nam1
			read(50,*)co1,syspro(:,ll)
			do m=1,co1 ! Skip the lines with phases properties.
				read(50,*)
			enddo
			pre = syspro(2,ll)
			tem = syspro(1,ll)
      call compro(pre, tem, densys, vpsys, vssys)
			write(51,1167) syspro(1,ll), syspro(2,ll), syspro(25,ll), syspro(26,ll), densys, vpsys, vssys
1167		format(1F20.3, 1F20.4, 2F20.10, 3F20.3)
		enddo
		deallocate(namphatxt)
		deallocate(phacomtxt)
	endif
	deallocate(syspro)
	close(50)
	close(51)




	! Get the _phases LitMod table.
	str2='_1.phm'
	str22='_phases.dat'
	file2  = trim(nampro)//trim(str2)
	file22 = trim(nampro)//trim(str22)
	open(70,file=file2)
	open(71,file=file22)
	read(70,*)head(1:11)
	read(70,*)nx
	nx=nx-2 ! exclude first two columns
	read(70,*)head(13)
	if(met==0)then ! EQUILIBRIUM CONDITION ----------------------------------
			allocate(ar1(nx))
			do l=1,ny
				read(70,'(A12)',advance='no')nam1
				read(70,*)co1,ar1(:)
				allocate(nampha1(co1))
				allocate(phapro1(nx,co1))
				do m=1,co1
          read(70,'(A10)',advance='no')nampha1(m)
					read(70,*)co1,phapro1(:,m)
					totwei1=phapro1(19,m) ! N,g
					feofac=phapro1(36,m)  ! FEO,wt%
          mgofac=phapro1(37,m)  ! MGO,wt%
					feoper = feowei*feofac / totwei1 *100
					mgoper = mgowei*mgofac / totwei1 *100
          phapro1(36,m)=feoper ! FEO %
					phapro1(37,m)=mgoper ! MGO %
				enddo
				if(isnan(ar1(26)))then ! Write zeros in case of NaNs.
					write(71,1068)'1','',ar1(1),ar1(2)
1068     		format(2A1,1F11.3,1F17.4)
					do m=1,co1
						write(71,1071)'0','',nampha1(m),'0','0','0','0','0','0','0'
1071					format(1A1,1A1,1A10,7A2)
					enddo
				else ! Write properties.
					write(71,2068)'1','',ar1(1),ar1(2)
2068				format(2A1,1F11.3,1F17.4)
					do m=1,co1
						write(71,2071)'0','',nampha1(m),phapro1(32,m),phapro1(31,m),& ! 31volfrac 36wtFe 37wtMg
						phapro1(33,m),&
            phapro1(34,m),' ',phapro1(35,m),' ',phapro1(36,m),' ',phapro1(37,m)
2071  format(1A1,1A1,1A10,3F9.5,1F25.10,1A1,1F25.10,1A1,1F25.10,1A1,1F25.10)
					enddo
				endif
				deallocate(nampha1)
				deallocate(phapro1)
			enddo
	elseif(met==1)then ! METASTABLE CONDITION =========================
			call reawerout(nampro)
			feofac = phacomtxt(7,m) ! feo wt%
			mgofac = phacomtxt(8,m) ! mgo wt%
			feoper = feowei*feofac / totwei2 * 100
			mgoper = mgowei*mgofac / totwei2 * 100
			allocate(ar1(nx))
			do l=1,ny
				read(70,'(A12)',advance='no')nam1
				read(70,*)co1,ar1(:)
				do m=1,co1
					read(70,*) ! Skip the lines with phase properties.
				enddo
				write(71,4068)'1','',ar1(1),ar1(2)
4068			format(2A1,1F11.3,1F17.4)
				do m=1,numphatxt
					write(71,4071)'0','',namphatxt(m),phacomtxt(1,m),phacomtxt(2,m),& ! 1]vol.frac. 7]wtFe 8]wtMg
					phacomtxt(5,m),phacomtxt(6,m),phacomtxt(9,m),phacomtxt(7,m),phacomtxt(8,m)
4071				format(1A1,1A1,1A10,3F9.5,4F15.10)
				enddo
			enddo
	endif
	deallocate(ar1)
	close(70)
	close(71)




end program

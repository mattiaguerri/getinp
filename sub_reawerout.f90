



!===============================================================================
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!                        Subroutine reawerout
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!===============================================================================


!!!=============================================================================
!!! Last check!: Mattia Guerri, November 2017!!!
!!!=============================================================================


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Read the werami outputs (one for each composition) at specified conditions.
!!!
!!! Input:
!!!       - nampro = Name of the Perple_X project.
!!!
!!! Output:
!!!       - numox = Number of oxides.
!!!       - numphatxt = Number of phases.
!!!
!!! Requirements:
!!!       - module var
!!!       - module strings
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine reawerout(nampro, numox, numphatxt)


use var
use strings


implicit none


integer numhea,con1
integer numphatxt
real(8)totwei2
character(100)nampro,namfil
character(150)nam1
character(16)nam2
character(10)nam3
character(len=1) :: delims
integer, parameter :: StrMax=10, Nmax = 20
character(len=StrMax), dimension(Nmax) :: args
integer :: nargs
integer nrid
integer l
integer numox


namfil=trim(nampro)//'_1.txt'
open(11111,file=namfil)


   ! Get rid of the header.
	con1=0
	do
		read(11111,*)nam1
		con1=con1+1
		if(nam1.eq.'Phase')then
			exit
		endif
	enddo


   ! Get the number of oxides.
	read(11111,'(A)')nam1
	delims = ' '
	nrid=7
	call parse(nam1, delims, args, nargs)
	numox=nargs-nrid


   ! Get the number of phases.
	numphatxt=0
	do
		read(11111,*)nam1
		if(nam1.eq.'Phase' .or. nam1.eq.'Molar')then
			exit
		endif
		numphatxt=numphatxt+1
	enddo


	allocate(namphatxt(numphatxt))


	! Get phases names and compositions.
	rewind(11111)
	do l=1,(con1+1)
		read(11111,*)nam1
	enddo


	allocate(phacomtxt((numox+4),numphatxt))


	do l=1,numphatxt
		read(11111,*) namphatxt(l), phacomtxt(:,l)
	enddo


	! Get molecular weight of the system.
	do
		read(11111,*)nam1
		if(nam1=='Molar')then
			exit
		endif
	enddo
	read(11111,*)nam1
	do l=1,numphatxt
		read(11111,*)nam1
	enddo
	read(11111,*)nam1,totwei2


	close(11111)


	end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!===============================================================================

!!! reawerout

!!! Read the werami output at specified PT conditions.
!!!
!!! Input:
!!!       - nampro = Name of the Perple_X project.
!!!
!!! Requirements:
!!!       - module var
!!!       - module strings


subroutine reawerout(nampro)


use var
use strings


implicit none


integer numhea,con1
real(8) totwei2
real(8) rea1
character(100) nampro, namfil
character(150) nam1
character(16) nam2
character(10) nam3
character(len=150) :: nam4
character(len=6) :: nam5
character(len=1) :: delims
integer, parameter :: StrMax=10, Nmax = 20
character(len=StrMax), dimension(Nmax) :: args
integer :: nargs
integer nrid
integer l, m, num1, rind, cind
integer, dimension(:), allocatable :: indsolid


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


	if (.not. allocated(phacomtxt)) allocate(phacomtxt((numox+4),numphatxt))


	do l=1,numphatxt
		read(11111,*) namphatxt(l), phacomtxt(:,l)
	enddo


	! Get end-members ratio of the phases.

  ! Allocate the end-members ratio.
  if (.not. allocated(endmemrat)) allocate(endmemrat(numphatxt, maxendmem_td))

  ! Get the index of the phases with end-members.
  allocate(indsolid(numphatxt))
  indsolid = 0
  m = 1
	do
		read(11111,*) nam1
		if(nam1=='Phase')then
		  exit
    endif
  enddo
  do
    read(11111,*) nam1
    do l = 1,numphatxt
      if (nam1==namphatxt(l)) then
        indsolid(m) = l
        m = m + 1
        exit
      endif
    enddo
    if (nam1=='Molar') then
      exit
    endif
  enddo

  close(11111)
  open(11111,file=namfil)

  ! Initialize all phases as made off only the first end-member.
  endmemrat(:,:) = 0.
  endmemrat(:,1) = 1.
	do
		read(11111,*) nam1, nam1
		if(nam1=='speciation')then
      exit
    endif
  enddo
  read(11111,'(A)') nam4
  rind = 0
  do
    read(11111,'(A)') nam4
    nam4(len(nam4):len(nam4)) = '_'
    if (trim(nam4(1:6))=='Molar') then
      exit
    else
      rind = rind + 1
      cind = 0
      do l = 1, len(nam4)
        if (nam4(l:l)==':') then
          cind = cind + 1
          nam5(1:6) = nam4(l+3:l+8)
          read(nam5,*) endmemrat(indsolid(rind), cind)
        elseif(nam4(l:l)=='_')then
          exit
        endif
      enddo
    endif
	enddo


	close(11111)
  open(11111,file=namfil)


	! Get molecular weight of the system.
	do
		read(11111,*) nam1
		if(nam1=='Molar')then
			exit
		endif
	enddo
	read(11111,*) nam1
	do l=1,numphatxt
		read(11111,*) nam1
	enddo
	read(11111,*) nam1, totwei2


	close(11111)


	end subroutine



!===============================================================================
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module var


  !!! Weight of iron and magnesium oxide.
	real, parameter :: feowei=71.8440, mgowei=40.3040


	!!! numpha_td =  Number of phases in the thermodynamic database.
  !!! maxendmem_td = Maximum number of end-members that each phase can have.
	integer, parameter :: numpha_td=20, maxendmem_td=7


  !!! Name of the various phases contained in the thermodynamic database.
	character(10)nampha_td(numpha_td)


  !!! Name of the phases read in the XXX_1.txt file.
  !!! This are the phases that are stable at a certain PT condition.
	character(len=10), dimension(:), allocatable :: namphatxt
	character(len=10), dimension(:,:), allocatable :: namphatxt_all


  !!! Composition of the phases in the XXX_1.txt file.
	real(8), dimension(:,:), allocatable :: phacomtxt
  real(8), dimension(:,:,:), allocatable :: phacomtxt_all


	real(8)alpha(maxendmem_td,numpha_td),vol0(maxendmem_td,numpha_td),kt0(maxendmem_td,numpha_td),molwei(maxendmem_td,numpha_td),&
         k0(maxendmem_td,numpha_td),k1(maxendmem_td,numpha_td),k2(maxendmem_td,numpha_td),&
         m0(maxendmem_td,numpha_td),m1(maxendmem_td,numpha_td),m2(maxendmem_td,numpha_td)


end module


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!===============================================================================

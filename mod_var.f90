! Module containing variables shared by the main and some subroutines.

module var


  ! Weight of iron and magnesium oxide.
	real, parameter :: feowei=71.8440, mgowei=40.3040


	! Number of phases in the thermodynamic database.
	integer, parameter :: numpha_td = 22

  ! Maximum number of end-members that each phase can have.
	integer, parameter :: maxendmem_td=7


  ! Name of the various phases contained in the thermodynamic database.
	character(10)nampha_td(numpha_td)


  integer numphatxt


  ! Name of the phases read in the *_1.txt file.
  ! This are the phases that are stable at a certain PT condition.
	character(len=10), dimension(:), allocatable :: namphatxt
	character(len=10), dimension(:,:), allocatable :: namphatxt_all


  ! Composition of the phases in the *_1.txt file.
	real(8), dimension(:,:), allocatable :: phacomtxt
  real(8), dimension(:,:,:), allocatable :: phacomtxt_all


	real(8), dimension(:), allocatable :: volref
	real(8), dimension(:), allocatable :: molref


  ! End members ratio, from the *txt file.
  real(8), dimension(:,:), allocatable :: endmemrat


  integer numox
  real(8) ox1, ox2, ox3, ox4, ox5, ox6, ox7, ox8, ox9, ox10, ox11, ox12


end module

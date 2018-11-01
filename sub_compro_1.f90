
    
subroutine compro_1(numphatxt, namphatxt, numox, molref, volref, phacomtxt, endmemrat, numpha_td, maxendmem_td,&
                    pre, tem, vpsys, vssys, densys)


implicit none



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Parameters and Variables.
real, parameter :: presta=1.0, temsta=298.15 ! pre(bar) tem(K), standard values
integer, value :: numphatxt
character(len=10), dimension(numphatxt) :: namphatxt
integer, value :: numox
real(8), dimension(numphatxt) :: molref
real(8), dimension(numphatxt) :: volref
real(8), dimension((numox+4),numphatxt) :: phacomtxt
integer, value :: numpha_td
integer, value :: maxendmem_td
real(8) endmemrat(maxendmem_td, numphatxt)
character(10) namphatd(numpha_td) !!! Name of the various phases contained in the thermodynamic database.
real(8) alpha(maxendmem_td, numpha_td), vol0(maxendmem_td,numpha_td), kt0(maxendmem_td, numpha_td),&
        molwei(maxendmem_td,numpha_td), k0(maxendmem_td,numpha_td), k1(maxendmem_td,numpha_td),&
        k2(maxendmem_td,numpha_td), m0(maxendmem_td,numpha_td), m1(maxendmem_td,numpha_td), m2(maxendmem_td,numpha_td)
real(8) volt(maxendmem_td,numpha_td),kt(maxendmem_td,numpha_td),volpt(maxendmem_td,numpha_td)
real(8) ksendmem(maxendmem_td,numpha_td)
real(8) musendmem(maxendmem_td,numpha_td)
real(8) molweipha(numphatxt)
real(8) volpha(numphatxt)
real(8) kspha(numphatxt)
real(8) muspha(numphatxt)
real(8) volsys, molweisys, kssys, kssysr, kssysv
real(8) mussys, mussysr, mussysv
real(8) vpsys, vssys, densys !!! Properties (of the bulk rock) computed for each composition.
real(8), value :: pre, tem
integer l, m
character(len=10)nam1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


include 'thedat91.h'


!!! Calculate phacomtxt(4,:) according to the change in volume percentages.
phacomtxt(4,:) = (phacomtxt(2,:)-volref(:))/volref(:) * molref(:) + molref(:)


!!! Get volume at pre tem for each endmem in the thermodynamic database.
do l=1,numpha_td
  volt(:,l)  = vol0(:,l) * (1. + alpha(:,l)*(tem-temsta) - 20.*alpha(:,l)*(sqrt(tem)-sqrt(temsta)))
  kt(:,l)    = kt0(:,l) * ( 1. - 1.5 * 1e-4 * (tem-temsta) )
  volpt(:,l) = volt(:,l) * ( 1. - (4.*pre) / (kt(:,l) + 4. * pre) )**0.25
enddo


!!! Get adiabatic bulk modulus at pre tem for each end-member in the thermodynamic database.
do l=1,numpha_td
  ksendmem(:,l)=k0(:,l)+k1(:,l)*(pre-presta)+k2(:,l)*(tem-temsta)
enddo


!!! Get adiabatic shear modulus at pre tem for each end-member in the thermodynamic database.
do l=1,numpha_td
  musendmem(:,l)=m0(:,l)+m1(:,l)*(pre-presta)+m2(:,l)*(tem-temsta)
enddo


!!! Get molecular weight for each phase.
do l=1,numphatxt
  do m=1,numpha_td
    nam1=trim(namphatxt(l))
    if(nam1==namphatd(m))then
      molweipha(l) = sum( molwei(:,m)*endmemrat(:,l) )
    endif
  enddo
enddo


!!! Get volume for each phase.
do l=1,numphatxt
  do m=1,numpha_td
    nam1=trim(namphatxt(l))
    if(nam1==namphatd(m))then
      volpha(l) = sum( volpt(:,m)*endmemrat(:,l) )
    endif
  enddo
enddo


!!! Get adiabatic bulk modulus for each phase.
do l=1,numphatxt
  do m=1,numpha_td
    nam1=trim(namphatxt(l))
    if(nam1==namphatd(m))then
      kspha(l) = sum( ksendmem(:,m)*endmemrat(:,l) )
    endif
  enddo
enddo


!!! Get adiabatic shear modulus for each phase.
do l=1,numphatxt
  do m=1,numpha_td
    nam1=trim(namphatxt(l))
    if(nam1==namphatd(m))then
      muspha(l) = sum( musendmem(:,m)*endmemrat(:,l) )
    endif
  enddo
enddo


!!! Get molecular weight for the system.
molweisys = sum( molweipha(:)*phacomtxt(4,:) )

! !!! DM
! !!! compute volref from molref for a new mixture
! !!! compute wt % from molar %
! NOTE: phacomtxt(4,:) is not normalized
! phacomtxt(1,:) = molweipha(:) * phacomtxt(3,:) / sum(molweipha(:) * phacomtxt(3,:))
! !!! compute vol % from wt %
! phacomtxt(2,:) = 


!!! Get volume for the system at pre tem.
volsys = sum( volpha(:)*phacomtxt(4,:) )


!!! voigtreusshill of the bulk moduli.
kssysr = (sum(phacomtxt(2,:)/100/kspha(:)))**(-1)
kssysv = sum(phacomtxt(2,:)/100*kspha(:))
kssys = (kssysr+kssysv)/2


!!! voigtreusshill of the shear moduli.
mussysr = (sum(phacomtxt(2,:)/100/muspha(:)))**(-1)
mussysv = sum(phacomtxt(2,:)/100*muspha(:))
mussys = (mussysr+mussysv)/2


!!! Get density for the system.
densys = (molweisys*1e-3)/(volsys*1e-5) ! kg/m^3


!!! Get Vp for the system.
vpsys=(sqrt((kssys*1D05+4./3.*mussys*1D05)/densys))*1e-3


!!! Get Vs for the system.
vssys=(sqrt(mussys*1e05/densys))*1e-3



! open(1, file='compro/outcomp.txt', form='formatted', status='replace', action='write')
! 
! write(1,*)'test'
! 
! 
! do m=1,numphatxt
!     !                  wt %      vol %     mol %     mol        SIO2     AL2O3    FEO      MGO      CAO      NA2O     K2O  
!     !Opx               21.21     16.70     17.02    0.898E-01  2.00000  0.00000  1.05953  0.94047  0.00000  0.00000  0.00000
!     write(1, *) namphatxt(m), phacomtxt(:,m) 
! enddo
! 
! do m=1,numphatxt
!     ! Opx               en: 0.47024, fs: 0.52976, opx: 0.00000
!     write(1, *) endmemrat(1,m), endmemrat(2,m), endmemrat(3,m) 
! enddo
! do m=1,numphatxt
!     write(1,*) molref(m)
! enddo
! do m=1,numphatxt
!     write(1,*) volref(m)
! enddo
! 
! 
! write(1,*)''
! write(1,*)pre
! write(1,*)tem
! write(1,*)vpsys
! write(1,*)vssys
! write(1,*)densys
! write(1,*)''
! close(1)

end subroutine





    
! program comprotest
! 
! integer, parameter :: numpha_td = 21, maxendmem_td = 7
! integer numphatxt, numox 
! character(len=10), allocatable :: namphatxt(:) !dimension(numphatxt)
! real(8), allocatable :: molref(:) !dimension(numphatxt)
! real(8), allocatable :: volref(:) !dimension(numphatxt)
! real(8), allocatable :: phacomtxt(:,:) !dimension((numox+4),numphatxt)
! real(8), allocatable :: endmemrat(:,:) !(maxendmem_td, numphatxt)
! real(8) pre, tem, vpsys, vssys, densys
! character(len=10) dummy
! 
! pre = 5000.0 !1.0 !
! tem = 700.0 !298.15 !
! numox = 7
! numphatxt = 6
! 
! allocate(namphatxt(numphatxt), molref(numphatxt), volref(numphatxt), phacomtxt(numox+4, numphatxt), &
!     endmemrat(maxendmem_td, numphatxt))
! 
! 
! open(1, file='test.txt', form='formatted', status='old', action='read')
! 
! do i=1,9
!     read(1, *)
! enddo
! do i=1,numphatxt
!     !                  wt %      vol %     mol %     mol        SIO2     AL2O3    FEO      MGO      CAO      NA2O     K2O  
!     !Opx               21.21     16.70     17.02    0.898E-01  2.00000  0.00000  1.05953  0.94047  0.00000  0.00000  0.00000
!     read(1, *) namphatxt(i), phacomtxt(:,i) 
! enddo
! do i=1,3
!     read(1, *)
! enddo
! do i=1,numphatxt-1
!     ! Opx               en: 0.47024, fs: 0.52976, opx: 0.00000
!     read(1, *) dummy, dummy, endmemrat(1,i), dummy, endmemrat(2,i), dummy, endmemrat(3,i) 
! enddo
! endmemrat(1,numphatxt) = 1
! 
! molref(:) = phacomtxt(3,:)
! volref(:) = phacomtxt(2,:)
! 
! close(1)
! 
! do i=1,numphatxt
! 
!    ! write(*,*) namphatxt(i), volref(i), molref(i), phacomtxt(:,i)
!     write(*,*) namphatxt(i), endmemrat(1,i), endmemrat(2,i), endmemrat(3,i)
! enddo
! 
! 
! call compro_1(numphatxt, namphatxt, numox, molref, volref, phacomtxt, endmemrat, numpha_td, maxendmem_td,&
!                     pre, tem, vpsys, vssys, densys)
!  !compro(numphatxt, namphatxt, numox, phacomtxt, endmemrat, numpha_td, maxendmem_td, pre, tem, vpsys, vssys, densys)
! 
!  write(*,*)''
!  write(*,*)'Vp = ', vpsys
!  write(*,*)'Vs = ', vssys
!  write(*,*)'Dens = ', densys
!  write(*,*)''
! 
!  
! end program


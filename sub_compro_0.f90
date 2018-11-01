



!!!=============================================================================

!   Subroutine compro

!!!=============================================================================

!!!=============================================================================
!!! Compute properties for metastable mineralogies.
!!! Compute the properties considering fixed mineralogy.

!!! It requires the module var in mod_var.f90.
!!!    namphatxt and phacomtxt are inside this module.
!!!    namphatxt = names of the phases in the reference mineralogy.
!!!    phacomtxt = composition of the phases in the reference mineralogy.

!!! Input:
!!!   - numphatxt = number of phases in the reference mineralogy.
!!!   - pre       = Pressure in Bar.
!!!   - tem       = Temperature in Kelvin.

!!! Output:
!!!   - vpsys  = Vp of the system (km/s)
!!!   - vssys  = Vs of the system (km/s)
!!!   - densys = Density of the system (kg/m3)
!!!=============================================================================


subroutine compro(numphatxt,namphatxt,numox,phacomtxt,endmemrat,numpha_td,maxendmem_td,pre,tem,vpsys,vssys,densys)


implicit none


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Parameters and Variables.

real, parameter :: presta=1.0, temsta=298.15 ! pre(bar) tem(K), standard values

integer numphatxt
character(len=10), dimension(numphatxt) :: namphatxt
integer numox
real(8), dimension((numox+4),numphatxt) :: phacomtxt
integer numpha_td
integer maxendmem_td
real(8) endmemrat(maxendmem_td, numphatxt)

!!! Name of the various phases contained in the thermodynamic database.
character(10)nampha_td(numpha_td)

real(8)alpha(maxendmem_td,numpha_td),vol0(maxendmem_td,numpha_td),kt0(maxendmem_td,numpha_td),molwei(maxendmem_td,numpha_td),&
       k0(maxendmem_td,numpha_td),k1(maxendmem_td,numpha_td),k2(maxendmem_td,numpha_td),&
       m0(maxendmem_td,numpha_td),m1(maxendmem_td,numpha_td),m2(maxendmem_td,numpha_td)

real(8)volt(maxendmem_td,numpha_td),kt(maxendmem_td,numpha_td),volpt(maxendmem_td,numpha_td)

real(8)ksendmem(maxendmem_td,numpha_td)

real(8)musendmem(maxendmem_td,numpha_td)

real(8) molweipha(numphatxt)
real(8) volpha(numphatxt)
real(8) kspha(numphatxt)
real(8) muspha(numphatxt)

real(8) volsys, molweisys, kssys, kssysr, kssysv
real(8) mussys, mussysr, mussysv

!!! Properties (of the bulk rock) computed for each composition.
real(8) vpsys, vssys, densys

real(8) pre, tem

integer l, m
character(len=10)nam1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


include 'thermodynamic_dataset.h'

	!!! Get volume at pre tem for each endmem in the thermodynamic database.
	do l=1,numpha_td
		do m=1,maxendmem_td
     volt(m,l)  = vol0(m,l) * (1. + alpha(m,l)*(tem-temsta) - 20.*alpha(m,l)*(sqrt(tem)-sqrt(temsta)))
		kt(m,l)    = kt0(m,l) * ( 1. - 1.5 * 1e-4 * (tem-temsta) )
		volpt(m,l) = volt(m,l) * ( 1. - (4.*pre) / (kt(m,l) + 4. * pre) )**0.25
		enddo
	enddo

	!!! Get adiabatic bulk modulus at pre tem for each end-member in the thermodynamic database.
	do l=1,numpha_td
		do m=1,maxendmem_td
			ksendmem(m,l)=k0(m,l)+k1(m,l)*(pre-presta)+k2(m,l)*(tem-temsta)
		enddo
	enddo

	!!! Get adiabatic shear modulus at pre tem for each end-member in the thermodynamic database.
	do l=1,numpha_td
		do m=1,maxendmem_td
		musendmem(m,l)=m0(m,l)+m1(m,l)*(pre-presta)+m2(m,l)*(tem-temsta)
		enddo
	enddo

	!!! Get molecular weight for each phase.
	! allocate(molweipha(numphatxt))
	do l=1,numphatxt
		do m=1,numpha_td
			nam1=trim(namphatxt(l))
			if(nam1==nampha_td(m))then
	molweipha(l)=molwei(1,m)*endmemrat(1,l)+molwei(2,m)*endmemrat(2,l)+molwei(3,m)*endmemrat(3,l)+&
				    molwei(4,m)*endmemrat(4,l)+molwei(5,m)*endmemrat(5,l)+molwei(6,m)*endmemrat(6,l)+&
					 molwei(7,m)*endmemrat(7,l)
			endif
		enddo
	enddo

	!!! Get volume for each phase.
	! allocate(volpha(numphatxt))
	do l=1,numphatxt
		do m=1,numpha_td
			nam1=trim(namphatxt(l))
			if(nam1==nampha_td(m))then
	volpha(l)=volpt(1,m)*endmemrat(1,l)+volpt(2,m)*endmemrat(2,l)+volpt(3,m)*endmemrat(3,l)&
				 +volpt(4,m)*endmemrat(4,l)+volpt(5,m)*endmemrat(5,l)+volpt(6,m)*endmemrat(6,l)&
				 +volpt(7,m)*endmemrat(7,l)
			endif
		enddo
	enddo

	!!! Get adiabatic bulk modulus for each phase.
	! allocate(kspha(numphatxt))
	do l=1,numphatxt
		do m=1,numpha_td
			nam1=trim(namphatxt(l))
			if(nam1==nampha_td(m))then
	kspha(l)=ksendmem(1,m)*endmemrat(1,l)+ksendmem(2,m)*endmemrat(2,l)+ksendmem(3,m)*endmemrat(3,l)+&
				ksendmem(4,m)*endmemrat(4,l)+ksendmem(5,m)*endmemrat(5,l)+ksendmem(6,m)*endmemrat(6,l)+&
				ksendmem(7,m)*endmemrat(7,l)
			endif
		enddo
	enddo

	!!! Get adiabatic shear modulus for each phase.
	! allocate(muspha(numphatxt))
	do l=1,numphatxt
		do m=1,numpha_td
			nam1=trim(namphatxt(l))
			if(nam1==nampha_td(m))then
	muspha(l)=musendmem(1,m)*endmemrat(1,l)+musendmem(2,m)*endmemrat(2,l)+musendmem(3,m)*endmemrat(3,l)+&
				 musendmem(4,m)*endmemrat(4,l)+musendmem(5,m)*endmemrat(5,l)+musendmem(6,m)*endmemrat(6,l)+&
				 musendmem(7,m)*endmemrat(7,l)
			endif
		enddo
	enddo

	!!! Get molecular weight for the system.
	molweisys=0
	do l=1,numphatxt
		molweisys = molweisys + molweipha(l)*phacomtxt(4,l)
	enddo

	!!! Get volume for the system at pre tem.
	volsys=0
	do l=1,numphatxt
		volsys = volsys + volpha(l)*phacomtxt(4,l)
	enddo

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


write(*,*)''
write(*,*)vpsys
write(*,*)vssys
write(*,*)densys
write(*,*)''


end subroutine

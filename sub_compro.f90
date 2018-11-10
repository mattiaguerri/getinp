! Compute physical properties as a function of P and T.


subroutine compro(pre, tem, densys, vpsys, vssys)


use var


implicit none


! Parameters and Variables.
real, parameter :: presta=1.0, temsta=298.15 ! pre(bar) tem(K), standard values
real(8) alpha(maxendmem_td, numpha_td), vol0(maxendmem_td,numpha_td), kt0(maxendmem_td, numpha_td),&
        molwei(maxendmem_td, numpha_td), k0(maxendmem_td,numpha_td), k1(maxendmem_td,numpha_td),&
        k2(maxendmem_td,numpha_td), m0(maxendmem_td,numpha_td), m1(maxendmem_td,numpha_td), &
        m2(maxendmem_td,numpha_td)
real(8) volt(maxendmem_td,numpha_td),kt(maxendmem_td,numpha_td),volpt(maxendmem_td,numpha_td)
real(8) ksendmem(maxendmem_td,numpha_td)
real(8) musendmem(maxendmem_td,numpha_td)
real(8) molweipha(numphatxt)
real(8) volpha(numphatxt)
real(8) kspha(numphatxt)
real(8) muspha(numphatxt)
real(8) volsys, molweisys, kssys, kssysr, kssysv
real(8) mussys, mussysr, mussysv
real(8) vpsys, vssys, densys ! Properties (of the bulk rock) computed for each composition.
real(8) pre, tem
integer l, m
character(len=10)nam1


include 'thermodynamic_dataset.h'


if (.not. allocated(volref)) allocate(volref(numphatxt))
if (.not. allocated(molref)) allocate(molref(numphatxt))


volref = phacomtxt(2,:)
molref = phacomtxt(4,:)


! Calculate phacomtxt(4,:) according to the change in volume percentages.
phacomtxt(4,:) = (phacomtxt(2,:)-volref(:))/volref(:) * molref(:) + molref(:)


! Get volume at PT for each endmem in the thermodynamic database.
do l = 1, numpha_td
  volt(:,l)  = vol0(:,l) * (1. + alpha(:,l)*(tem-temsta) - 20.*alpha(:,l)*(sqrt(tem)-sqrt(temsta)))
  kt(:,l)    = kt0(:,l) * ( 1. - 1.5 * 1e-4 * (tem-temsta) )
  volpt(:,l) = volt(:,l) * ( 1. - (4.*pre) / (kt(:,l) + 4. * pre) )**0.25
enddo


! Get adiabatic bulk modulus at pre tem for each end-member in the thermodynamic database.
do l=1,numpha_td
  ksendmem(:,l)=k0(:,l)+k1(:,l)*(pre-presta)+k2(:,l)*(tem-temsta)
enddo


! Get adiabatic shear modulus at pre tem for each end-member in the thermodynamic database.
do l=1,numpha_td
  musendmem(:,l)=m0(:,l)+m1(:,l)*(pre-presta)+m2(:,l)*(tem-temsta)
enddo


! Get molecular weight for each phase.
do l=1,numphatxt
  do m=1,numpha_td
    nam1=trim(namphatxt(l))
    if(nam1==nampha_td(m))then
      molweipha(l) = sum( molwei(:,m)*endmemrat(l,:) )
    endif
  enddo
enddo


! Get volume for each phase.
do l=1,numphatxt
  do m=1,numpha_td
    nam1=trim(namphatxt(l))
    if(nam1==nampha_td(m))then
      volpha(l) = sum( volpt(:,m)*endmemrat(l,:) )
    endif
  enddo
enddo


! Get adiabatic bulk modulus for each phase.
do l=1,numphatxt
  do m=1,numpha_td
    nam1=trim(namphatxt(l))
    if(nam1==nampha_td(m))then
      kspha(l) = sum( ksendmem(:,m)*endmemrat(l,:) )
    endif
  enddo
enddo


! Get adiabatic shear modulus for each phase.
do l=1,numphatxt
  do m=1,numpha_td
    nam1=trim(namphatxt(l))
    if(nam1==nampha_td(m))then
      muspha(l) = sum( musendmem(:,m)*endmemrat(l,:) )
    endif
  enddo
enddo


! Get molecular weight for the system.
molweisys = sum( molweipha(:)*phacomtxt(4,:) )

! Get volume for the system at pre tem.
volsys = sum( volpha(:)*phacomtxt(4,:) )


! voigtreusshill of the bulk moduli.
kssysr = (sum(phacomtxt(2,:)/100/kspha(:)))**(-1)
kssysv = sum(phacomtxt(2,:)/100*kspha(:))
kssys = (kssysr+kssysv)/2


! voigtreusshill of the shear moduli.
mussysr = (sum(phacomtxt(2,:)/100/muspha(:)))**(-1)
mussysv = sum(phacomtxt(2,:)/100*muspha(:))
mussys = (mussysr+mussysv)/2


! Get density for the system.
densys = (molweisys*1e-3)/(volsys*1e-5) ! kg/m^3


! Get Vp for the system.
vpsys = (sqrt((kssys*1D05+4./3.*mussys*1D05)/densys))*1e-3


! Get Vs for the system.
vssys = (sqrt(mussys*1e05/densys))*1e-3


return


end

 !...end-of-timestep calculation to determine if water and energy
 !...balance is maintained
 
 subroutine balan(sib,nsl_old,wwwliq_old,wwwice_old,cas_q)
 
 use kinds
 use sibtype
 use sib_const_module, only: &
     nsoil,  &
     snofac, &
     dtt,    &
     dti,    &
     tau, &
     denh2o, &
     denice
 
 use physical_parameters, only: &
     hltm
 
 use eau_params, only : &
     lfus
 
 
 implicit none
 
 !-----------------------------------------------------------------------
 !...WATER BALANCE...
 !
 !     WATER IN + CHANGE IN STORAGE = WATER OUT
 !
 !     inputs:
 !       precipitation             (sib_prog%cupr,sib_prog%lspr)    (mm/sec)
 !
 !     output2: 
 !       CAS-mixed layer latent heat flux   (sib_diag%fws)          (W/m^2)
 !
 !       runoff
 !          surface runoff                   (sib_diag%roffo)       (mm/hr)
 !          subsurface runoff                (sib_diag%roff)        (mm/hr)
 !
 !       storage
 !          interception reservoirs
 !             canopy interception           (sib_prog%capac(1))    (kg/m^2)
 !             ground interception           (sib_prog%capac(2))    (kg/m^2)
 !          soil
 !             soil water                    (sib_prog%www_liq)     (kg/m^2)
 !             soil ice                      (sib_prog%www_ice)     (kg/m^2)
 !          snow
 !             snow water                    (sib_prog%www_liq)     (kg/m^2)
 !             snow ice                      (sib_prog%www_ice)     (kg/m^2)
 !
 !           canopy airspace                  (cas_q)           (kg/m^2)
 !
 !              ** note: indices for soil water and ice arrays
 !                       will be 1 to 10 for soil (1 at top,
 !                       10 at bottom of soil column), and from
 !                       -4 (top) to 0 (bottom) for up to 5 
 !                       snow layers.
 !
 !----------------------------------------------------------------------
 !...ENERGY BALANCE
 !
 !    ENERGY IN + CHANGE IN STORAGE = ENERGY OUT
 !
 !    inputs:
 !      radiation absorbed by canopy    (sib_diag%radt(1))       (W/m^2)
 !      radiation absorbed by ground    (sib_diag%radt(2))       (W/m^2)
 !
 !    outputs:   
 !      latent heat 
 !       transpiration                       (sib_diag%ect)      (J/m^2)
 !       evaporation
 !          from canopy interception storage (sib_diag%eci)      (J/m^2)
 !          from ground interception storage (sib_diag%egi)      (J/m^2)
 !          from surface soil layer          (sib_diag%egs)      (J/m^2)
 !          from snow                        (sib_diag%ess)      (J/m^2)  
 !      sensible heat
 !       canopy                              (sib_diag%hc)       (J/m^2)
 !       ground                              (sib_diag%hg)       (J/m^2)
 !       snow                                (sib_diag%hs)       (J/m^2)
 !
 !    storage
 !      canopy storage flux                  (sib_diag%chf)      (W/m^2)
 !      soil storage flux                    (sib_diag%shf)      (W/m^2)
 !
 !
 !----------------------------------------------------------------------
 
 type(sib_t), intent(inout) :: sib
 
 !-----------------------------------------------------------------------
 !     input variables
 !-----------------------------------------------------------------------
 
 integer(kind=int_kind),intent(in)      :: nsl_old
 real(kind=dbl_kind),intent(in), 
     dimension(-nsnow+1:nsoil) :: wwwliq_old    ! (kg/m^2)
 real(kind=dbl_kind),intent(in), 
     dimension(-nsnow+1:nsoil) :: wwwice_old    ! (kg/m^2)
 
 real(kind=dbl_kind),intent(in) :: cas_q         ! (kg/m^2)
 
 !-----------------------------------------------------------------------
 !     end input variables
 !-----------------------------------------------------------------------
 !     local variables
 !-----------------------------------------------------------------------
 
 real(kind=dbl_kind) :: dstor    ! change in storage, canopy and 
 !   surface interception (kg/m^2)
 
 real(kind=dbl_kind)   :: dqsoil   ! change in storage, soil only
 !   liquid and ice (kg/m^2)
 
 real(kind=dbl_kind) :: dqsnow   ! change in storage, snow (kg/m^2)
 real(kind=dbl_kind) :: dqvegsnow   ! change in storage, snow (kg/m^2)
 real(kind=dbl_kind) :: snownew  ! amount of snow, end-of-timestep (kg/m^2)
 real(kind=dbl_kind) :: snowold  ! amount of snow, beg-of-timestep (kg/m^2)
 
 real(kind=dbl_kind) :: evap     ! evaporation, from CAS to atmosphere.
 !   this encompasses CAS storage, 
 !   ground interception stores, and
 !   evap from top soil layer as well 
 !   as snow (kg/m^2)
 
 real(kind=dbl_kind) :: transp   ! transpiration (kg/m^2)
 
 real(kind=dbl_kind) :: runoff   ! total runoff: surface + subsurface
 !    (kg/m^2)
 real(kind=dbl_kind) :: sbeg,send! sum holders
 
 real(kind=dbl_kind) :: precip   ! precip (kg/m^2)
 
 real(kind=dbl_kind) :: cas_q_new ! CAS water stored as vapor (kg/m^2)
 
 real(kind=dbl_kind) :: rhs,lhs  ! right and left hand sides of energy
 !  balance equation (W/m^2)
 
 integer(kind=int_kind) :: i        ! loop index
 
 
 
     !-----------------------------------------------------------------------
     !     end local variables
     !-----------------------------------------------------------------------
 
     !...NOTE...unless otherwise noted, all units will be kg/m^2 water
 
 
     !...change in canopy and surface interception storage 
 ! CSR moved capac_old to diag
     dstor = (sib%prog%capac(1) - sib%diag%capac_old(1))   +  & ! canopy interception
         (sib%prog%capac(2) - sib%diag%capac_old(2))        ! surface interception
 
     !...change in soil water
     dqsoil = 0.0
     do i=1,nsoil
         dqsoil = dqsoil + (sib%prog%www_liq(i) - wwwliq_old(i)) + &
             (sib%prog%www_ice(i) - wwwice_old(i))
     enddo
 
     !...beginning of timestep snow water (liquid + ice)
 ! CSR count full snow column
     snowold = 0.0
     do i=-nsnow+1,0
         snowold = snowold + wwwliq_old(i) + wwwice_old(i)
     enddo
 
     !...end of timestep snow water (liquid + ice)
 ! CSR count full snow column
     snownew = 0.0
     do i=-nsnow+1,0
         snownew = snownew + sib%prog%www_liq(i) + sib%prog%www_ice(i)
     enddo
 
 !    dqsnow = snownew - snowold
     dqsnow = sib%prog%snow_mass - sib%diag%snow_mass_old
     dqvegsnow = sib%prog%snow_veg - sib%diag%snow_veg_old
 
     !...evaporation, from canopy and ground surface interception 
     !...stores (sib%diag%eci, sib%diag%egi), 
     !...as well as top soil layer (sib%diag%egs) and snow evaporation
 
     evap = sib%diag%fws * dtt / hltm           ! total CAS water vapor flux
 
     !...runoff, surface + subsurface
     runoff = (sib%diag%roffo + sib%diag%roff)
 
     !...precip
     precip = (sib%prog%lspr + sib%prog%cupr) * dtt
 
     !...WATER BALANCE...
 ! CSR: works pretty well now
     sib%diag%wbal =  precip - (evap + runoff) - &
         (dqsoil + dqsnow + dqvegsnow + dstor) - sib%diag%cas_w_storage * dtt / hltm
 
 
     !...setting water balance error at 0.1 kg/m^2 (or 0.1mm)
     if(abs(sib%diag%wbal) > 0.5_dbl_kind) then
         print*,'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW'
         print'(2(a,g16.6))','WATER IMBALANCE: hour=',tau,'imbalance=' &
             ,sib%diag%wbal,' kg/m^2'
         print'(a)','TERMS:  input - output - storage (all units kg/m^2)'
         print'(3(a,g14.6))','input=',precip,' output=',evap+runoff &
             ,' storage=',dqsoil + dqsnow + dstor + dqvegsnow + sib%diag%cas_w_storage * dtt / hltm
         print'(2(a,g16.6))','evap(fws)=',evap,' runoff=',runoff
         print*,' '
         print'(a,4g14.6)','output (eci,egi,egs,ess):',sib%diag%eci/hltm &
             ,sib%diag%egi/hltm,sib%diag%egs/hltm &
             ,sib%diag%ess*snofac/hltm
         print'(a,5f14.6)','storage (interception, soil, snow,vapor, canopy snow):' &
             ,dstor,dqsoil,dqsnow,sib%diag%cas_w_storage * dtt / hltm, dqvegsnow
         print'(a,2f14.6)','runoff (overland, subsfc):' &
             ,sib%diag%roffo  ,sib%diag%roff
 
         print*,'soil moisture'
         sbeg = 0.0
         send = 0.0
         do i=1,nsoil
             print'(i4,2(a,g14.6))',i,&
                  ' beg liq=',wwwliq_old(i), ' end liq=',sib%prog%www_liq(i)
             sbeg = sbeg + wwwliq_old(i)
             send = send + sib%prog%www_liq(i)
         enddo
         print'(2(a,g14.6))','sum beg=',sbeg,' sum end=',send
 
         print*,'Dcapac1: ',sib%prog%capac(1) - sib%diag%capac_old(1),sib%prog%capac(1)
         print*,'Dcapac2: ',sib%prog%capac(2) - sib%diag%capac_old(2),sib%prog%capac(2)
        
 
         sbeg = 0.0
         send = 0.0    
         print*,'soil ice'
         do i=1,nsoil
             print'(i4,2(a,g14.6))',i,' beg ice=',wwwice_old(i),' end ice=' &
                 ,sib%prog%www_ice(i)
             sbeg = sbeg + wwwice_old(i)
             send = send + sib%prog%www_ice(i)
         enddo
         print'(2(a,g14.6))','sum beg=',sbeg,' sum end=',send
 
         print*,'snow',nsl_old,sib%prog%nsl,sib%diag%snow_mass_old,sib%prog%snow_mass
         sbeg = 0.0
         send = 0.0
         do i=-nsnow+1,0
             print'(i4,2(a,g14.6))',i,' beg ice=',wwwice_old(i),' end ice=' ,sib%prog%www_ice(i)
             sbeg = sbeg + wwwice_old(i)
             send = send + sib%prog%www_ice(i)
         enddo
 
         print'(2(a,g14.6))','sum beg=',sbeg,' sum end=',send
         sbeg = 0.0
         send = 0.0
         do i=-nsnow+1,0
             print'(i4,2(a,g14.6))',i,' beg liq=',wwwliq_old(i),' end liq=' ,sib%prog%www_liq(i)
             sbeg = sbeg + wwwliq_old(i)
             send = send + sib%prog%www_liq(i)
         enddo
         print'(2(a,g14.6))','sum beg=',sbeg,' sum end=',send
 
 !        print*,'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW'
 !        stop
     endif
 
     !----------------- END OF WATER BALANCE CODE ----------------------------
 
    !----------------- ENERGY BALANCE ---------------------------------------
 
 ! CSR where has the heat term from melt/freeze energy gone?
 
 ! CSR canopy air space balance (W/m2) - still has spikes (snow? eci/egi?)
 
     rhs = sib%diag%cas_w_storage + &  ! change in CAS water storage
           sib%diag%cas_e_storage + &    ! change in CAS heat storage
           sib%diag%fss      +      &  ! sensible heat flux
           sib%diag%fws               ! latent heat flux
 
     lhs = (sib%diag%hg + sib%diag%hc + sib%diag%hs + &
           sib%diag%ect + sib%diag%eci + sib%diag%egi + &
           sib%diag%egs  + sib%diag%ess) * dti
         
     sib%diag%abal = lhs - rhs
 
 ! CSR canopy balance (W/m2) - still has spikes (snow? eci/egi?)
     sib%diag%cbal = sib%diag%radtt(1) - sib%diag%chf - &
          ( sib%diag%ect + sib%diag%eci + sib%diag%hc ) * dti
 
 ! CSR ground balance (W/m2) - way off +/- 50W/m2
     sib%diag%gbal = sib%diag%radtt(2) +  sib%diag%radtt(3) & 
          - sib%diag%shf &
          - (sib%diag%hg + sib%diag%hs + sib%diag%egi + sib%diag%egs + sib%diag%ess ) * dti
 
 ! CSR total energy balance (W/m2) - way off +/- 50W/m2
 
     rhs = sib%diag%cas_e_storage + sib%diag%cas_w_storage + &
          sib%diag%fss + sib%diag%fws + sib%diag%chf + sib%diag%shf
 
     lhs = sib%diag%radtt(1) + sib%diag%radtt(2) + sib%diag%radtt(3)
 
     sib%diag%ebal = lhs - rhs
 
     if (abs(sib%diag%ebal) > 0.001) then
         !   if (ebal /= 0.0) then
         print*,'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE'
         print*, 'point',sib%stat%pt_num
         print'(4(a,g14.6))','ENERGY IMBALANCE: hour =',tau,'rhs=' &
             ,rhs,' lhs=',lhs,' imbalance=',sib%diag%ebal
         print*,'imbalance amount=',sib%diag%ebal,' (W/m^2)'
         print*,'canopy air space balance=',sib%diag%abal,' (W/m^2)'
         print*,'canopy balance=',sib%diag%cbal,' (W/m^2)'
         print*,'ground balance=',sib%diag%gbal,' (W/m^2) AREAS:',sib%diag%areas
         print'(2(a,g14.6))','sib%diag%ect=',sib%diag%ect*dti,' sib%diag%eci=' &
             ,sib%diag%eci*dti
         print'(2(a,g14.6))','sib%diag%egi=',sib%diag%egi*dti,' sib%diag%egs=' &
             ,sib%diag%egs*dti
         print'(2(a,g14.6))','sib%diag%ess=',sib%diag%ess*dti,' sib%diag%hc='  &
             ,sib%diag%hc*dti
         print'(2(a,g14.6))','sib%diag%hg=',sib%diag%hg*dti,' sib%diag%hs='    &
             ,sib%diag%hs*dti
         print'(3(a,g14.6))','radt(1)=',sib%diag%radt(1),' radt(2)=',          &
             sib%diag%radt(2),' radt(3)=',sib%diag%radt(3)
         print'(3(a,g14.6))','radtt(1)=',sib%diag%radtt(1),' radtt(2)=',          &
             sib%diag%radtt(2),' radtt(3)=',sib%diag%radtt(3)
         print'(2(a,g14.6))','sib%diag%chf=',sib%diag%chf,' sib%diag%shf=',    &
             sib%diag%shf
         print*,'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE'
         print*,'snow layers: ',-sib%prog%nsl
 
 !        stop
 
     endif
 
 
 end subroutine balan
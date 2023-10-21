00001 
00002 module cos
00003 
00004    use kinds
00005    use sibtype
00006    use sib_const_module, only: &
00007         po2m,   &
00008         pdb,    &
00009         dtt,    &
00010         dti,    &
00011         denh2o
00012    use physical_parameters, only: &
00013         p0 => p0_sfc, &
00014         tice
00015 
00016    implicit none
00017 
00018 
00019 !--------------------------------------------------------
00020 
00021 !LOCAL VARIABLES
00022 
00023         
00024    real(kind=dbl_kind) :: cosa     ! CAS COS concentration (mol COS/mol air)
00025    real(kind=dbl_kind) :: cosm     ! reference level COS concentration 
00026                                                                         !   (mol C/mol air)
00027 !    real(kind=dbl_kind) :: coss     ! leaf surface COS concentration 
00028                                                                 !   (mol C/mol air)
00029 !    real(kind=dbl_kind) :: pcosa    ! intermediate CAS COS concentration 
00030                                                                 !   (Pa)
00031 !    real(kind=dbl_kind) :: pcoss    ! intermediate leaf surface COS 
00032                                                                 !   partial pressure (Pa)
00033 !    real(kind=dbl_kind) :: pcosi    ! intermediate stomatal (internal) 
00034                                                                         !   COS partial pressure (Pa)
00035 !    real(kind=dbl_kind) :: pcosc    ! intermediate leaf chloroplast COS 
00036                                                                 !   partial pressure (Pa)
00037         
00038    real(kind=dbl_kind) :: rcos          ! ratio of net flux of CO2 and COS scaled by concentration      
00039    real(kind=dbl_kind) :: rcos2 ! gross CO2 flux ratio  
00040 
00041    real(kind=dbl_kind) :: gtcos
00042         
00043 
00044 
00045 
00046    contains
00047                 
00048    subroutine cos_calc(sib,co2cap,gah2o,gbh2o,gsh2o,co2m,co2a,xgco2m,qt,gmeso)
00049                                                 
00050    use kinds
00051    use sibtype
00052 
00053    implicit none
00054                 
00055         
00056    type(sib_t), intent(inout) :: sib
00057         
00058         
00059    !input variables
00060    real(kind=dbl_kind) :: co2cap   ! conversion factor from ppm to mole/m2 in the CAS
00061    real(kind=dbl_kind) :: gah2o    !
00062    real(kind=dbl_kind) :: gbh2o    !
00063    real(kind=dbl_kind) :: gsh2o    !
00064    real(kind=dbl_kind) :: co2m     ! reference level CO2 concentration
00065                                    !       (mol C/mol air)
00066    real(kind=dbl_kind) :: co2a     ! CAS CO2 concentration 
00067                                    !       (mol CO2/mol air)
00068    real(kind=dbl_kind) :: xgco2m
00069    real(kind=dbl_kind) :: qt
00070    real(kind=dbl_kind) :: gmeso    ! mesophyll conductance
00071 
00072 !itb...local variables
00073    integer(kind=int_kind) :: j     !loop variable
00074 
00075    real(kind=dbl_kind) :: co2c
00076    real(kind=dbl_kind) :: cosa
00077    real(kind=dbl_kind) :: gcosm
00078 
00079 
00080    real(kind=dbl_kind) :: freeze    ! term to restrict COS uptake by frozen ground
00081    real(kind=dbl_kind) :: moist     ! term to restrict COS uptake by saturated ground
00082 
00083    real(kind=dbl_kind) :: resp_rel  ! relative fraction of respiration in each of the 
00084                                     ! top 3 soil layers
00085 
00086    real(kind=dbl_kind) :: root_tot  ! total root amount in top 3 soil layers
00087    real(kind=dbl_kind) :: wfract    ! fraction of saturation in to 3 soil layers
00088 
00089    real(kind=dbl_kind) :: wet_exp   ! wetness scaling of respiration term
00090 
00091    real(kind=dbl_kind),dimension(3) :: cos_grnd    ! ground COS flux (mol/m2/sec)
00092                                                    ! THIS IS AN UPTAKE FLUX
00093 
00094    real(kind=dbl_kind), parameter :: k_cos_soil = 1.2E4
00095         
00096 
00097    cosm    = sib%prog%pcosm  /  (sib%prog%ps*100.)
00098    co2c = sib%diag%pco2c(6) / (sib%prog%ps*100.)
00099 
00100    cosa = sib%prog%pcosap / (sib%prog%ps*100.)
00101 
00102 
00103 !itb...added aparkk (may 2008)
00104    gcosm = gmeso*sib%param%vmax0(1) * sib%diag%aparkk * sib%diag%rstfac(2) * 2.1**qt
00105 
00106 
00107 
00108    gtcos = 1.0 / ((1.6/gsh2o*1.05758605008) + (1.4/gbh2o*1.03803136381) +       &
00109                    (1.0/gah2o) + 1.0/xgco2m + 1.0/gcosm)
00110 
00111 
00112 !itb_cos...new eqn from Joe
00113 
00114    sib%diag%cosflux = gtcos * cosa 
00115 
00116 
00117 
00118 
00119    sib%diag%assim2 = 0.97 * sib%diag%cosflux * (co2m - co2c) / cosm     
00120 
00121    sib%diag%rcos = (sib%diag%cosflux/sib%diag%cflux) * (co2m/cosm)
00122 
00123    rcos2 = (sib%diag%cosflux/sib%diag%assimn(6)) * (co2m/cosm)
00124                                                                         
00125 !   sib%diag%resptot = (sib%diag%respg + sib%diag%assimn(6)) + sib%diag%assim2  
00126 
00127 !   sib%diag%resptot2 = (sib%diag%rcos / rcos2 - 1.0) * sib%diag%assim2
00128 
00129 
00130 
00131 
00132 !itb...ground uptake of COS; we're limiting calculations to top 3 soil layers,
00133 !itb...but scaling the contributions to total soil respiration
00134 
00135    cos_grnd(:) = 0.0
00136 
00137  !itb...ground uptake restricted by extremely wet soil
00138 
00139    root_tot = sib%param%rootf(1) + sib%param%rootf(2) + sib%param%rootf(3)
00140 
00141    resp_rel = sib%param%het_respfac(1) * sib%diag%soilscale(1)    &
00142             + sib%param%het_respfac(2) * sib%diag%soilscale(2)    &
00143             + sib%param%het_respfac(3) * sib%diag%soilscale(3)    
00144 
00145    do j=1,3   !top 3 soil layers
00146 
00147       if(sib%prog%www_liq(j) == 0.0 .AND. sib%prog%www_ice(j) == 0.0) then
00148 !          print*,'layer =',j
00149 !          print*,'biome:',sib%param%biome,' LAI=',sib%param%zlt
00150 !          print*,'no water in soil layer'
00151 
00152           freeze = 1.0
00153 
00154       else
00155 
00156          freeze = sib%prog%www_liq(j) / ( sib%prog%www_liq(j) + sib%prog%www_ice(j) )
00157 
00158       endif
00159 
00160 
00161 
00162       wfract =    &
00163          ((sib%prog%www_liq(j) / (sib%prog%dz(j) * sib%param%poros * denh2o))   &
00164          * (sib%param%rootf(j) / root_tot ))
00165 
00166       moist = 1.42 - (wfract * 1.42)
00167 
00168       moist = MIN(1.0,moist)
00169 
00170 !itb...now adjust moist to utilize soil moisture respiration scaling factor
00171  
00172       wet_exp = ((wfract**sib%param%zm - sib%param%woptzm) / (1.0 - sib%param%woptzm))**2.0
00173       wet_exp = MIN(wet_exp,10.0_dbl_kind)
00174 
00175       moist = moist * (0.8 * sib%param%wsat**wet_exp + 0.2)
00176 
00177 
00178       cos_grnd(j) =          &
00179        (sib%param%het_respfac(j) * sib%diag%soilscale(j)) / resp_rel *     &
00180         sib%diag%resp_het * cosa  * k_cos_soil *       &
00181                            freeze * moist * sib%diag%soilq10(j)
00182 
00183 !print*,j,sib%param%het_respfac(j),sib%diag%soilscale(j),resp_rel,sib%diag%resp_het,cosa,k_cos_soil,freeze,moist,sib%diag%soilq10(j)
00184 
00185    enddo
00186 
00187 
00188   
00189  
00190 
00191    sib%diag%cos_grnd = 0.0
00192 
00193 
00194    do j=1,3
00195  
00196      sib%diag%cos_grnd = sib%diag%cos_grnd + cos_grnd(j)  
00197 
00198 !print*,j,cos_grnd(j),sib%diag%cos_grnd
00199 
00200    enddo
00201 
00202 
00203 
00204 !itb...prognostic eqn for COSA: follows phosib/CO2 exactly
00205 
00206 
00207    cosa = (cosa + dtt/co2cap*((cosm * gah2o) - sib%diag%cos_grnd - sib%diag%cosflux)) /       &
00208                   (1.0 + (dtt * gah2o)/co2cap)
00209 
00210 
00211 
00212 
00213 !print*,':',cosa,cos_grnd,sib%diag%cosflux
00214 
00215 
00216    sib%prog%pcosap = cosa * sib%prog%ps * 100.
00217 
00218    sib%diag%coss = cosa - sib%diag%cosflux*(1.0/gah2o+1.4/gbh2o)
00219    sib%diag%cosi = sib%diag%coss - sib%diag%cosflux * (1.6/gsh2o * 1.057860)
00220    sib%diag%cosc = sib%diag%cosi - sib%diag%cosflux * (1.0/xgco2m) 
00221   
00222    sib%diag%cos_flux_pbl = gah2o * (cosa - cosm)   ! mol/m2/sec
00223 
00224    sib%diag%cos_temp  = cos_grnd(1)
00225    sib%diag%cos_temp1 = cos_grnd(2) + cos_grnd(3)
00226 
00227 !   print'(a,3g15.5)','COS:',sib%diag%cos_flux_pbl,sib%diag%cosflux,sib%diag%cos_grnd
00228                         
00229 end subroutine cos_calc
00230 
00231 end module cos
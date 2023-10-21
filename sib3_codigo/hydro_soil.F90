00001 
00002 subroutine hydro_soil(sib)
00003 
00004 !----------------------------------------------------------------------
00005 !
00006 !     soil water - based on CLM_HYDRO_SOIL.F90 code...
00007 !
00008 !----------------------------------------------------------------------
00009 !
00010 !     following taken (almost) verbatim from CLM_HYDRO_SOIL comments...
00011 !
00012 !     main subroutine used to execute the calculation of water 
00013 !     processes over soil.
00014 !
00015 !     1) water within soil (subroutine soilwater)
00016 ! 
00017 !     2) runoff
00018 !        The original code was provided by Robert E. Dickinson based on
00019 !        following clues: exponential decrease of Ksat, a water table
00020 !        level determination level including highland and lowland levels
00021 !        and fractional area of wetland (water table above the surface).
00022 !        Runoff is parameterized from the lowlands in terms of precip
00023 !        incident on wet areas and a base flow, where these are estimated
00024 !        using ideas from TOPMODEL.
00025 !
00026 !     The original scheme was modified by Z.-L. Yang and G.-Y. Niu,
00027 !     *  using a new method to determine water table depth and the 
00028 !        fractional wet area (fwsoil).
00029 !     *  computing runoff (surface and subsurface) from this fraction and
00030 !        the remaining fraction (i.e. 1-fwsoil)
00031 !     *  for the 1-fwsoil part, using BATS1e method to compute surface and
00032 !        subsurface runoff.
00033 !
00034 !     The original code on soil moisture and runoff was provided by R.E.
00035 !     Dickinson in July 1996.
00036 !
00037 !     Revision History:
00038 !     15 September 1999: Yongjiu Dai; Initial code
00039 !     12 November  1999: Z.-L. Yang and G.-Y. Niu
00040 !     15 December  1999: Paul Houser and Jon Radakovich; F90 revision
00041 !     25 January   2002: Ian Baker; Integration into SiB
00042 !
00043 !----------------------------------------------------------------------
00044 
00045 use kinds
00046 use sibtype
00047 
00048 use physical_parameters, only: &
00049     hltm
00050 
00051 use sib_const_module, only: &
00052     nsoil,  &
00053     denice, &
00054     denh2o, &
00055     wtfact, &
00056     dtt,    &
00057     dti
00058 
00059 implicit none
00060 
00061 !----------------------------------------------------------------------
00062 
00063 type(sib_t), intent(inout) :: sib
00064 
00065 !----------------------------------------------------------------------  
00066 
00067 
00068 !...LOCAL VARIABLES
00069 integer(kind=int_kind) :: j,i      ! loop variable
00070 real(kind=dbl_kind)    :: zwice    ! total ice mass in soil column
00071 !   (kg m^-2)
00072 real(kind=dbl_kind)    :: www_tot(1:nsoil)  
00073 ! total pore space occupied
00074 !  by ice+liquid (-)
00075 real(kind=dbl_kind)    :: dw_liq(1:nsoil)   
00076 ! change in layer liquid
00077 !  -volumetric (m^3/m^3)
00078 real(kind=dbl_kind)    :: dzmm(1:nsoil)
00079 ! soil layer thickness in mm (mm)
00080 real(kind=dbl_kind)    :: zmm(1:nsoil)
00081 ! node depth in mm (mm)
00082 real(kind=dbl_kind)    :: wmean    ! averaged soil wetness in top 
00083 !  layers
00084 real(kind=dbl_kind)    :: zmean    ! top soil layers contributing to 
00085 !  runoff
00086 real(kind=dbl_kind)    :: fz       ! factor (-)
00087 real(kind=dbl_kind)    :: zwt      ! water table depth (m)
00088 real(kind=dbl_kind)    :: infil    ! infiltration into soil 
00089 !  (kg m^-2 sec^-1)
00090 real(kind=dbl_kind)    :: hk(1:nsoil) 
00091 ! hydraulic conductivity 
00092 !  (mm h20 sec^-1)
00093 real(kind=dbl_kind)    :: dhkdw (1:nsoil)  
00094 ! d(hk)/d(water content)
00095 real(kind=dbl_kind)    :: q_wet    ! subsurface runoff from 'wet' part
00096 !  (mm H2O sec^-1)
00097 real(kind=dbl_kind)    :: q_dry    ! subsurface runoff from 'dry' part
00098 !  (mm H2O sec^-1)
00099 real(kind=dbl_kind)    :: hksum    ! total hk for layers 6-9
00100 real(kind=dbl_kind)    :: zsat     ! hk, weighted for soil thickness
00101 real(kind=dbl_kind)    :: wsat     ! hk, weighted for soil wetness
00102 real(kind=dbl_kind)    :: dzksum   ! hk, weighted for soil thickness
00103 real(kind=dbl_kind)    :: fwsoil   ! saturation fraction
00104 real(kind=dbl_kind)    :: watmin   ! minimum soil moisture 
00105 real(kind=dbl_kind)    :: xs       ! excess soil moisture 
00106 !  (kg m^-2) 
00107 
00108 
00109 real(kind=dbl_kind)    :: roffo_t   ! placeholder for overland
00110 ! runoff (kg/m^2)
00111 
00112 !---------------------------------------------------------------------
00113 
00114 
00115     zwice = 0.0  ! sum of ice mass of soil
00116 
00117     do j=1,nsoil
00118         zwice = zwice + sib%prog%www_ice(j)
00119 
00120         sib%prog%vol_ice(j) = min(sib%param%poros, sib%prog%www_ice(j)/(sib%prog%dz(j)*denice))
00121         sib%diag%eff_poros(j) = sib%param%poros - sib%prog%vol_ice(j)
00122         sib%prog%vol_liq(j) = min(sib%diag%eff_poros(j), &
00123             sib%prog%www_liq(j)/(sib%prog%dz(j)*denh2o))
00124         if (sib%prog%vol_liq(j) == 0.0 .and. sib%prog%www_liq(j) > 0.0 ) then
00125             sib%diag%roff = sib%diag%roff + sib%prog%www_liq(j) 
00126             sib%prog%www_liq(j) = 0.0
00127         endif
00128 
00129         www_tot(j) = min(1.0_dbl_kind,(sib%prog%vol_ice(j)+ &
00130             sib%prog%vol_liq(j))/sib%param%poros)
00131     enddo
00132 
00133 
00134     !...determine water table
00135     wmean = 0.0
00136     fz    = 1.0
00137     do j=1,nsoil
00138         wmean = wmean + www_tot(j)*sib%prog%dz(j)
00139     enddo
00140     zwt = fz * (sib%prog%layer_z(nsoil) - wmean)
00141 
00142     !...saturation fraction
00143 
00144     fwsoil = wtfact * min(1.0_dbl_kind,exp(-zwt))
00145     !      print'(2(a,g16.6))','water table depth, m=',zwt,' sat fraction=',fwsoil
00146 
00147 
00148     !...these soil calculations are hardwired for a 10-layer soil model
00149     wmean = 0.0
00150     zmean = 0.0
00151 
00152     do j=1,3
00153         zmean = zmean + sib%prog%dz(j)
00154         wmean = wmean + www_tot(j)*sib%prog%dz(j)
00155     enddo
00156 
00157     wmean = wmean/zmean
00158 
00159     !itb...modifying infiltration from CLM. We put precipitation/snowmelt 
00160     !itb...into surface interception store (sib%prog%capac(2)). Any infiltration
00161     !itb...will come from that reservoir. The hope is that we can hold
00162     !itb...onto puddles from one timestep to the next. Capac(2) is constrained
00163     !itb...by satcap(2), which is set in subroutine RADA2.
00164 
00165 
00166     !itb...THIS IS A PROBLEM-MESH BETWEEN SiB/CLM IS NOT CLEAN...
00167 
00168     !itb...old code...
00169     !      roffo_t  = max(0.0_dbl_kind,fwsoil*sib%diag%www_inflow) + &
00170     !                 max(0.0_dbl_kind,(1.0 - fwsoil)  &
00171     !               * min(1.0_dbl_kind,wmean**4*sib%diag%www_inflow))
00172 
00173     !itb...I think that roffo_t represents everything that can't 
00174     !itb...infiltrate during this timestep.
00175     !itb...new code
00176     roffo_t  = max(0.0_dbl_kind,fwsoil*sib%prog%capac(2)*dti) +  &
00177         max(0.0_dbl_kind,(1.0 - fwsoil) &
00178         * min(1.0_dbl_kind,wmean**4*sib%prog%capac(2)*dti))
00179 
00180     roffo_t  = roffo_t * dtt   ! kg/m^2
00181 
00182     !      sib%diag%roffo = sib%diag%roffo + roffo_t
00183 
00184     !...infiltration into surface soil layer 
00185     infil = sib%prog%capac(2) - roffo_t  ! units: kg/m^2
00186 
00187     sib%prog%capac(2) = sib%prog%capac(2) - infil 
00188 
00189     sib%diag%roffo = sib%diag%roffo + max(0.0_dbl_kind, &
00190         (sib%prog%capac(2) - sib%param%satcap(2)*denh2o))  ! kg/m^2
00191 
00192 !    sib%prog%capac(2) = max(0.0_dbl_kind,sib%prog%capac(2) - &
00193 !        sib%param%satcap(2)*denh2o)
00194 
00195     sib%prog%capac(2) = min(sib%prog%capac(2),sib%param%satcap(2)*denh2o)
00196     sib%prog%capac(2) = MAX(sib%prog%capac(2),0.0_dbl_kind)
00197 
00198     !...set up r, a, b, and c vectors (following Bonan's (1996) soil)
00199     !...for tridiagonal matrix.
00200     !...(length units will be millimeters)
00201 
00202     do j = 1,nsoil
00203         zmm(j)  = sib%prog%node_z(j) *1000.0
00204         dzmm(j) = sib%prog%dz(j) * 1000.0
00205     enddo
00206 
00207     !...need to convert infil from kg/m^2 to kg/m^2/sec
00208     if(infil > 0.0) then
00209         infil = infil * dti
00210     endif
00211 
00212     call soilwater( sib, infil, hk, dhkdw, dw_liq, dzmm, zmm )
00213 
00214 
00215     !...update the liquid water mass (kg/m^2)
00216     do j=1,nsoil
00217         sib%prog%www_liq(j) = sib%prog%www_liq(j) + &
00218             dw_liq(j)*sib%prog%dz(j)*denh2o
00219 !print*,'DW:',j,dw_liq(j)
00220     enddo
00221 
00222 
00223 
00224     !...streamflow and total runoff
00225     !...The amount of streamflow is assumed to be maintained by flow 
00226     !...from the lowland water table with different levels contributing 
00227     !...according to their thickness and saturated hydraulic conductivity,
00228     !...i.e. a given layer below the water table interface loses water
00229     !...at a rate per unit depth given by drainage*hk/(sum over all layers 
00230     !...below this water table of hk*dz).  Because this is a slow smooth 
00231     !...process, and not strongly coupled to water in any one layer, it 
00232     !...should remain stable for explicit time differencing.  Hence, for 
00233     !...simplicity it is removed explicitly prior to the main soil water 
00234     !...calculation.  
00235     !...Another assumption: no subsurface runoff for ice mixed soil.
00236     !...Zong-Liang Yang and G.-Y. Niu
00237 
00238     sib%diag%qqq = 0.0
00239     q_wet  = 0.0
00240     q_dry  = 0.0
00241     hksum  = 0.0
00242 
00243     !...HARDWIRE...
00244     !...taking out streamflow...
00245 
00246     !      do j=6,nsoil-1
00247     !       hksum = hksum + hk(j)
00248     !      enddo
00249 
00250     !      if (zwice <= 0.0 .and. hksum > 0.0 )then
00251     !        zsat = 0.0
00252     !        wsat = 0.0
00253     !        dzksum = 0.0
00254     !        do j=6,nsoil-1
00255     !          zsat = zsat + sib%prog%dz(j) *hk(j)
00256     !          wsat = wsat + www_tot(j)*sib%prog%dz(j)*hk(j)
00257     !          dzksum = dzksum + hk(j)*sib%prog%dz(j)
00258     !        enddo
00259     !        wsat = wsat / zsat
00260 
00261     !        q_wet = fwsoil * 1.0e-5 * exp(-zwt)
00262     !        q_dry = (1.-fwsoil) * 4.0e-2 * wsat **(2.0*sib%param%bee+3)
00263     !        sib%diag%qqq = q_wet + q_dry
00264 
00265     !...streamflow...
00266     !        do j=6,nsoil-1
00267     !          sib%prog%www_liq(j) = sib%prog%www_liq(j) -dtt*sib%diag%qqq*sib%prog%dz(j)  &
00268     !                                            *hk(j)/dzksum
00269     !        enddo
00270     !      endif
00271 
00272 
00273 
00274     !...limit www_liq to be greater than or equal to watmin
00275     !...get water needed to bring www_liq equal to watmin from lower level
00276 
00277     watmin = 0.0
00278     !      watmin = sib%param%vwcmin
00279     do j=1,nsoil-1
00280         !        if(sib%prog%www_liq(j) < 0.0 ) then
00281         if(sib%prog%www_liq(j) < watmin*sib%prog%dz(j)*denh2o ) then
00282             xs = watmin * sib%prog%dz(j) * denh2o - sib%prog%www_liq(j)
00283         else
00284             xs = 0.0
00285         endif
00286         sib%prog%www_liq(j)   = sib%prog%www_liq(j)   + xs
00287         sib%prog%www_liq(j+1) = sib%prog%www_liq(j+1) - xs
00288 !print*,'layer=',j,'  excess=',xs
00289     enddo
00290 
00291     j = nsoil
00292     if(sib%prog%www_liq(j) < watmin) then
00293         xs = watmin * sib%prog%dz(j) * denh2o - sib%prog%www_liq(j)
00294 !print*,'funky water:',sib%prog%www_liq(j)
00295     else
00296         xs = 0.0
00297     endif
00298     sib%prog%www_liq(j) = sib%prog%www_liq(j) + xs
00299     sib%diag%qqq = sib%diag%qqq - xs
00300 
00301 !print*,'layer=',nsoil,'  excess=',xs
00302 
00303 
00304 
00305     !...determine water in excess of saturation
00306 
00307     !itb...I don't like how CLM included SATCAP here...
00308     !      xs = max(0.0_dbl_kind,sib%prog%www_liq(1)-                           &
00309     !                (sib%param%satcap(2)+sib%diag%eff_poros(1) * sib%prog%dz(1)*denh2o))
00310     !      if(xs > 0.0) sib%prog%www_liq(1) = sib%param%satcap(2)+sib%diag%eff_poros(1)  &
00311     !                                                 *sib%prog%dz(1)*denh2o
00312 
00313     xs = max(0.0_dbl_kind,sib%prog%www_liq(1)-sib%diag%eff_poros(1) &
00314         * sib%prog%dz(1)*denh2o)
00315 
00316     if(xs > 0.0) sib%prog%www_liq(1) = sib%diag%eff_poros(1) * &
00317         sib%prog%dz(1) * denh2o
00318 
00319     do j=2,nsoil
00320         xs = xs + max(sib%prog%www_liq(j)-sib%diag%eff_poros(j) &
00321             *sib%prog%dz(j)*denh2o,0.0_dbl_kind)
00322 
00323         sib%prog%www_liq(j) = min(sib%diag%eff_poros(j)*sib%prog%dz(j)*denh2o &
00324             ,sib%prog%www_liq(j))
00325     enddo
00326 
00327     !...sub-surface runoff and drainage
00328     sib%diag%qqq = sib%diag%qqq + xs                    
00329     sib%diag%roff = sib%diag%roff + sib%diag%qqq
00330 
00331     !...renew ice and liquid mass due to condensation
00332     !      if(sib%prog%nsl == 0) then
00333     !        sib%prog%www_liq(1) = sib%prog%www_liq(1) + dew*dtt
00334     !        sib%prog%www_ice(1) = sib%prog%www_ice(1) + (deposition -sublimation)*dtt
00335     !      endif
00336 
00337 
00338 end subroutine hydro_soil
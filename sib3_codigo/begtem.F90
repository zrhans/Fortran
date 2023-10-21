00001 !
00002 !================SUBROUTINE BEGTEM=======================================
00003 !
00004 subroutine begtem(sib,sib_loc)
00005 
00006 
00007     use kinds
00008     use sibtype
00009 
00010     use physical_parameters, only: &
00011         cp => spec_heat_cp, &
00012         grav,               &
00013         hltm,               &
00014         tice
00015 
00016     use sib_const_module, only: &
00017         nsoil,  &
00018         denh2o, &
00019         denice, &
00020         tkwat,  &
00021         tkice,  &
00022         tkair,  &
00023         cww,    & ! water heat capacity
00024         clai,   &
00025         cv,     &
00026         cpice,  &
00027         cpliq
00028 
00029     implicit none
00030 
00031     !----------------------------------------------------------------------
00032 
00033     type(sib_t), intent(inout) :: sib
00034 
00035     type(sib_local_vars)     ,intent(inout) :: sib_loc
00036     ! variables local to SiB
00037 
00038     !----------------------------------------------------------------------  
00039 
00040 
00041     !      References
00042 
00043     !      Sellers, P.J., M.D. Heiser, F.G. Hall, S.J. Goetz, D.E. Strebel,
00044     !                     S.B. Verma, R.L. Desjardins, P.M. Schuepp,
00045     !                     J.I. MacPherson, 1995: Effects of Spatial Variability
00046     !                     in Topography, Vegetation Cover and Soil Moisture on 
00047     !                     Area-aAveraged Surface Fluxes: A Case Study Using the
00048     !                     FIFE 1989 Data. JGR, 100(D12), 25607-25629.
00049 
00050     !      Sellers, P.J. and Mintz, Y., Y.C. Sud, A. Dalcher, 1986: A Simple 
00051     !                     Biospher Model (SiB) for use Within General 
00052     !                     Circulation Models. JAS, 43(6),505-531.
00053 
00054     !========================================================================
00055     !
00056     !     Calculation of flux potentials and constants prior to heat 
00057     !         flux calculations.  
00058     !
00059     !======================================================================== 
00060 
00061 
00062     !++++++++++++++++++++++++++++++OUTPUT+++++++++++++++++++++++++++++++++++
00063     !
00064     !       CZC            CANOPY HEAT CAPACITY (J M-2 K-1)
00065     !       PSY            PSYCHROMETRIC CONSTANT (hPa K-1)
00066     !       TKSOIL         SOIL/SNOW THERMAL CONDUCTIVITY (W M-1 K-1)
00067     !       SHCAP          SOIL/SNOW HEAT CAPACITY (J M-2 K-1)
00068     !       ETC            E(star) TC-vapor pressure at leaf sfc
00069     !       GETC           dE(star)/dTC
00070     !       ETG            E(star) TG-vapor pressure at ground sfc
00071     !       GETG           dE(star)/dTG
00072     !       ETS            E(star) TS-vapor pressure at snow sfc
00073     !       GETS           dE(star)/dTS
00074     !       WC             CANOPY WETNESS FRACTION
00075     !       WG             GROUND WETNESS FRACTION
00076     !       RSTFAC(2)      SOIL MOISTURE STRESS FACTOR 
00077     !       RSOIL          SOIL SURFACE RESISTANCE (S M-1)
00078     !       HR             SOIL SURFACE RELATIVE HUMIDITY
00079 
00080 
00081     !
00082     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
00083 
00084 
00085     !......LOCAL VARIABLES
00086     integer :: i,j
00087     real(kind=dbl_kind) ::  
00088         one,    ! must be for numerical purposes...
00089         satw,   ! fraction of saturation, soil level
00090         fl,     ! fraction liquid in soil layer
00091         dke,    ! Kersten number
00092         bw,     ! partial density of water (ice+liquid)
00093         fac,    ! fraction of saturation of top soil layer
00094         psit,   ! moisture potential of top soil layer
00095         argg,   ! RH of air at soil surface
00096         phroot   ! normalized value of soil moisture potential
00097     ! over all soil levels
00098 
00099 
00100     real(kind=dbl_kind), dimension(nsoil) :: 
00101         dksat,         ! thermal conductivity, saturated soil (W/m/K)
00102         phroot_layer,  ! layer values of phi-soil moisture potential
00103         vwc             ! volumetric water content (theta) (-)
00104    
00105 
00106     real(kind=dbl_kind), dimension(-nsnow+1:nsoil) :: 
00107         thk ! thermal conductivity of layer (W/m/K)
00108 
00109     real(kind=dbl_kind),dimension(1) :: ppl,ttl,esst,dtesst
00110     ! holder variables to make SGI 
00111     ! compiler happy for calls to
00112     ! eau_sat
00113 
00114 
00115     one = 1.0          
00116 
00117     !----------------------------------------------------------------------
00118     !     CALCULATION OF CANOPY HEAT CAPACITIES.
00119     !----------------------------------------------------------------------
00120     sib%param%czc = sib%param%zlt*clai+(0.5*sib%prog%snow_veg/denice + &
00121         sib%prog%capac(1)/denh2o)*cww
00122     sib%diag%psy = cp / hltm * sib%prog%ps / .622    
00123 
00124 
00125     !-------------------------------------------------------------------
00126     !   calculation of soil/snow thermal conductivity 
00127     !   and heat capacity
00128     !   Based on Farouki (1981), Jordan (1991), de Vires (1963),
00129     !   and on CLM coding
00130     !-------------------------------------------------------------------
00131 
00132     do j=1,nsoil
00133 
00134         !...fractional expression of saturation
00135         satw = ((sib%prog%www_liq(j)/denh2o) + (sib%prog%www_ice(j)/denice))/ &
00136             (sib%prog%dz(j) * sib%param%poros)
00137 
00138 
00139         satw = min(1.0_dbl_kind,satw)
00140 
00141         if(satw > 1.0E-6) then                 ! water present in soil
00142             fl = sib%prog%www_liq(j) / (sib%prog%www_liq(j) + &
00143                 sib%prog%www_ice(j)) ! frac liq
00144 
00145             if(sib%prog%td(j) >= tice) then ! unfrozen soil
00146                 ! kersten number
00147                 dke = max(0.0_dbl_kind, log10(satw) + 1.0_dbl_kind)  
00148                 dksat(j) = sib%param%tksatu(j)
00149             else ! frozen soil
00150                 dke = satw
00151                 dksat(j) = sib%param%tkmg(j) * 0.249**(fl*sib%param%poros) &
00152                     *2.29**sib%param%poros
00153             endif
00154 
00155             thk(j) = dke*dksat(j) + (1.0-dke)*sib%param%tkdry(j)
00156 
00157         else ! soil is very dry
00158             thk(j) = sib%param%tkdry(j)
00159         endif
00160 
00161     enddo  !nsoil loop
00162 
00163     !...thermal conductivity of snow
00164     if (sib%prog%nsl < 0) then
00165         do j= sib%prog%nsl+1,0
00166             bw = (sib%prog%www_liq(j) + sib%prog%www_ice(j))/sib%prog%dz(j)
00167             thk(j) = tkair + (7.75E-5*bw + 1.105E-6*bw*bw)*(tkice-tkair) 
00168         enddo
00169     endif
00170 
00171     !...thermal conductivity at the layer interface
00172     do j = sib%prog%nsl+1,nsoil-1
00173 
00174         sib%param%tksoil(j) = thk(j)*thk(j+1)* &
00175             (sib%prog%node_z(j+1)-sib%prog%node_z(j))  &
00176             /(thk(j)*(sib%prog%node_z(j+1)-sib%prog%layer_z(j)) + &
00177             thk(j+1)*(sib%prog%layer_z(j)-sib%prog%node_z(j)))
00178 
00179     enddo
00180 
00181 
00182     !itb...THIS IS A PATCH
00183     !      sib%param%tksoil(nsoil) = 0.0
00184     sib%param%tksoil(nsoil) = sib%param%tksoil(nsoil-1)
00185 
00186     !......heat capacity, soil
00187     do j=1,nsoil
00188         sib%param%shcap(j) = sib%param%csolid(j)*(1.0-sib%param%poros)*sib%prog%dz(j) &
00189             + sib%prog%www_ice(j)*cpice + sib%prog%www_liq(j)*cpliq
00190     enddo
00191 
00192 
00193     !......heat capacity, snow
00194     if(sib%prog%nsl < 0) then
00195         do j=sib%prog%nsl+1,0
00196             sib%param%shcap(j) = cpliq*sib%prog%www_liq(j) + &
00197                 cpice*sib%prog%www_ice(j)
00198         enddo
00199     endif
00200 
00201     !...slamda is the lambda(z_sub h,j)/(zj+1 - zj) term from the CLM
00202     !...document. slamda*(Tj+1 - Tj) is the heat flux from layer j to 
00203     !...layer j+1. This is a different numerical scheme than Bonan.
00204 
00205     do j=sib%prog%nsl+1,nsoil-1
00206         sib%param%slamda(j) = sib%param%tksoil(j) / (sib%prog%node_z(j+1) - &
00207             sib%prog%node_z(j))
00208     enddo
00209     
00210 
00211     !...THIS IS A PATCH
00212     sib%param%slamda(nsoil) = sib%param%slamda(nsoil-1)
00213 
00214     !
00215     !----------------------------------------------------------------------
00216     !      Calculation of ground surface temperature and wetness fractions
00217     !        
00218     !----------------------------------------------------------------------
00219     !
00220 
00221     !...get saturation vapor pressure ('e-star') values for canopy,
00222     !...soil, and snow
00223 
00224     ppl(1) = sib%prog%ps*100.0
00225     ttl(1) = sib%prog%tc
00226 
00227     call dtess_eau(1,ppl,ttl,esst,dtesst)
00228 
00229     sib_loc%etc  = esst(1)
00230     sib_loc%getc = dtesst(1)
00231 
00232     ttl(1) = sib%prog%td(sib%prog%nsl+1)
00233 
00234     call dtess_eau(1,ppl,ttl,esst,dtesst)
00235 
00236     sib_loc%etg = esst(1) 
00237     sib_loc%getg = dtesst(1)
00238 
00239     !...vapor pressures come out of eau_sat in Pa, so convert to mb
00240     sib_loc%etc  = sib_loc%etc/100.0
00241     sib_loc%getc = sib_loc%getc/100.0
00242     sib_loc%etg  = sib_loc%etg/100.0
00243     sib_loc%getg = sib_loc%getg/100.0
00244 
00245 
00246     if(sib%prog%nsl < 0 ) then
00247         ttl(1) = sib%prog%td(sib%prog%nsl+1)
00248 
00249         call dtess_eau(1,ppl,ttl,esst,dtesst)
00250 
00251         sib_loc%ets = esst(1)/100.0
00252         sib_loc%gets = dtesst(1)/100.0
00253 
00254     else
00255         sib_loc%ets   =  sib_loc%etg
00256         sib_loc%gets  =  sib_loc%getg
00257     endif
00258 
00259     !...canopy and ground fractional wetness...
00260 
00261     sib%diag%wc = MIN( one,( sib%prog%capac(1)/denh2o + &
00262         sib%prog%snow_veg)/sib%param%satcap(1) )
00263 
00264     sib%diag%wg = MAX( 0.*one, &
00265         (sib%prog%capac(2)/(sib%param%satcap(2)*denh2o)) )*0.25
00266 
00267     !-----------------------------------------------------------------------
00268     !     CALCULATION OF SOIL MOISTURE STRESS FACTOR.
00269     !     AVERAGE SOIL MOISTURE POTENTIAL IN ROOT ZONE (LAYER-2) USED AS
00270     !     SOURCE FOR TRANSPIRATION.
00271     !
00272     !      PHROOT      (PSI-R) : EQUATION (48) , SE-86
00273     !      RSTFAC(2)  F(PSI-L) : MODIFICATION OF EQUATION (12), SE-89
00274     !
00275     !     CALCULATION OF WATER STRESS AND PLANT AVAILABLE WATER HAS BEEN
00276     !     CHANGED IN SiB3. 
00277     !-----------------------------------------------------------------------
00278     !
00279 
00280 !bio...wssp is the 'water stress shape parameter'. reasonable values will be
00281 !bio...between 0.1 and 1.0. This variable controls the shape of the water stress 
00282 !bio...curve. A value of 1.0 yields a linear stress between wilt point and field 
00283 !bio...capacity, and a small value gives a steep drop-off when WP is neard-not as
00284 !bio...much stress at higher vwc amounts.
00285 
00286     sib%diag%wssp = 0.2
00287     
00288     sib%diag%paw_tot    = 0.0
00289     sib%diag%paw_max    = 0.0
00290     sib%diag%paw(:)     = 0.0
00291 
00292 !bio...Assumption is that there will be no stress if volumetric water content
00293 !bio...is above field capacity. Entire soil column is considered. Calculate
00294 !bio...amount that soil water is below FC.
00295 
00296     do i=1,nsoil
00297         sib%prog%vol_ice(i) = min(sib%param%poros,sib%prog%www_ice(i)/   &
00298             (sib%prog%dz(i)*denice))
00299         sib%diag%eff_poros(i) = sib%param%poros - sib%prog%vol_ice(i)
00300         sib%prog%vol_liq(i) = min(sib%diag%eff_poros(i),    &
00301             sib%prog%www_liq(i)/(sib%prog%dz(i)*denh2o))
00302             
00303         sib%diag%paw(i) = sib%prog%vol_liq(i) - sib%param%vwcmin
00304         sib%diag%paw(i) = max(sib%diag%paw(i),0.0_dbl_kind)
00305         sib%diag%paw_tot = sib%diag%paw_tot +      &
00306                                 sib%diag%paw(i) * sib%prog%dz(i)
00307         sib%diag%paw_max = sib%diag%paw_max +           &
00308                          ((sib%param%fieldcap - sib%param%vwcmin) * sib%prog%dz(i))
00309         enddo
00310         
00311 !bio...calculate stress factor. If total column soil moisture is at or below wilt 
00312 !bio...point, stress=0 (total stress). No stress if column-mean vwc is above FC.
00313 
00314          sib%diag%pawfrac = sib%diag%paw_tot/sib%diag%paw_max
00315      sib%diag%pawfrac = MAX(0.0_dbl_kind, sib%diag%pawfrac)
00316      sib%diag%pawfrac = MIN(sib%diag%pawfrac, 1.0_dbl_kind)
00317 
00318          sib%diag%rstfac(2) = ((1+sib%diag%wssp) * sib%diag%pawfrac)/    &
00319                               (sib%diag%wssp + sib%diag%pawfrac)
00320         
00321 !       print*,sib%diag%paw_tot, sib%diag%paw_max, sib%diag%pawfrac, sib%diag%rstfac(2)
00322         
00323     
00324     !itb...maintain rstfac2 at or above 0.1
00325     sib%diag%rstfac(2) = MAX(sib%diag%rstfac(2), 0.1_dbl_kind)
00326 
00327     !
00328 
00329     !----------------------------------------------------------------------
00330     !
00331     !      RSOIL FUNCTION FROM FIT TO FIFE-87 DATA.  Soil surface layer
00332     !         relative humidity.
00333     !
00334     !      RSOIL      (RSOIL) : EQUATION (3), Sellers et al, JGR, 100(D12)
00335     !                                          25607-25629
00336     !      HR         (Fh)    : EQUATION (66) , SE-86
00337     !----------------------------------------------------------------------
00338 
00339 ! CSR: evaporation from liquid/frozen soil
00340     fac = MIN( sib%prog%www_liq(1)/ &
00341         (sib%param%poros*sib%prog%dz(1)*denh2o) + &
00342          sib%prog%www_ice(1)/ &
00343         (sib%param%poros*sib%prog%dz(1)*denice), one )
00344 
00345 ! CSR: no evaporation from frozen soil: check in updat also!
00346 !    fac = MIN( sib%prog%www_liq(1)/ &
00347 !        (sib%param%poros*sib%prog%dz(1)*denh2o), one )
00348 
00349     fac = MAX( fac, 0.02*one  )
00350 
00351     sib%diag%rsoil =  exp(8.206 - 4.255 * fac)    
00352 
00353     psit = sib%param%phsat * fac ** (- sib%param%bee )
00354     argg = max(-10.*one,(psit*grav/ (461.5*sib%prog%td(sib%prog%nsl+1)) ))
00355     sib%diag%hr = exp(argg)
00356 
00357     !...storage inertia terms...
00358     sib%diag%cas_cap_heat = sib%prog%ros * cp * max(4.0_dbl_kind,sib%param%z2)
00359     sib%diag%cas_cap_vap  = sib%prog%ros * cv * max(4.0_dbl_kind,sib%param%z2) &
00360         / sib%diag%psy
00361     sib%diag%cas_cap_co2  = max(4.0_dbl_kind,sib%param%z2)          
00362     ! this goes 
00363     ! out to phosib
00364 
00365 end subroutine begtem
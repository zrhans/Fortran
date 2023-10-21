00001 
00002 !=====================SUBROUTINE UPDATE=================================
00003 
00004 subroutine update(sib,sib_loc)
00005 
00006 !itb====================================================================
00007 !itb
00008 !itb  MOVING STORAGE TERM UPDATES (SNOW, CAPAC) HERE FROM ENDTEM, WHICH
00009 !itb     NO LONGER EXISTS. FLUXES PREVIOUSLY CALCULATED IN ENDTEM ARE
00010 !itb     TAKEN CARE OF IN THE PROGNOSTIC C.A.S. CALCULATIONS, SO WE
00011 !itb     MERELY NEED TO TAKE CARE OF STORAGE TERMS NOW.
00012 !itb
00013 !itb      November 2000
00014 !
00015 !=======================================================================
00016 !
00017 !     UPDATING OF ALL HYDROLOGICAL PROGNOSTIC VARIABLES.  SNOW AND
00018 !        RUNOFF CALCULATIONS (SEE ALSO INTER2).  SUBROUTINES SNOW2 AND
00019 !        RUN2 OF 1D MODEL ARE INLINED IN THIS CODE.
00020 !
00021 !=======================================================================
00022 
00023 
00024 !++++++++++++++++++++++++++++++OUTPUT+++++++++++++++++++++++++++++++++++
00025 !
00026 !       DTC            CANOPY TEMPERATURE INCREMENT (K)
00027 !       DTG            GROUND SURFACE TEMPERATURE INCREMENT (K)
00028 !       WWW(3)         GROUND WETNESS 
00029 !       CAPAC(2)       CANOPY/GROUND LIQUID INTERCEPTION STORE (kg m^-2)
00030 !       SNOWW(2)       CANOPY/GROUND SNOW INTERCEPTION STORE (M)
00031 !       ROFF           RUNOFF (MM/HR)
00032 !
00033 !++++++++++++++++++++++++++DIAGNOSTICS++++++++++++++++++++++++++++++++++
00034 !
00035 !       ECMASS         CANOPY EVAPOTRANSPIRATION (MM)
00036 !       EGMASS         GROUND EVAPOTRANSPIRATION (MM)
00037 !
00038 !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
00039 
00040 
00041 use kinds
00042 use sibtype
00043 use sib_const_module, only:  &
00044     snofac,  &
00045     nsoil,   &
00046     snomel,  &
00047     denh2o,  &
00048     denice,  &
00049     phmin,   &
00050     dtt,     &
00051     dti,     &
00052     tkair,   &
00053     tkice,   &
00054     cpliq,   &
00055     cpice
00056 
00057 use physical_parameters, only:  &
00058     hltm,                &
00059     pi,                  &
00060     tice,                &
00061     cp => spec_heat_cp
00062 
00063 use eau_params, only : &
00064     lfus
00065 
00066 
00067 implicit none
00068 
00069 !----------------------------------------------------------------------
00070 
00071 type(sib_t), intent(inout) :: sib
00072 type(sib_local_vars)     ,intent(inout) :: sib_loc
00073   ! variables local to SiB
00074 
00075 !----------------------------------------------------------------------  
00076 
00077 
00078 
00079 !...LOCAL VARIABLES
00080 real(kind=dbl_kind) :: flux_imb(sib%prog%nsl+1:nsoil)
00081 real(kind=dbl_kind) :: water_imb(sib%prog%nsl+1:nsoil)
00082 real(kind=dbl_kind) :: wice0(sib%prog%nsl+1:nsoil)
00083 real(kind=dbl_kind) :: wliq0(sib%prog%nsl+1:nsoil)
00084 real(kind=dbl_kind) :: wmass(sib%prog%nsl+1:nsoil)
00085 real(kind=dbl_kind) :: sflux(sib%prog%nsl+1:nsoil)
00086 real(kind=dbl_kind) :: sflx1(sib%prog%nsl+1:nsoil)
00087 real(kind=dbl_kind) :: htstor(sib%prog%nsl+1:nsoil)
00088 real(kind=dbl_kind) :: rsnow  ! fraction of snow that is ice
00089 real(kind=dbl_kind) :: ecpot
00090 real(kind=dbl_kind) :: egpot
00091 real(kind=dbl_kind) :: espot
00092 real(kind=dbl_kind) :: hrr
00093 real(kind=dbl_kind) :: cogs1
00094 real(kind=dbl_kind) :: cogs2
00095 real(kind=dbl_kind) :: ectmax(nsoil)  ! layer max transpiration (J m^-2)
00096 real(kind=dbl_kind) :: ectmax_tot !column total max transp (J m^-2)
00097 real(kind=dbl_kind) :: egsmax
00098 real(kind=dbl_kind) :: ect_layer
00099 real(kind=dbl_kind) :: ect_diff
00100 real(kind=dbl_kind) :: facks
00101 real(kind=dbl_kind) :: cpdpsy
00102 real(kind=dbl_kind) :: timcon
00103 real(kind=dbl_kind) :: hltmi
00104 real(kind=dbl_kind) :: hfus
00105 real(kind=dbl_kind) :: ecidif
00106 real(kind=dbl_kind) :: egidif
00107 real(kind=dbl_kind) :: ecit
00108 real(kind=dbl_kind) :: egit
00109 real(kind=dbl_kind) :: temp
00110 real(kind=dbl_kind) :: propor
00111 real(kind=dbl_kind) :: heatr
00112 real(kind=dbl_kind) :: le_phase
00113 
00114 real(kind=dbl_kind) :: btran
00115 real(kind=dbl_kind) :: s_node
00116 real(kind=dbl_kind) :: rresis(nsoil)
00117 real(kind=dbl_kind) :: smp_node
00118 real(kind=dbl_kind) :: tinc(sib%prog%nsl+1:nsoil) ! temperature increment
00119 real(kind=dbl_kind) :: shcap_temp ! new heat capacity of updated top snow level 
00120 
00121 real(kind=dbl_kind), dimension(-nsnow+1:nsoil) :: 
00122         thk ! thermal conductivity of layer (W/m/K)
00123 
00124 real(kind=dbl_kind) :: bw   ! partial density of water (ice+liquid)
00125 
00126 integer(kind=int_kind) :: i,j
00127 
00128 !----------------------------------------------------------------------
00129 !
00130 !----------------------------------------------------------------------
00131 
00132 
00133     timcon = pi / 86400.0
00134 
00135     hltmi = 1. / hltm      ! inverse heat of vap, units kg/J
00136 
00137     hfus = snomel/1000.0   ! heat of fusion, units J/kg
00138 
00139     cpdpsy = cp/sib%diag%psy
00140 
00141     if(sib%prog%nsl < 0) then
00142         wice0(1) = 0.0
00143         wliq0(1) = 0.0
00144         do i=sib%prog%nsl+1,0,-1
00145             wice0(1) = wice0(1) + sib%prog%www_ice(i)
00146             wliq0(1) = wliq0(1) + sib%prog%www_liq(i)
00147         enddo
00148         !itb...patch
00149         if (wliq0(1) > 0.0 ) then
00150             rsnow = wice0(1) / (wliq0(1) + sib%prog%capac(2) )
00151         else
00152             rsnow = 1.0
00153         endif
00154         wice0(1) = 0.0
00155         wliq0(1) = 0.0
00156     else
00157         rsnow = 0.0
00158     endif
00159 
00160     !itb...effective porosity
00161     do i = sib%prog%nsl+1,nsoil
00162         sib%prog%vol_ice(i) = min(sib%param%poros, sib%prog%www_ice(i)/(sib%prog%dz(i)*denice))
00163         sib%diag%eff_poros(i) = sib%param%poros - sib%prog%vol_ice(i)
00164         sib%prog%vol_liq(i) = min(sib%diag%eff_poros(i),  &
00165             sib%prog%www_liq(i)/(sib%prog%dz(i)*denh2o))
00166         !if no effective porosity, lose liquid to runoff
00167         !itb...this is to maintain water balance...
00168         if (sib%prog%vol_liq(i) == 0.0 .and. sib%prog%www_liq(i) > 0.0 ) then
00169             sib%diag%roff = sib%diag%roff + sib%prog%www_liq(i)
00170             sib%prog%www_liq(i) = 0.0
00171         endif
00172     enddo
00173 
00174 !itb...total soil water
00175     sib%diag%www_tot_soil = 0.0
00176     do i=1,nsoil
00177      sib%diag%www_tot_soil = sib%diag%www_tot_soil +      &
00178                sib%prog%www_liq(i) + sib%prog%www_ice(i)
00179     enddo
00180 
00181 
00182 
00183     !pl this is the potential gradient in Pa
00184     !pl this WAS realized in sibslv
00185 
00186     ecpot = (sib_loc%etc + sib_loc%getc*sib_loc%dtc) - sib%prog%ea 
00187 
00188 
00189     !pl and this is the  INTERCEPTION flux in J/m2
00190 
00191     sib%diag%eci = ecpot * sib_loc%geci * sib%prog%ros * cpdpsy * dtt
00192 
00193     !pl and this is the TRANSPIRATION flux in J/m2
00194 
00195     sib%diag%ect = ecpot * sib_loc%gect * sib%prog%ros * cpdpsy * dtt
00196 
00197 !    print*,sib%diag%eci/dtt,sib%diag%ect/dtt,ecpot,sib_loc%coc,sib_loc%geci+sib_loc%gect
00198 
00199     !itb...ground interception terms will be split up further in
00200     !itb...the new code; previously, mean values were used.
00201 
00202     !pl this is the potential gradient in Pa (ground to CAS and snow to CAS)
00203     !pl this WAS realized in sibslv
00204 
00205     if(sib%prog%nsl == 0 ) then
00206         egpot = (sib_loc%etg + sib_loc%getg*sib_loc%dtd(sib%prog%nsl+1)) - sib%prog%ea 
00207         espot = 0.0
00208         sib%diag%ess = 0.0
00209 
00210 
00211         hrr = sib%diag%hr
00212         if ( sib_loc%fg < 0.5_dbl_kind ) hrr = 1.
00213 
00214         cogs1 =  sib_loc%gegs * hrr !* (1.-sib%diag%areas)                
00215         cogs2 =  sib_loc%gegs       !* (1.-sib%diag%areas)
00216 
00217         ! EVAPORATIVE flux in J/m2
00218         sib%diag%egs =  ((sib_loc%etg + sib_loc%getg * sib_loc%dtd(sib%prog%nsl+1)) * cogs1  & 
00219              -sib%prog%ea * cogs2 ) * sib%prog%ros * cpdpsy *dtt
00220 
00221         egsmax = 0.5_dbl_kind * (sib%prog%www_liq(1) + sib%prog%www_ice(1))
00222         egsmax = egsmax * hltm
00223         egsmax = max(egsmax,0.0_dbl_kind)
00224         
00225         sib%diag%egs = min( sib%diag%egs, egsmax )
00226 
00227         ! interception flux in J/m2
00228         sib%diag%egi = egpot * sib_loc%gegi * sib%prog%ros * cpdpsy * dtt
00229         
00230     else
00231 
00232         !itb...i'm going to gamble that ESS will generally be low, and not
00233         !itb...larger than the available amount of snow. This is also because
00234         !itb...small snow amounts are folded into top soil layer by subroutine
00235         !itb...combine_snow
00236 
00237         egpot = 0.0
00238         espot = (sib_loc%ets  + sib_loc%gets* sib_loc%dtd(sib%prog%nsl+1)) - sib%prog%ea  
00239         sib%diag%egs = 0.0
00240         sib%diag%egi = 0.0
00241 
00242 
00243         !itb...snow surface evaporation (new: J/m2)
00244 
00245         sib%diag%ess = espot * sib%prog%ros * cpdpsy / sib%diag%rd / snofac * dtt
00246 
00247 
00248 
00249         !itb...??? when is sib%diag%ess subtracted from snow amount?
00250         !itb...is it taken from snow water or snow ice?
00251 !            sib%prog%www_liq(sib%prog%nsl+1)*hltm*dti,              &
00252 !            sib%prog%www_ice(sib%prog%nsl+1)/snofac*hltm*dti
00253 
00254         !itb...can't evaporate more snow than you have...
00255         sib%diag%ess = min(sib%diag%ess,sib%prog%www_ice(sib%prog%nsl+1)/ snofac*hltm)
00256 
00257         !itb...now subtract it out
00258         sib%prog%www_ice(sib%prog%nsl+1) = sib%prog%www_ice(sib%prog%nsl+1) - sib%diag%ess * snofac /hltm
00259 
00260         if(sib%prog%www_ice(sib%prog%nsl+1) == 0.0 ) then
00261             if(sib%prog%www_liq(sib%prog%nsl+1) > 0.0) then
00262                 sib%prog%capac(2) = sib%prog%capac(2) +  &
00263                     sib%prog%www_liq(sib%prog%nsl+1)
00264                 sib%prog%www_liq(sib%prog%nsl+1) = 0.0
00265                 sib%prog%vol_liq(sib%prog%nsl+1) = 0.0
00266                 sib%prog%td(sib%prog%nsl+1) = 0.0
00267             endif                   ! get rid of snow layer
00268             sib%prog%dz(sib%prog%nsl+1)     = 0.0
00269             sib%prog%node_z(sib%prog%nsl+1) = 0.0
00270             sib%prog%layer_z(sib%prog%nsl)  = 0.0
00271             sib%prog%nsl = sib%prog%nsl + 1  
00272         endif
00273 
00274     endif
00275 
00276 
00277     !itb...make sure you don't evap more than you have (units are J m^-2)
00278     !itb...SiB2 restricted evaporation by limiting egs to 1/2 the water in 
00279     !itb...soil moisture layer 1. 
00280     !itb...In SiB3, we will restrict by wilting point; WP is a soil  
00281     !itb...parameter, currently must be set by user. We're working on it...
00282 
00283 
00284     !itb...Make sure you don't transpire more water than is in the soil.
00285     !itb...Have to play around with this; both evaporation and transpiration
00286     !itb...can remove water from top soil layer. Plan is to give evaporation
00287     !itb...priority in top layer, then push any transpiration deficit downwards
00288     !itb...in the soil. Idea is that lower roots will supply water (if
00289     !itb...available) to make up deficit from upper soil. 
00290 
00291     ectmax(:)  = 0.0
00292     ectmax_tot = 0.0
00293 
00294     do i=1,nsoil
00295         ectmax(i) =  sib%diag%paw(i) * denh2o * sib%prog%dz(i) * hltm
00296         ectmax_tot = ectmax_tot + ectmax(i)
00297     enddo
00298 
00299 
00300     !itb...cannot transpire more than PAW
00301 
00302     sib%diag%ect = min ( ectmax_tot, sib%diag%ect )
00303 
00304 
00305     !itb...need to do a check for positive values...
00306     !itb...FOR NOW, not allowing negative egs (dew or frost) accumulate
00307     !itb...on ground-have had problems with this in the past.
00308 
00309     btran       = 1.0E-10
00310 
00311 
00312     if(sib%diag%egs > 0.0 )then
00313        if(sib%prog%www_liq(1) > 1.0E-10) then
00314         if( (sib%diag%egs + sib%diag%ect*sib%param%rootf(1)) > egsmax ) then
00315              sib%param%rootr(1) = (0.5_dbl_kind * sib%prog%www_liq(1) * hltm -      &
00316                                                     sib%diag%egs)/sib%diag%ect
00317             sib%param%rootr(1) = MIN(sib%param%rootr(1), 1.0_dbl_kind)
00318             sib%param%rootr(1) = MAX(sib%param%rootr(1), 0.0_dbl_kind)
00319             sib%param%rootr(1) = sib%param%rootr(1) * sib%param%rootf(1)
00320             if(sib%prog%www_liq(1) <= sib%param%vwcmin) sib%param%rootr(1) = 0.0_dbl_kind
00321             
00322         else
00323 !            print*,'rootr1:',sib%param%vwcmin,sib%prog%vol_liq(1),   &
00324 !              sib%prog%vol_ice(1),sib%prog%www_liq(1),sib%prog%www_ice(1)
00325             sib%param%rootr(1) = (1.0 - sib%param%vwcmin/sib%prog%vol_liq(1)) /  &
00326                 (1.0 - sib%param%vwcmin/sib%param%fieldcap)
00327             sib%param%rootr(1) = MIN(sib%param%rootr(1), 1.0_dbl_kind)
00328             sib%param%rootr(1) = MAX(sib%param%rootr(1), 0.0_dbl_kind)
00329             sib%param%rootr(1) = sib%param%rootr(1) * sib%param%rootf(1)
00330         endif
00331       else
00332         sib%param%rootr(1) = 0.0
00333       endif
00334     else
00335         if(sib%prog%www_liq(1) > 1.0E-10) then
00336             sib%param%rootr(1) = (1.0 - sib%param%vwcmin/sib%prog%vol_liq(1)) /  &
00337                 (1.0 - sib%param%vwcmin/sib%param%fieldcap)
00338             sib%param%rootr(1) = MIN(sib%param%rootr(1), 1.0_dbl_kind)
00339             sib%param%rootr(1) = MAX(sib%param%rootr(1), 0.0_dbl_kind)
00340             sib%param%rootr(1) = sib%param%rootr(1) * sib%param%rootf(1)
00341         else
00342             sib%param%rootr(1) = 0.0
00343         endif
00344     endif
00345 
00346     
00347 
00348     !itb...no transpiration loss from frozen soil...
00349     if(sib%prog%td(1) < tice) sib%param%rootr(1) = 0.0
00350     btran = btran + sib%param%rootr(1)
00351 
00352 
00353     do i=2,nsoil
00354         if(sib%prog%td(i) >= tice .and. sib%prog%vol_liq(i) > 0.0 ) then
00355             sib%param%rootr(i) = (1.0 - sib%param%vwcmin/sib%prog%vol_liq(i)) /  &
00356                 (1.0 - sib%param%vwcmin/sib%param%fieldcap)
00357 
00358             sib%param%rootr(i) = MIN(sib%param%rootr(i), 1.0_dbl_kind)
00359             sib%param%rootr(i) = MAX(sib%param%rootr(i), 0.0_dbl_kind)
00360 
00361             sib%param%rootr(i) = sib%param%rootr(i) * sib%param%rootf(i)
00362             btran = btran + sib%param%rootr(i)           
00363         else
00364             sib%param%rootr(i) = 0.0
00365         endif
00366     enddo 
00367 
00368     !itb...normalize to get layer contribution
00369     do i=1,nsoil
00370         sib%param%rootr(i) = sib%param%rootr(i) / btran
00371         sib%param%rootr(i) = max(sib%param%rootr(i), 0.0_dbl_kind)
00372     enddo
00373 !
00374     !itb...any deficit left over will have to be partitioned into sensible heat
00375 
00376 
00377     !itb...these fluxes were all realized in sibslv. If positive, they
00378     !itb...imply transfer of water vapor INTO the CAS. If negative,
00379     !itb...they imply transfer OUT OF the CAS. We need to adjust
00380     !itb...the various reserviors as well as the CAS vapor capacity, 
00381     !itb...making sure that none go to negative values.
00382 
00383     !itb...the actual movement of vapor is taken care of in the
00384     !itb...equations in sibslv. All we do now is adjust the surface
00385     !itb...and vegetation storage reservoirs to reflect what we've
00386     !itb...already added or taken out.
00387 
00388     !pl this is the limitation to the ECI flux in J/m2
00389 
00390     ! this is the exaggerated ECI
00391     ecidif=MAX(0.0_dbl_kind,(sib%diag%eci - sib%prog%snow_veg * hltm -      &
00392         sib%prog%capac(1)  * hltm))
00393 
00394     ! the capac(1)+snow_veg limited ECI
00395     ecit   = MIN(sib%diag%eci,                                       &
00396         ( sib%prog%snow_veg*hltm + sib%prog%capac(1) * hltm))
00397 
00398     ! this is the exaggerated EGI
00399     egidif=                                                       &
00400         MAX(0.0_dbl_kind,sib%diag%egi-(sib%prog%snow_mass +                  &
00401         sib%prog%capac(2)) * hltm) * (1.-rsnow)
00402     
00403     ! the capac(2) + snow_mass limited EGI
00404     egit  =                                                       &
00405         MIN(sib%diag%egi, ((sib%prog%snow_mass +                             &
00406         sib%prog%capac(2)) * hltm + sib%diag%ess) ) * (1.-rsnow)
00407 
00408     !itb...this is a little less complicated than the old technique...
00409     !sib%diag%egi = egit
00410 
00411     ! reduce capac by max EGI possible (EGIT) 
00412     sib%prog%capac(2) = sib%prog%capac(2) - egit * (1.0-rsnow) * hltmi
00413 
00414     ! take the exaggerated EGI from the lowest soil store (it is very little)
00415     sib%prog%www_liq(nsoil)=sib%prog%www_liq(nsoil)-egidif * hltmi
00416 
00417     ! calculate mm of of water flux from ground
00418     sib%diag%egmass   = sib%diag%egi*facks * hltmi
00419 
00420 !---------------------------------------------------------------------
00421 !     CALCULATION OF SENSIBLE HEAT FLUXES FOR THE END OF THE TIMESTEP.
00422 !        SEE FIGURE (2) OF SE-86.  NOTE THAT INTERCEPTION LOSS EXCESS
00423 !        ENERGIES (ECIDIF, EGIDIF) ARE ADDED.
00424 !
00425 !      HC          (HC)    : EQUATION (63) , SE-86
00426 !      HG          (HGS)   : EQUATION (65) , SE-86
00427 !----------------------------------------------------------------------
00428 
00429     !itb...i've left the leaf one-sided, for now...
00430 
00431     sib%diag%hc = ( sib%prog%tc  - sib%prog%ta)  /sib%diag%rb       &
00432         * sib%prog%ros * cp * dtt !+ ecidif 
00433 
00434     sib%diag%chf = sib%param%czc * dti * sib_loc%dtc
00435 
00436     !itb...keep in mind that currently we are not worrying about 
00437     !itb...partial snow cover issues. with that in mind, there will
00438     !itb...only exist ground H when the is NO snow, snow H when
00439     !itb...there IS snow.
00440 
00441     !itb...ground sensible heat flux 
00442 
00443 
00444     !itb...snow fluxes
00445     if(sib%prog%nsl < 0 ) then  !SNOW case
00446 
00447         !...remember that td was updated in addinc
00448         sib%diag%hs = (sib%prog%td(sib%prog%nsl+1)  - sib%prog%ta )    &
00449             / sib%diag%rd * sib%prog%ros * cp * dtt + egidif
00450         sib%diag%hg = 0.0
00451     else
00452         sib%diag%hg = ( sib%prog%td(1) - sib%prog%ta ) /sib%diag%rd       &
00453             * sib%prog%ros * cp * dtt + egidif
00454         sib%diag%hs = 0.0
00455     endif
00456 
00457 !----------------------------------------------------------------------
00458 !     CALCULATION OF STORAGE HEAT FLUXES
00459 !
00460 !---------------------------------------------------------------------- 
00461 
00462     if (sib%prog%nsl == 0) then
00463        sib%diag%shf = sib_loc%dtd(1) * sib%param%shcap(1) * dti  + &
00464             sib%param%slamda(1) * (sib%prog%td(1) - sib%prog%td(2) )
00465     else
00466        sib%diag%shf = sib_loc%dtd(sib%prog%nsl+1) * sib%param%shcap(sib%prog%nsl+1) * dti +  &
00467             sib%param%slamda(sib%prog%nsl+1) * (sib%prog%td(sib%prog%nsl+1) -  sib%prog%td(sib%prog%nsl+2) )
00468     endif
00469 
00470 
00471 !        sib%diag%areas * ((sib_loc%dtd(sib%prog%nsl+1) * shcap_temp *        &
00472 !        dti ) * sib%prog%dz(sib%prog%nsl+1)) +                                 &
00473 
00474 !        (sib%param%tksoil(sib%prog%nsl+1) * (sib%prog%td(sib%prog%nsl+2) -      &
00475 !        sib%prog%td(sib%prog%nsl+1))/(sib%prog%node_z(sib%prog%nsl+2) -      &
00476 !        sib%prog%node_z(sib%prog%nsl+1)))  )
00477 
00478 
00479 
00480 !----------------------------------------------------------------------
00481 !    INTERCEPTION LOSSES APPLIED TO SURFACE WATER STORES.                      
00482 !    EVAPORATION LOSSES ARE EXPRESSED IN J M-2 : WHEN DIVIDED BY
00483 !    ( HLTM*1000.) LOSS IS IN M M-2. MASS TERMS ARE IN KG M-2 DT-1
00484 !    INTERCEPTION AND DRAINAGE TREATED IN INTER2.
00485 !
00486 !      CAPAC/SNOWW(1) (M-C)   : EQUATION (3)  , SE-86
00487 !      CAPAC/SNOWW(2) (M-G)   : EQUATION (4)  , SE-86
00488 !----------------------------------------------------------------------
00489 
00490     !itb...do not allow negative transpiration...
00491     !PL HERE WE DO A CHECK FOR CONDENSATION AND MAKE SURE THAT IT ONLY
00492     !PL HAPPENS TRHOUGH ECI AND EGI
00493 
00494     rsnow = (sib%prog%snow_veg/denice)/   &
00495         (sib%prog%capac(1)/denh2o + sib%prog%snow_veg/denice + 1.0E-10)
00496 
00497     facks = 1. + rsnow * ( snofac-1. )
00498 
00499     if ( (sib%diag%ect) < 0.0) then
00500         sib%diag%eci = sib%diag%ect+sib%diag%eci
00501         ecit=ecit+sib%diag%ect
00502         sib%diag%ect = 0.
00503         facks = 1. / facks
00504     endif
00505 
00506 
00507     ! reduce capac(1) by max ECI (ECIT)
00508     sib%prog%capac(1) = sib%prog%capac(1)-(1.-rsnow)*ecit*facks*hltmi
00509     sib%prog%snow_veg = sib%prog%snow_veg - rsnow*ecit * facks * hltmi
00510     sib%prog%snow_veg = max(sib%prog%snow_veg,0.0_dbl_kind)
00511     sib%prog%capac(1) = max(sib%prog%capac(1),0.0_dbl_kind)
00512 
00513     ! calculate mm of water flux from canopy
00514     sib%diag%ecmass   = sib%diag%eci*facks * hltmi
00515 
00516     ! take the rest of ECI (ECIDIF) from the lowest soil layer (very little usually)
00517     sib%prog%www_liq(nsoil) = sib%prog%www_liq(nsoil) - ecidif* hltmi
00518 
00519 !    print*,sib%diag%egi,egit,egidif,sib%diag%eci,ecit,ecidif
00520 
00521 !!----------------------------------------------------------------------
00522 !
00523 !   Calculation of phase change within snow and soil layers.
00524 !   This code based on routine CLM_MELTFREEZE from the Common
00525 !   Land Model (CLM)  (Dai et al, submitted)
00526 !   CLM web info:  http://clm.gsfc.nasa.gov
00527 !----------------------------------------------------------------------
00528 
00529     !...initialize some values
00530     sib%diag%snowmelt = 0.0
00531 
00532     do j = sib%prog%nsl+1,nsoil
00533         sib_loc%imelt(j)     = 0
00534         flux_imb(j)  = 0.0
00535         water_imb(j) = 0.0
00536         wice0(j)     = sib%prog%www_ice(j)
00537         wliq0(j)     = sib%prog%www_liq(j)
00538         wmass(j)     = sib%prog%www_ice(j) + sib%prog%www_liq(j)
00539     enddo
00540 
00541     !...calculate some fluxes. 
00542 
00543     do j = sib%prog%nsl+1,nsoil-1
00544 
00545         !itb...remember: temperature increment was added in addinc.
00546 
00547         !itb...sflux is the 'previous timestep' flux value
00548         sflux(j) = sib%param%slamda(j)*(sib_loc%td_old(j+1)-sib_loc%td_old(j))
00549 
00550         !itb...sflx1 is the 'n+1 timestep' flux value
00551         sflx1(j) = sib%param%slamda(j)*(sib%prog%td(j+1) -sib%prog%td(j))
00552 
00553     enddo
00554 
00555 
00556     sflux(nsoil) = 0.0
00557     sflx1(nsoil) = 0.0
00558 
00559     !...melting check
00560     do j = sib%prog%nsl+1,nsoil
00561 
00562         !...if ice exists in warm conditions, melt it
00563         if(sib%prog%www_ice(j) > 0.0 .and. sib%prog%td(j) > tice) then
00564             sib_loc%imelt(j)  = 1
00565             tinc(j) = sib%prog%td(j) - tice
00566             sib%prog%td(j) = tice
00567         endif
00568 
00569         !...if water exists in cold conditions, freeze it
00570         if(sib%prog%www_liq(j) > 0.0 .and. sib%prog%td(j) < tice) then
00571             sib_loc%imelt(j)  = 2
00572             tinc(j) = sib%prog%td(j) - tice
00573             sib%prog%td(j) = tice
00574         endif
00575 
00576     enddo
00577 
00578 
00579     !...check for existence of snow, less depth than 0.01 m
00580     if(sib%prog%nsl == 0  .and. sib%prog%snow_mass > 0.0_dbl_kind) then
00581         if(sib%prog%td(1) > tice) then
00582             sib_loc%imelt(1) = 1
00583             tinc(1) = sib%prog%td(1) - tice
00584             sib%prog%td(1)    = tice
00585         endif
00586     endif
00587 
00588     !...change in storage
00589     do j = sib%prog%nsl + 1, nsoil
00590         htstor(j) = sib%param%shcap(j)*dti*   &
00591             (sib%prog%td(j) - sib_loc%td_old(j))
00592     enddo
00593 
00594     !itb...calculate energy surplus or loss if there was melting or freezing
00595 
00596     do j = sib%prog%nsl+1,nsoil
00597         if(sib_loc%imelt(j) > 0 ) then    ! did melting/freezing occur?
00598             flux_imb(j) = tinc(j) * sib%param%shcap(j) ! (J/m^2)
00599         endif
00600     enddo
00601 
00602 
00603 
00604     !...the CLM boys say these have been checked carefully-they are the 
00605     !...result of computed error in the tridiagonal matrix solver.
00606 
00607     do j=sib%prog%nsl+1,nsoil
00608         if(sib_loc%imelt(j) == 1 .and. flux_imb(j) < 0.0 ) then
00609             sib_loc%imelt(j)    = 0
00610             flux_imb(j) = 0.0
00611         endif
00612 
00613         if(sib_loc%imelt(j) == 2 .and. flux_imb(j) > 0.0 ) then
00614             sib_loc%imelt(j)    = 0
00615             flux_imb(j) = 0.0
00616         endif
00617     enddo 
00618 
00619 
00620     do j=sib%prog%nsl+1,nsoil
00621 
00622         !...melting or freezing occurring, some error present
00623         if(sib_loc%imelt(j) > 0 .and. abs(flux_imb(j))>0.0) then
00624 
00625             !itb...water_imb > 0 ==> melting ice
00626             !itb...water_imb < 0 ==> freezing water
00627 
00628             water_imb(j) = flux_imb(j) / lfus  !(kg of water)
00629 
00630             !...snow exists, but less than critical depth. CLM boys say this
00631             !...needs work.
00632 
00633 
00634             !...LEVEL 1 - top soil layer, small amt of snow present, 
00635             !...no snow layers
00636             if(j == 1) then  
00637                 if(sib%prog%nsl == 0 .and. sib%prog%snow_depth >  &
00638                     0.01_dbl_kind .and. water_imb(j) > 0.0_dbl_kind) then 
00639 
00640                     temp = sib%prog%snow_mass
00641                     sib%prog%snow_mass = max(0.0_dbl_kind,temp-water_imb(j))
00642                     propor = sib%prog%snow_mass/temp
00643                     sib%prog%snow_depth = sib%prog%snow_depth * propor
00644 
00645                     heatr = flux_imb(j)*dti -       &
00646                         lfus*(temp-sib%prog%snow_mass)*dti
00647 
00648                     if(heatr > 0.0) then
00649                         water_imb(j) = heatr*dtt/lfus 
00650                         flux_imb(j) = heatr
00651                     else
00652                         water_imb(j) = 0.0
00653                         flux_imb(j)  = 0.0
00654                     endif
00655 
00656                     sib%diag%snowmelt = max(0.0_dbl_kind,  &
00657                         (temp-sib%prog%snow_mass))*dti
00658                     le_phase = lfus*sib%diag%snowmelt  ! w/m^2
00659                 endif  
00660             endif ! j==1
00661 
00662             heatr = 0.0
00663             if(water_imb(j) > 0.0) then
00664                 sib%prog%www_ice(j) = max(0.0_dbl_kind,wice0(j)-water_imb(j))
00665                 heatr = flux_imb(j)*dti  &
00666                     - lfus*(wice0(j)-sib%prog%www_ice(j))*dti
00667             elseif(water_imb(j) < 0.0_dbl_kind) then
00668                 sib%prog%www_ice(j) = min(wmass(j),wice0(j)-water_imb(j))
00669                 heatr = flux_imb(j)*dti  &
00670                     - lfus*(wice0(j)-sib%prog%www_ice(j))*dti
00671             endif
00672 
00673             sib%prog%www_liq(j) = max(0.0_dbl_kind,wmass(j)-sib%prog%www_ice(j))
00674 
00675             if(abs(heatr) > 0.0) then
00676                 sib%prog%td(j) = sib%prog%td(j) + dtt*heatr/sib%param%shcap(j)
00677 
00678                 if(sib%prog%www_liq(j)*sib%prog%www_ice(j) > 0.0 )  &
00679                     sib%prog%td(j) = tice
00680             endif 
00681 
00682             le_phase = le_phase + lfus*(wice0(j) - sib%prog%www_ice(j))*dti
00683 
00684             if(sib_loc%imelt(j) == 1 .and. j < 1 ) then
00685                 sib%diag%snowmelt = sib%diag%snowmelt +  &
00686                     max(0.0_dbl_kind, (wice0(j) - sib%prog%www_ice(j)))*dti
00687             endif
00688 
00689 
00690 
00691         endif   ! imelt \= 0 and flux_imb \= 0
00692     enddo 
00693 
00694 
00695     !...convert to (joules) - what is this needed for?
00696     !        energy_of_snowmelt = sib%diag%snowmelt * lfus
00697 
00698 !----------------------------------------------------------------------
00699 !
00700 !      END OF CLM_MELTFREEZE CODE
00701 !
00702 !----------------------------------------------------------------------
00703 
00704 !itb...calculate change in energy and water of CAS (CSR: also include water)
00705 
00706    sib%diag%cas_e_storage = sib_loc%dta * sib%diag%cas_cap_heat * dti
00707    sib%diag%cas_w_storage = sib_loc%dea * sib%diag%cas_cap_vap * dti
00708 
00709 
00710 ! CSR update radiation
00711 
00712    if (sib%prog%nsl == 0) then
00713       sib%diag%radtt(1) = sib%diag%radt(1) &
00714            - sib_loc%lcdtc*sib_loc%dtc &
00715            - sib_loc%lcdtg*sib_loc%dtd(1)
00716       
00717       sib%diag%radtt(2) = sib%diag%radt(2) &
00718            - sib_loc%lgdtc*sib_loc%dtc &
00719            - sib_loc%lgdtg*sib_loc%dtd(1)
00720       
00721       sib%diag%radtt(3) = 0.
00722    else
00723       sib%diag%radtt(1) = sib%diag%radt(1) &
00724            - sib_loc%lcdtc*sib_loc%dtc &
00725            - sib_loc%lcdts*sib_loc%dtd(sib%prog%nsl+1)
00726       
00727       sib%diag%radtt(2) = 0.
00728       
00729       sib%diag%radtt(3) = sib%diag%radt(3) &
00730            - sib_loc%lsdts*sib_loc%dtd(sib%prog%nsl+1) &
00731            - sib_loc%lsdtc*sib_loc%dtc
00732    endif
00733 
00734 
00735 end subroutine update
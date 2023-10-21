00001 !=======================================================================
00002 subroutine mapper( lat,                 &
00003                    prevDOY,             &
00004                    DOY,                 &
00005 !itb_modis
00006 !                   prevNDVI,            &
00007 !                   curNDVI,             &
00008                    prevLAI,             &
00009                    curLAI,              &
00010                    prevFPAR,            &
00011                    curFPAR,             &
00012 !itb_modis
00013                    fVCover,             &
00014                    ChiL,                &
00015                    LTran,               &
00016                    LRef,                &
00017                    MorphTab,            &
00018                    AeroVar,             &
00019                    LAIgrid,             &
00020                    fVCovergrid,         &
00021                    TimeVar )
00022 !=======================================================================
00023 ! calculates time dependant boundary condition variables for SiB.
00024 
00025 use kinds
00026 IMPLICIT NONE
00027 
00028 ! begin input variables
00029 real lat         ! center latitude of grid cell
00030 
00031 !itb_modis
00032 !real(kind=real_kind) :: curNDVI        ! FASIR NDVI values for a grid cell
00033 !real(kind=real_kind) :: prevNDVI    ! previous month's NDVI value
00034 real(kind=real_kind) :: curLAI
00035 real(kind=real_kind) :: prevLAI
00036 real(kind=real_kind) :: curFPAR
00037 real(kind=real_kind) :: prevFPAR
00038 
00039 !itb_modis
00040 
00041 
00042 real(kind=dbl_kind) :: fVCover     !
00043 real(kind=dbl_kind) :: ChiL      !
00044 real LTran(2,2)  !
00045 real LRef(2,2)   !
00046 
00047 ! begin input biome dependant, physical morphology variables
00048 type biome_morph_var
00049    real (kind=real_kind) :: zc        ! Canopy inflection height (m)
00050    real (kind=real_kind) :: LWidth    ! Leaf width
00051    real (kind=real_kind) :: LLength   ! Leaf length     
00052    real (kind=real_kind) :: LAImax    ! Maximum LAI
00053    real (kind=real_kind) :: stems     ! Stem area index
00054    real (kind=real_kind) :: NDVImax   ! Maximum NDVI
00055    real (kind=real_kind) :: NDVImin   ! Minimum NDVI
00056    real (kind=real_kind) :: SRmax     ! Maximum simple ratio
00057    real (kind=real_kind) :: SRmin     ! Minimum simple ratio
00058 end type biome_morph_var
00059 
00060 type(biome_morph_var) MorphTab
00061 
00062 ! begin input aerodynamic parameters
00063 type aero_var
00064    real (kind=real_kind) :: zo        ! Canopy roughness coeff 
00065    real (kind=real_kind) :: zp_disp  ! Zero plane displacement
00066    real (kind=real_kind) :: RbC      ! RB Coefficient
00067    real (kind=real_kind) :: RdC      ! RC Coefficient
00068 end type aero_var
00069 
00070 type(aero_var) AeroVar(50,50) ! aerodynamic interpolation tables
00071 
00072 real (kind=real_kind) :: LAIgrid(50)       ! grid of LAI values for lookup table
00073 real (kind=real_kind) :: fVCovergrid(50)   ! grid of fVCover values for 
00074                                          !  interpolation table
00075 
00076 ! begin time dependant, output variables
00077 type time_dep_var
00078    real (kind=real_kind) :: fPAR    ! Canopy absorbed fraction of PAR
00079    real (kind=real_kind) :: LAI     ! Leaf-area index
00080    real (kind=real_kind) :: Green   ! Canopy greeness fraction of LAI
00081    real (kind=real_kind) :: zo      ! Canopy roughness coeff 
00082    real (kind=real_kind) :: zp_disp ! Zero plane displacement
00083    real (kind=real_kind) :: RbC     ! RB Coefficient (c1)
00084    real (kind=real_kind) :: RdC     ! RC Coefficient (c2)
00085    real (kind=real_kind) :: gmudmu  ! Time-mean leaf projection
00086 end type time_dep_var
00087 
00088 type(time_dep_var) TimeVar
00089 
00090 ! begin internal variables
00091 real (kind=real_kind) ::DOY         ! Day of Year (DOY) of ndvi input map
00092 real (kind=real_kind) ::prevDOY     ! Day of Year (DOY) of previous ndvi map
00093 
00094 real(kind=real_kind), parameter :: fPARmax=0.95
00095                  ! Maximum possible FPAR corresponding to 98th percentile
00096 real(kind=real_kind), parameter :: fPARmin=0.01
00097                  ! Minimum possible FPAR corresponding to 2nd percentile
00098 !     For more information on fPARmin and fPARmax, see
00099 !     Sellers et al. (1994a, pg. 3532); Los (1998, pg. 29, 37-39)
00100 
00101    !-----------------------------------------------------------------------
00102    ! Calculate time dependant variables
00103    !-----------------------------------------------------------------------
00104    ! Calculate first guess fPAR 
00105    ! use average of Simple Ratio (SR) and NDVI methods.
00106 
00107 !itb_modis; don't need to do these anymore...
00108 !   call AverageAPAR (prevNDVI, MorphTab%NDVImin, MorphTab%NDVImax,   &
00109 !                     MorphTab%SRmin, MorphTab%SRmax, fPARmax,        &
00110 !                            fParmin, prevfPAR)
00111 
00112 !   call AverageAPAR (curNDVI, MorphTab%NDVImin, MorphTab%NDVImax,    &
00113 !                     MorphTab%SRmin, MorphTab%SRmax, fPARmax,        &
00114 !                            fParmin, TimeVar%fPAR)
00115 
00116    ! Calculate leaf area index (LAI) and greeness fraction (Green)
00117    !   See S. Los et al 1998 section 4.2.
00118 
00119    !   Select previous month
00120 
00121 !itb_modis
00122    call laigrn (curFPAR, prevfPAR, DOY, prevDOY, fPARmax, fVCover,  &
00123                     MorphTab%stems, MorphTab%LAImax, TimeVar%Green,   &
00124                     TimeVar%LAI)
00125 
00126    ! Interpolate to calculate aerodynamic, time varying variables
00127    call AeroInterpolate (curLAI, fVCover, LAIgrid,fVCovergrid,   &
00128                          AeroVar, TimeVar%zo, TimeVar%zp_disp,        &
00129                          TimeVar%RbC, TimeVar%RdC)
00130 
00131    ! Calculate mean leaf orientation to par flux (gmudmu)
00132    call gmuder (lat, DOY, ChiL, TimeVar%gmudmu)
00133 
00134    ! recalculate fPAR adjusting for Sun angle, vegetation cover fraction,
00135    ! and greeness fraction, and LAI
00136    call aparnew (TimeVar%LAI, TimeVar%Green, LTran, LRef,   &
00137                  TimeVar%gmudmu, fVCover, TimeVar%fPAR,     &
00138                  fPARmax, fPARmin)
00139 
00140 !itb_modis
00141      TimeVar%LAI  = curLAI
00142      TimeVar%fPAR = curFPAR
00143 
00144 !print*,'mapper;',TimeVar%LAI,TimeVar%fPAR
00145 
00146    return
00147 end subroutine mapper
00148 
00149 
00150 !-SUBROUTINE: averageapar--------------------------------------------
00151 
00152 subroutine averageapar ( ndvi, ndvimin, ndvimax, srmin, srmax, &
00153         fparmax, fparmin, fpar )
00154 
00155     !----------------------------------------------------------------
00156     !
00157     ! Calucluates Canopy absorbed fraction of Photosynthetically
00158     ! Active Radiation (fPAR) using an average of the Simple Ratio (sr)
00159     ! and NDVI methods (Los et al. (1999), eqn. 5-6). The empirical
00160     ! SR method assumes a linear relationship between fPAR and SR.
00161     ! The NDVI assumes a linear relationship between fPAR and NDVI.
00162     !
00163     !----------------------------------------------------------------
00164 
00165     use kinds
00166     
00167     implicit none
00168 
00169     !-Parameters-----------------------------------------------------
00170     real(kind=real_kind) :: ndvi        ! normalized NDVI for vegetation type
00171     real(kind=real_kind) :: ndvimin     ! minimum NDVI for vegetation type
00172     real(kind=real_kind) :: ndvimax     ! maximum NDVI for vegetation type
00173     real(kind=real_kind) :: srmin       ! minimum SR for vegetation type
00174     real(kind=real_kind) :: srmax       ! maximum SR for vegetation type
00175     real(kind=real_kind) :: fparmax     ! maximum possible fPAR, corresponding
00176                                         !       to 98th percentile
00177     real(kind=real_kind) :: fparmin     ! minimum possible fPAR, corresponding
00178                                         !       to 2nd percentile
00179 
00180     real(kind=real_kind) :: fpar        ! canopy absorbed fraction of PAR
00181 
00182     !-Local Variables------------------------------------------------
00183     real(kind=real_kind) :: locndvi     ! local copy of ndvi
00184     real(kind=real_kind) :: sr          ! simple reatio of near IR and
00185                                         !       visible radiances
00186     real(kind=real_kind) :: ndvifpar    ! fPAR from NDVI method
00187     real(kind=real_kind) :: srfpar      ! fPAR from SR method
00188 
00189     ! Switch to local value of ndvi to prevent and changes
00190     locndvi = ndvi
00191 
00192     ! Insure calculated NDVI value falls within physical limits
00193     ! for vegetation type
00194 
00195 
00196     locndvi = max(locndvi, ndvimin)
00197     locndvi = min(locndvi, ndvimax)
00198 
00199     ! Calculate simple ratio (SR)
00200     sr=(1.+LocNDVI)/(1.-LocNDVI)
00201 
00202         
00203     ! Calculate fPAR using SR method (Los et al. (1999), eqn. 5)
00204 
00205 
00206     srfpar = (sr - srmin) * (fparmax - fparmin) / (srmax - srmin) + fparmin
00207 
00208     ! Calculate fPAR using NDVI method (Los et al. (1999), eqn. 6)
00209     ndvifpar = (locndvi - ndvimin) * (fparmax - fparmin) /  &
00210         (ndvimax - ndvimin) + fparmin
00211 
00212 
00213     ! Take the mean of the 2 methods
00214     fpar = 0.5 * (srfpar + ndvifpar)
00215 
00216     return
00217 
00218 end subroutine averageapar
00219 
00220 
00221 !=======================================================================
00222 subroutine laigrn (fPAR,fPARm,DOY,DOYm,fPARmax,fVCover,stems, LAImax,Green,LAI)
00223 !=======================================================================
00224 ! calculate leaf area index (LAI) and greenness fraction (Green) from fPAR. 
00225 ! LAI is linear with vegetation fraction and exponential with fPAR.
00226 ! See Sellers et al (1994), Equations 7 through 13.
00227 
00228 use kinds                                                                      
00229 implicit none
00230 
00231 ! begin input variables
00232 real fPAR     ! fraction of PAR absorbed by plants at current time
00233 real fPARm    ! fraction of PAR absorbed by plants at previous time
00234 real DOY      ! Day of Year (DOY) of the current time
00235 real DOYm     ! Day of Year (DOY) of the previous time
00236 real fPARmax  ! maximum possible FPAR corresponding to 98th percentile
00237 real(kind=dbl_kind) :: fVCover  ! vegetation cover fraction
00238 real stems    ! stem area index for the specific biome type
00239 real LAImax   ! maximum total leaf area index for specific biome type
00240 
00241 ! begin output variables
00242 real Green    ! greeness fraction of the total leaf area index
00243 real LAI      ! area average total leaf area index
00244 
00245 ! begin internal variables
00246 real LAIg     ! green leaf area index at current time
00247 real LAIgm    ! green leaf area index at previous time
00248 real LAId     ! dead leaf area index at current time
00249 real dTime    ! number of days between previous time and current time
00250 
00251 ! Calculate current and previous green leaf area index (LAIg and LAIgm):
00252 ! LAIg is log-linear with fPAR.  Since measured fPAR is an area average, 
00253 ! divide by fVCover to get portion due to vegetation.  Since fVCover can
00254 ! be specified, check to assure that calculated fPAR does not exceed fPARMax.
00255 
00256 
00257    if(fPAR/fVCover.ge.fPARmax) then
00258       LAIg=LAImax
00259    else
00260       LAIg=alog(1.-fPAR/real(fVCover))*LAImax/alog(1-fPARmax)
00261    endif
00262 
00263    if(fPARm/fVCover.ge.fPARmax) then
00264       LAIgm=LAImax
00265    else
00266       LAIgm=alog(1.-fPARm/real(fVCover))*LAImax/alog(1-fPARmax)
00267    endif
00268 
00269 
00270    ! Calculate dead leaf area index (LAId):
00271    ! If LAIg is increasing or unchanged, the vegetation is in growth mode.
00272    ! LAId is then very small (very little dead matter).
00273    ! If LAIg is decreasing, the peak in vegetation growth has passed and
00274    ! leaves have begun to die off.  LAId is then half the change in LAIg,
00275    ! assuming half the dead leaves fall off.
00276 
00277    !     Growth mode dead leaf area index:
00278    if (LAIg.ge.LAIgm) LAId=0.0001
00279 
00280    !     die-off (post peak growth) dead leaf area index:
00281    !                  time between previous and current values is needed
00282    if (DOY.ge.DOYm) dTime = DOY-DOYm
00283    if (DOY.lt.DOYm) dTime = DOY+(365.-DOYm)
00284    ! percentage of loss retained as dead leaves depends on time passed
00285    ! if 1 month, 50% is retained. if 0 days, 100% retained, 2 months 25%    
00286    if (LAIg.lt.LAIgm) LAId=(LAIgm-LAIg)*(0.5**(dTime/30.4))
00287 
00288 
00289    ! Calculate area average, total leaf area index (LAI):
00290    LAI=(LAIg+LAId+stems)*fVCover
00291 
00292    ! Calculate greeness fraction (Green):
00293    ! Greeness fraction=(green leaf area index)/(total leaf area index)
00294    Green=LAIg/(LAIg+LAId+stems)
00295 
00296    return                                                                    
00297 end subroutine laigrn
00298 
00299 
00300 
00301 !-SUBROUTINE: aerointerpolate----------------------------------------
00302 
00303 subroutine aerointerpolate ( lai, fvcover, laigrid, fvcovergrid, &
00304         aerovar, zo, zp_disp, rbc, rdc )
00305 
00306     !----------------------------------------------------------------
00307     !
00308     ! This subroutine calculates the aerodynamic parameters by
00309     ! bi-linear interpolation from a lookup tabke of previously
00310     ! calculated values.
00311     ! The interporation table is a numpts x numpts LAI/fVCover grid
00312     ! with LAI ranging from 0.02 to 10 and fVCover ranging from
00313     ! 0.01 to 1.
00314     !
00315     !----------------------------------------------------------------
00316     
00317     use kinds
00318 
00319     implicit none
00320 
00321     !-Parameters-----------------------------------------------------
00322     real(kind=real_kind) :: lai         ! actual area averages LAI
00323     real(kind=dbl_kind)  :: fvcover     ! vegetation cover fraction
00324     real(kind=real_kind) :: laigrid(50) ! grid of LAI values
00325     real(kind=real_kind) :: fvcovergrid(50) ! grid of fVCover values
00326 
00327     type aero_var
00328     real(kind=real_kind) :: zo          ! canopy roughness coefficient
00329     real(kind=real_kind) :: zp_disp     ! zero plane displacement
00330     real(kind=real_kind) :: rbc         ! RB coefficient
00331     real(kind=real_kind) :: rdc         ! RD coefficient
00332     end type aero_var
00333 
00334     type(aero_var) aerovar(50,50)       ! interpolation table
00335 
00336     real(kind=real_kind) :: rbc         ! interpolated RB coefficient
00337     real(kind=real_kind) :: rdc         ! interpolated RD coefficient
00338     real(kind=real_kind) :: zo          ! interpolated roughness length
00339     real(kind=real_kind) :: zp_disp     ! interpolated zero plane displacement
00340 
00341     !-Local Variables------------------------------------------------
00342     integer(kind=int_kind) :: i         ! index for LAI grid
00343     integer(kind=int_kind) :: j         ! index for fVCover grid
00344     real(kind=real_kind) :: loclai      ! local LAI
00345     real(kind=dbl_kind) :: locfvcover  ! local fVCover
00346     real(kind=dbl_kind) :: temp
00347     real(kind=real_kind) :: dlai        ! grid spacing between LAI values
00348     real(kind=real_kind) :: dfvcover    ! grid spacing between fVCover values
00349 
00350     ! Calculate grid spacing (assumed fixed)
00351     dlai = laigrid(2) - laigrid(1)
00352     dfvcover = fvcovergrid(2) - fvcovergrid(1)
00353 
00354     ! Assign input LAI and fVCover to local variables and make sure 
00355     ! they lie within the limits of the interpolation tables, assuring
00356     ! the LAI and fVConver values returned from the subroutine are not
00357     ! modified.
00358     loclai = max(lai, 0.02)
00359     temp = 0.01
00360     locfvcover = max(fvcover, temp)
00361 
00362     ! Determine the nearest array location for the desired LAI and fVCover
00363     i = int(loclai / dlai) + 1
00364     j = int(locfvcover / dfvcover) + 1
00365     j = min(j, 49)
00366 
00367     ! Interpolate RbC variable
00368     call interpolate( laigrid(i), loclai, dlai, &
00369         fvcovergrid(j), locfvcover, dfvcover,   &
00370         aerovar(i,j)%rbc, aerovar(i+1,j)%rbc,   &
00371         aerovar(i,j+1)%rbc, aerovar(i+1,j+1)%rbc, rbc )
00372 
00373     ! Interpolate RdC variable
00374     call interpolate( laigrid(i), loclai, dlai, &
00375         fvcovergrid(j), locfvcover, dfvcover,   &
00376         aerovar(i,j)%rdc, aerovar(i+1,j)%rdc,   &
00377         aerovar(i,j+1)%rdc, aerovar(i+1,j+1)%rdc, rdc )
00378 
00379     ! Interpolate roughness length
00380     call interpolate( laigrid(i), loclai, dlai, &
00381         fvcovergrid(j), locfvcover, dfvcover,   &
00382         aerovar(i,j)%zo, aerovar(i+1,j)%zo,     &
00383         aerovar(i,j+1)%zo, aerovar(i+1,j+1)%zo, zo )
00384 
00385     ! Interpolate zero plane displacement
00386     call interpolate( laigrid(i), loclai, dlai,       &
00387         fvcovergrid(j), locfvcover, dfvcover,         &
00388         aerovar(i,j)%zp_disp, aerovar(i+1,j)%zp_disp, &
00389         aerovar(i,j+1)%zp_disp, aerovar(i+1,j+1)%zp_disp, zp_disp )
00390 
00391 
00392     return
00393 
00394 end subroutine aerointerpolate
00395 
00396 
00397 !=======================================================================      
00398 subroutine gmuder (Lat, DOY, ChiL, gmudmu)
00399 !=======================================================================      
00400 ! calculates time mean leaf projection relative to the Sun.
00401 
00402 use kinds
00403 implicit none
00404 
00405 ! begin input variables
00406 real Lat      ! latitude in degrees
00407 real DOY      ! day-of-year (typically middle day of the month)
00408 real(kind=dbl_kind) :: ChiL      ! leaf angle distribution factor
00409 
00410 ! begin output variables
00411 real gmudmu   ! time mean projected leaf area normal to Sun
00412 
00413 ! begin internal variables
00414 integer itime ! time counter
00415 real gtime     ! time from 0:00 Greenwhich Mean Time (GMT)
00416 real coshr    ! cosine of the Greenwhich Meridian (GM) Hour angle
00417 real mu       ! cosine of the Solar zenith angle
00418 real chiv     ! dummy variable for leaf angle distribution factor
00419 real dec      ! declination of the Sun (Solar Declination)
00420 real sin_dec  ! sine of the solar declination
00421 real cos_dec  ! cosine of the solar declination
00422 real pi       ! the constant pi
00423 real pi180    ! conversion factor from degrees to radians
00424 real aa       ! minimum possible LAI projection vs. cosine Sun angle
00425 real bb       ! slope leaf area projection vs. cosine Sun angle
00426 real cloud    ! (?) Cloud cover fraction
00427 real fb       ! (?) mean cosine of Sun Angle
00428 real swdown   ! (?) downward shortwave flux
00429 real pardif   ! (?) PAR flux difracted into canopy by clouds
00430 real pardir   ! (?) PAR flux directly onto canopy
00431 real difrat   ! (?) fraction of shortwave flux defracted by clouds
00432 real vnrat    ! (?) shortwave flux ratio of some sort
00433 real tor      ! (?) TBD
00434 real topint   ! (?) Total flux onto canopy adjusted for sun angle
00435 real botint   ! (?) total PAR flux onto canopy during 24 hour period
00436 
00437 ! Assign values to constants
00438 data pi /3.141592/
00439 
00440    pi180=pi/180. 
00441    cloud=0.5
00442 
00443    ! Calculate solar declination in radians
00444    dec=pi180*23.5*sin(1.72e-2*(DOY-80))
00445 
00446    ! Calculate sine and cosine of solar declination
00447    sin_dec=sin(dec)                                                         
00448    cos_dec=cos(dec)
00449 
00450    ! Begin time loop to average leaf projection over 24 hour period
00451    topint=0.
00452    botint=0.
00453 
00454    do itime=1, 48, 1                                                     
00455 
00456       ! Calculate time from zero Greenwhich Mean Time (GMT)
00457       gtime=0.5*real(itime) 
00458 
00459       ! Calculate cosine of hour angle of Grenwhich Meridion (GM)
00460       coshr=cos(-pi+gtime/24.*2.*pi)
00461 
00462       ! Calculate cosine of the Sun angle (mu)
00463       !     longitude=GM=0 degrees, latitude=Lat
00464       mu=sin(Lat*pi180)*sin_dec+cos(Lat*pi180)*cos_dec*coshr
00465 
00466       ! Ensure the cosine of Sun angle is positive, but not zero
00467       !     e.g., daylight, sun angle<=89.4 degrees (about start disc set/rise)
00468       mu=amax1(0.01, mu)                                            
00469 
00470       ! It looks like this code calculates the direct and difracted PAR based
00471       ! on the solar constant and a cloud fraction of 0.5 at the top and
00472       ! bottom of the atmosphere.  During night, mu=0.01, a constant.  These
00473       ! calculations do not match the definition of G(mu)/mu described in 
00474       ! Bonan (1996) and Sellers (1985).
00475       tor = 0.7**(1./mu)
00476       swdown = 1375.*mu*(tor+0.271-0.294*tor)
00477       difrat = 0.0604/(mu-0.0223)+0.0683
00478       difrat = max(difrat,0.)
00479       difrat = min(difrat,1.)
00480       difrat = difrat+(1.-difrat)*cloud
00481       vnrat  = (580.-cloud*464.)/((580.-cloud*499.) + (580.-cloud*464.))
00482       pardir = (1.-difrat)*vnrat*swdown
00483       pardif = difrat*vnrat*swdown
00484       topint = topint+pardir*mu+pardif*0.5
00485       botint = botint+pardir+pardif
00486 
00487    enddo                                                                 
00488 
00489    ! Calculate what looks like a mean value of something
00490    fb=topint/botint
00491 
00492    ! Calculate min and slope of LAI projection 
00493    chiv=ChiL                                                               
00494    if (abs(chiv) .le. 0.01) chiv=0.01
00495    !   calculate minimum value of projected leaf area
00496    aa=0.5-0.633*chiv-0.33*chiv*chiv
00497    !   calculate slope of projected leaf area wrt cosine sun angle
00498    bb=0.877*(1.-2.*aa)                                             
00499 
00500    ! Calculate mean projected LAI in Sun direction assuming fb approximates
00501    ! the mean cosine of the sun angle
00502    gmudmu=(aa+bb*fb)/fb                                            
00503 
00504    return   
00505                                                                     
00506 end subroutine gmuder
00507 
00508 
00509 
00510 !-SUBROUTINE: aparnew-----------------------------------------------
00511 
00512 subroutine aparnew ( lai, green, ltran, lref, gmudmu, fvcover, fpar, & 
00513         fparmax, fparmin )
00514 
00515     !----------------------------------------------------------------
00516     !
00517     ! Recomputes the Canopy absorbed fraction of Photosynthetically
00518     ! Active Radiation (fPAR), adjusting for solar zenith angle and
00519     ! the vegetation cover fraction (fVCover) using a modified form
00520     ! of Beer's lae.
00521     ! See Selleres et al. Part II (1996), eqns. 9-13.
00522     !
00523     !----------------------------------------------------------------
00524 
00525     use kinds
00526 
00527     implicit none
00528 
00529     !-Parameters-----------------------------------------------------
00530     real(kind=real_kind) :: lai         ! Leaf Area Index
00531     real(kind=real_kind) :: green       ! Greeness fraction of LAI
00532 
00533     ! For LTran and LRef:
00534     !   (1,1) : shortwave, green plants
00535     !   (2,1) : longwave, green plants
00536     !   (1,2) : shortwave, brown plants
00537     !   (2,2) : longwave, brown plants
00538     real(kind=real_kind) :: ltran(2,2)  ! Leaf transmittance of 
00539                                         !       green/brown plants
00540     real(kind=real_kind) :: lref(2,2)   ! Leaf reflectance for
00541                                         !       green/brown plants
00542     real(kind=real_kind) :: gmudmu      ! Daily time-mean canopy optical depth
00543     real(kind=dbl_kind)  :: fvcover     ! Canopy cover fraction
00544     real(kind=real_kind) :: fparmax     ! Maximum possible FPAR corresponding
00545                                         !       to 98th percentile
00546     real(kind=real_kind) :: fparmin     ! Minimum possible FPAR corresponding
00547                                         !       to 2nd percentile
00548 
00549     !-Local Variables------------------------------------------------
00550     real(kind=real_kind) :: fpar        ! Area average canopy absorbed fraction
00551                                         !       of PAR
00552     real(kind=real_kind) :: scatp       ! Canopy transmittance + reflectance
00553                                         !       wrt PAR
00554     real(kind=real_kind) :: park        ! Mean canopy absorption optical depth
00555                                         !       wrt PAR
00556 
00557     ! Calculate canopy transmittance + reflectance coefficient wrt PAR
00558     ! transmittance + reflectance coefficients = green plants + brown plants
00559     scatp = green * (ltran(1,1) + lref(1,1)) + (1. - green) * &
00560         (ltran(1,2) + lref(1,2))
00561 
00562     ! Caclulate PAR absorption optical depth in canopy adjusting for
00563     ! variance in projected leaf area wrt solar zenith angle
00564     ! (Sellers et al. Part II (1996), eqn. 13b)
00565     ! PAR absorption coefficient = (1 - scatp)
00566     park = sqrt(1. - scatp) * gmudmu
00567 
00568     ! Calculate the new fPAR (Sellers er al. Part II (1996), eqn 9)
00569     fpar = fvcover * (1. - exp(-park * lai / fvcover))
00570 
00571     ! Ensure calculated fPAR falls within physical limits
00572     fpar = amax1(fparmin, fpar)
00573     fpar = amin1(fparmax, fpar)
00574 
00575     return
00576 
00577 end subroutine aparnew
00578 
00579 
00580 
00581 !=======================================================================
00582 subroutine interpolate(x1, x, Dx, y1, y, Dy, z11, z21, z12, z22, z)
00583 !=======================================================================
00584 ! calculates the value of z=f(x,y) by linearly interpolating
00585 ! between the 4 closest data points on a uniform grid.  The subroutine
00586 ! requires a grid point (x1, y1), the grid spacing (Dx and Dy), and the 
00587 ! 4 closest data points (z11, z21, z12, and z22).
00588 
00589 use kinds
00590 
00591 ! begin input variables
00592 real x1  ! the x grid location of z11
00593 real x   ! x-value at which you will interpolate z=f(x,y)
00594 real Dx  ! grid spacing in the x direction
00595 real y1  ! the y grid location of z11
00596 !real y   ! y-value at which you will interpolate z=f(x,y)
00597 real(kind=dbl_kind) :: y   ! y-value at which you will interpolate z=f(x,y)
00598 real Dy  ! grid spacing in the y direction
00599 real z11 ! f(x1, y1)
00600 real z21 ! f(x1+Dx, y1)
00601 real z12 ! f(x1, y1+Dy)
00602 real z22 ! f(x1+Dx, y1+Dy)
00603 
00604 ! begin output variables
00605 real z   ! f(x,y), the desired interpolated value
00606 
00607 ! begin internal variables
00608 real zp  ! z'=first interpolated value at (x, y1)
00609 real zpp ! z''=second interpolated value at (x, Y1+Dy)
00610 
00611 
00612    ! interpolate between z11 and z21 to calculate z' (zp) at (x, y1)
00613    zp=z11+(x-x1)*(z21-z11)/Dx
00614 
00615    ! interpolate between z12 and z22 to calculate z'' (zpp) at (x, Y1+Dy)
00616    zpp=z12+(x-x1)*(z22-z12)/Dx
00617 
00618    ! interpolate between zp and zpp to calculate z at (x,y)
00619    z=zp+(y-y1)*(zpp-zp)/Dy
00620 
00621    return
00622    
00623 end subroutine interpolate
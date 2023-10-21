00001 
00002 
00003 
00004 
00005 
00006 module sibtype
00007 
00008 !----------------------------------------------------------------------
00009 !
00010 !   New SiB module: SiB returns to a single-point model, for 
00011 !   reasons of compatability with BUGS and MPI offline code (SiBDRV)
00012 !   as well as to adhere to the CCM4 coding standard
00013 !
00014 ! Modifications
00015 !  created Ian Baker, 06 Feb 2002
00016 !  Kevin Schaefer changed tdew1/2 to sh1/2 (8/17/04)
00017 !  Kevin Schaefer moved respfactor variables to param branch var tree (5/6/05)
00018 !  Kevin Schaefer added auto, hetero, and total respiration (5/6/05) 
00019 !
00020 !----------------------------------------------------------------------
00021 
00022 use kinds
00023 use sib_const_module, only: nsoil, nsnow, physmax
00024 
00025 implicit none
00026 
00027 !jlc...These need to be public
00028 public  sib_t
00029 public  sib_local_vars
00030 
00031 !jlc...Not sure what these need to be
00032 public param_vars
00033 public prognostic_vars
00034 public diagnostic_vars
00035 public sib_status
00036 
00037 
00038 !------------------------------------------------------------------
00039 !                   BOUNDARY CONDITION VARIABLES
00040 !------------------------------------------------------------------
00041 type param_vars
00042 
00043 
00044     !...boundary conditions--TIME INVARIANT
00045     real(kind=dbl_kind) :: biome     ! biome type (see refs for description)
00046     real(kind=dbl_kind) :: chil      ! leaf angle distribution factor (-)
00047     real(kind=dbl_kind) :: phc       ! 1/2 crit leaf water pot limit (m)
00048     real(kind=dbl_kind) :: z1        ! canopy bottom (m)
00049     real(kind=dbl_kind) :: z2        ! canopy top (m)
00050     real(kind=dbl_kind) :: poros     ! soil porosity (zero to one)
00051     real(kind=dbl_kind) :: satco     ! hydraulic conductivity at 
00052     !               saturation (m/s)
00053     real(kind=dbl_kind) :: bee       ! Clapp & Hornberber 'b' exponent (-)
00054     real(kind=dbl_kind) :: phsat     ! soil tension at saturation (m)
00055     real(kind=dbl_kind) :: slope     ! cosine of mean slope (-)
00056     real(kind=dbl_kind) :: vcover    ! fraction of vegetation cover (0-1)
00057     real(kind=dbl_kind) :: vmax0(5)  ! Rubisco vel of sun leaf (mol/m^2/sec)
00058     !   (1) C3 photosynthesis
00059     !   (2) C4 photosynthesis
00060     !   (3) open
00061     !   (4) open
00062     !   (5) open
00063     real(kind=dbl_kind) :: trop(5)   ! temp coeff in GS-A model (K)
00064     !   (1) C3 photosynthesis
00065     !   (2) C4 photosynthesis
00066     !   (3) open
00067     !   (4) open
00068     !   (5) open
00069     real(kind=dbl_kind) :: trda(5)   ! temp coeff in GS-A model (K^-1)
00070     !   (1) C3 photosynthesis
00071     !   (2) C4 photosynthesis
00072     !   (3) open
00073     !   (4) open
00074     !   (5) open
00075     real(kind=dbl_kind) :: trdm(5)   ! temp coeff in GS-A model (K)
00076     !   (1) C3 photosynthesis
00077     !   (2) C4 photosynthesis
00078     !   (3) open
00079     !   (4) open
00080     !   (5) open
00081     real(kind=dbl_kind) :: respcp(5) ! respiration fraction of vmax0 (-)
00082     !   (1) C3 photosynthesis
00083     !   (2) C4 photosynthesis
00084     !   (3) open
00085     !   (4) open
00086     !   (5) open
00087     real(kind=dbl_kind) :: slti(5)   ! slope of lo-temp inhibition (K^-1)
00088     !   function (K^-1)
00089     !   (1) C3 photosynthesis
00090     !   (2) C4 photosynthesis
00091     !   (3) open
00092     !   (4) open
00093     !   (5) open
00094     real(kind=dbl_kind) :: shti(5)   ! slop of hi-temp inhibition (K^-1)
00095     !   function (K-1)
00096     !   (1) C3 photosynthesis
00097     !   (2) C4 photosynthesis
00098     !   (3) open
00099     !   (4) open
00100     !   (5) open
00101     real(kind=dbl_kind) :: hltii(5)  ! 1/2 point of lo-temp inhibition (K)
00102     !   function
00103     !   (1) C3 photosynthesis
00104     !   (2) C4 photosynthesis
00105     !   (3) open
00106     !   (4) open
00107     !   (5) open
00108     real(kind=dbl_kind) :: hhti(5)   ! 1/2 point of hi-temp inhibition (K)
00109     !   function
00110     !   (1) C3 photosynthesis
00111     !   (2) C4 photosynthesis
00112     !   (3) open
00113     !   (4) open
00114     !   (5) open
00115 !itb_frost...
00116     real(kind=dbl_kind) :: hfti(5)  ! 1/2 point of frost inhibition (K)
00117     !   function
00118     !   (1) C3 photosynthesis
00119     !   (2) C4 photosynthesis
00120     !   (3) open
00121     !   (4) open
00122     !   (5) open
00123     real(kind=dbl_kind) :: sfti(5)   ! slope of frost inhibition (K)
00124     !   function
00125     !   (1) C3 photosynthesis
00126     !   (2) C4 photosynthesis
00127     !   (3) open
00128     !   (4) open
00129     !   (5) open
00130     real(kind=dbl_kind) :: tcmin
00131 !itb_frost...
00132     real(kind=dbl_kind) :: soref(2)  ! soil reflectance (-)
00133     !   (1) shortwave
00134     !   (2) longwave
00135     real(kind=dbl_kind) :: effcon(5) ! quantum efficiency (mol/mol)
00136     !   (1) C3 photosynthesis
00137     !   (2) C4 photosynthesis
00138     !   (3) open
00139     !   (4) open
00140     !   (5) open
00141     real(kind=dbl_kind) :: binter(5) ! conductance-photosynthesis 
00142     !   intercept (mol m^-2 sec^-1)
00143     !   (1) C3 photosynthesis
00144     !   (2) C4 photosynthesis
00145     !   (3) open
00146     !   (4) open
00147     !   (5) open
00148     real(kind=dbl_kind) :: gradm(5)  ! conductance-photosynthesis slope 
00149     !   parameter (-)
00150     !   (1) C3 photosynthesis
00151     !   (2) C4 photosynthesis
00152     !   (3) open
00153     !   (4) open
00154     !   (5) open
00155     real(kind=dbl_kind) :: atheta(5) ! WC WE coupling parameter (-)
00156     !   (1) C3 photosynthesis
00157     !   (2) C4 photosynthesis
00158     !   (3) open
00159     !   (4) open
00160     !   (5) open
00161     real(kind=dbl_kind) :: btheta(5) ! WC&WE, WS coupling parameter (-)
00162     !   (1) C3 photosynthesis
00163     !   (2) C4 photosynthesis
00164     !   (3) open
00165     !   (4) open
00166     !   (5) open
00167     real(kind=dbl_kind) :: zm        ! respiration parameter - exponent
00168     real(kind=dbl_kind) :: wopt      ! respiration parameter - optimum (-)
00169     !  soil moisture
00170     real(kind=dbl_kind) :: woptzm    ! wopt to the zm exponent
00171     real(kind=dbl_kind) :: wsat      ! respiration parameter (-)
00172     real(kind=dbl_kind) :: sandfrac  ! soil sand fraction
00173     real(kind=dbl_kind) :: clayfrac  ! soil clay fraction
00174     real(kind=dbl_kind) :: physfrac(5)
00175     ! physiology fraction-5 elements must add
00176     !  up to 1.0
00177     !  (1) C3 vegetation
00178     !  (2) C4 vegetation
00179     !  (3) open
00180     !  (4) open
00181     !  (5) open
00182     real(kind=dbl_kind) :: physfrac1(5)
00183     real(kind=dbl_kind) :: physfrac2(5)
00184     real(kind=dbl_kind) :: physfrac3(5)
00185     ! physiology fraction-5 elements must add up to 1.0
00186     !  (1) C3 vegetation
00187     !  (2) C4 vegetation
00188     !  (3) open
00189     !  (4) open
00190     !  (5) open
00191 
00192     integer(kind=int_kind) :: phystype(5)
00193     ! physiology type-will be either 3 or 4
00194     !  (1) C3 vegetation - always 3
00195     !  (2) C4 vegetation - always 4
00196     !  (3) open
00197     !  (4) open
00198     !  (5) open
00199     real(kind=dbl_kind) :: rootf(nsoil)
00200     ! root fraction
00201     real(kind=dbl_kind) :: rootr(nsoil)
00202     ! adjusted root fraction
00203     real(kind=dbl_kind) :: tran(2,2) ! leaf transmittance (-)
00204     !  (1,1) - shortwave, green plants
00205     !  (1,2) - longwave, green plants
00206     !  (2,1) - shortwave, brown plants
00207     !  (2,2) - longwave, brown plants
00208     real(kind=dbl_kind) :: ref(2,2)  ! leaf reflectance (-)
00209     !  (1,1) - shortwave, green plants
00210     !  (1,2) - longwave, green plants
00211     !  (2,1) - shortwave, brown plants
00212     !  (2,2) - longwave, brown plants
00213 
00214     real(kind=dbl_kind) :: het_respfac(nsoil) 
00215     ! hetrotrophic respiration factor for balancing soil heterotrophic
00216     !  respiration with annual canopy net assimilation (-)
00217 
00218     real(kind=dbl_kind) :: auto_respfac 
00219     ! autotrophic respiration factor for balancing growth/maintenance
00220     !  respiration with annual canopy net assimilation (-)
00221 
00222     real(kind=dbl_kind) :: tot_an(13)       ! total canopy net assimilation
00223     real(kind=dbl_kind) :: tot_gpp(13)      ! total Gross Primary Production
00224     real(kind=dbl_kind) :: tot_rc(13)       ! total canopy autotrophic resp
00225     real(kind=dbl_kind) :: tot_fpar(13)     ! total fraction absorbed PAR
00226     real(kind=dbl_kind) :: tot_ss(13,nsoil) ! total soilscale
00227     real(kind=dbl_kind) :: tot_nee(13)      ! total NEE
00228     real(kind=dbl_kind) :: tot_het(13)      ! total heterotrophic resp
00229     real(kind=dbl_kind) :: tot_auto(13)     ! total autotrophic resp
00230     ! used for calculating annual heterotrophic and autotrophic respfactors
00231 
00232 !itb_iso
00233 
00234     real(kind=dbl_kind) :: tot_d13c(13)     ! total d13C of photosynthesized 
00235                                             !   material, weighted by GPP
00236 
00237     real(kind=dbl_kind) :: d13c_het         ! del13C of heterotrophic respiration
00238                                             !  (per mil vs PDB)
00239 
00240     real(kind=dbl_kind) :: d13c_auto(6)
00241                                             ! del 13C of autotrophic respiration
00242                                             !  (per mil vs PDB)
00243 
00244     real(kind=dbl_kind) :: d13c_psn(6)
00245                                             ! del13C of photosnythesized material,
00246                                             !  by phystype, for the integration period;
00247                                             !  used to calculate autotrophic resp.
00248                                             !  
00249 
00250     real(kind=dbl_kind) :: psn_accum(6)
00251                                             ! accumulated photosynthetic material;
00252                                             !  used to calculate d13C_psn.
00253                                             !  (mol/m2)
00254 
00255 !itb_iso_end
00256 
00257 
00258     real(kind=dbl_kind) :: tkmg(nsoil)   ! thermal conductivity, soil 
00259     ! minerals (W m^-1 K^-1)
00260     real(kind=dbl_kind) :: tksatu(nsoil) ! thermal conductivity, saturated
00261     !   soil (W m^-1 K^-1)
00262     real(kind=dbl_kind) :: tkdry(nsoil)
00263     ! thermal conductivity, dry soil 
00264     !                   (W m^-1 K^-1)      
00265     real(kind=dbl_kind) :: tksoil(-nsnow+1:nsoil) 
00266     ! ground and snow thermal 
00267     !  conductivity (W m^-1 K^-1)
00268     real(kind=dbl_kind) :: slamda(-nsnow+1:nsoil) 
00269     ! CLM heat flux term
00270     !   (see begtem.F) (m^2 K W^-1)
00271     real(kind=dbl_kind) :: csolid(nsoil)
00272     ! heat capacity, soil solids
00273     !                         (J/m^3/K)
00274     real(kind=dbl_kind) :: shcap(-nsnow+1:nsoil)
00275     ! soil total heat capacity
00276     !                         (J/m^2/K)
00277     real(kind=dbl_kind) :: satcap(2) ! saturation capacity depth for vegetation
00278     !   (1) and ground (2) (m)
00279     real(kind=dbl_kind) :: czc       ! canopy heat capacity (J m^-2 K-1)
00280 
00281     real(kind=dbl_kind) :: vwcmin    ! soil wilting point (volumetric)
00282     real(kind=dbl_kind) :: fieldcap  ! soil field capacity (volumetric)
00283 
00284     !...boundary conditions-TIME VARYING
00285 
00286 !itb_modis; 
00287 !    real(kind=real_kind) :: NDVI      ! Normalized Difference Vegetation Index (-)
00288 !    real(kind=real_kind) :: NDVI1     ! Normalized Difference Vegetation Index (-)
00289 !    real(kind=real_kind) :: NDVI2     ! Normalized Difference Vegetation Index (-)
00290 !    real(kind=real_kind) :: NDVI3     ! Normalized Differenc eVegetation Index (-)
00291 !    real(kind=real_kind) :: NDVI_time1 ! NDVI time (day of year)
00292 !    real(kind=real_kind) :: NDVI_time2 ! NDVI time (day of year)
00293 !    real(kind=real_kind) :: NDVI_time3 ! NDVI time (day of year)
00294 !    integer(kind=int_kind) :: ndvi_period1  ! NDVI time (which period it's in)
00295 !    integer(kind=int_kind) :: ndvi_period2  ! NDVI time (which period it's in)
00296 !    integer(kind=int_kind) :: ndvi_period3  ! NDVI time (which period it's in)
00297 
00298 !itb_modis; replace NDVI arrays with LAI/fPAR arrays
00299     real(kind=real_kind) :: mlai      ! MODIS-derived LAI
00300     real(kind=real_kind) :: mlai1
00301     real(kind=real_kind) :: mlai2
00302     real(kind=real_kind) :: mlai3
00303     real(kind=real_kind) :: mfpar      ! MODIS-derived fPAR
00304     real(kind=real_kind) :: mfpar1
00305     real(kind=real_kind) :: mfpar2
00306     real(kind=real_kind) :: mfpar3
00307     real(kind=real_kind) :: modis_time1 ! MODIS time (day of year)
00308     real(kind=real_kind) :: modis_time2 ! 
00309     real(kind=real_kind) :: modis_time3 ! 
00310     integer(kind=int_kind) :: modis_period1  ! MODIS time (which period it's in)
00311     integer(kind=int_kind) :: modis_period2  ! 
00312     integer(kind=int_kind) :: modis_period3  ! 
00313 
00314 !itb_modis; end
00315 
00316     real(kind=dbl_kind) :: aparc      ! absorbed fraction of PAR (-)
00317     real(kind=dbl_kind) :: aparc1     ! absorbed fraction of PAR (-)
00318     real(kind=dbl_kind) :: aparc2     ! absorbed fraction of PAR (-)
00319     real(kind=dbl_kind) :: zlt        ! leaf area index (-)
00320     real(kind=dbl_kind) :: zlt1       ! leaf area index (-)
00321     real(kind=dbl_kind) :: zlt2       ! leaf area index (-)
00322     real(kind=dbl_kind) :: green      ! green fraction of LAI (-)
00323     real(kind=dbl_kind) :: green1     ! green fraction of LAI (-)
00324     real(kind=dbl_kind) :: green2     ! green fraction of LAI (-)
00325     real(kind=dbl_kind) :: z0d        ! roughness length (m)
00326     real(kind=dbl_kind) :: z0d1       ! roughness length (m)
00327     real(kind=dbl_kind) :: z0d2       ! roughness length (m)
00328     real(kind=dbl_kind) :: z0         ! roughness length adjust for 
00329     !   snow-covered canopy (m)
00330     real(kind=dbl_kind) :: z01        ! roughness length adjust for 
00331     !   snow-covered canopy (m)
00332     real(kind=dbl_kind) :: z02        ! roughness length adjust for 
00333     !   snow-covered canopy (m)
00334     real(kind=dbl_kind) :: zp_disp    ! zero-plane displacement (m)
00335     real(kind=dbl_kind) :: zp_disp1   ! zero-plane displacement (m)
00336     real(kind=dbl_kind) :: zp_disp2   ! zero-plane displacement (m)
00337     real(kind=dbl_kind) :: zpd_adj    ! zp_disp adjusted for snow on canopy (m)
00338     real(kind=dbl_kind) :: zpd_adj1   ! zp_disp adjusted for snow on canopy (m)
00339     real(kind=dbl_kind) :: zpd_adj2   ! zp_disp adjusted for snow on canopy (m)
00340     real(kind=dbl_kind) :: cc1        ! bulk pbl resistance coefficient (s/m)^0.5
00341     real(kind=dbl_kind) :: cc2        ! ground to CAS resistance (-)
00342     ! coefficient
00343     real(kind=dbl_kind) :: rbc        ! cc1 adjusted for snow (s/m)^0.5
00344     real(kind=dbl_kind) :: rbc1       ! cc1 adjusted for snow (s/m)^0.5
00345     real(kind=dbl_kind) :: rbc2       ! cc1 adjusted for snow (s/m)^0.5
00346     real(kind=dbl_kind) :: rdc        ! cc2 adjusted for snow (-)
00347     real(kind=dbl_kind) :: rdc1       ! cc2 adjusted for snow (-)
00348     real(kind=dbl_kind) :: rdc2       ! cc2 adjusted for snow (-)
00349     real(kind=dbl_kind) :: gmudmu     ! time-mean leaf projection (-)
00350     real(kind=dbl_kind) :: gmudmu1    ! time-mean leaf projection (-)
00351     real(kind=dbl_kind) :: gmudmu2    ! time-mean leaf projection (-)
00352 
00353     ! isotope stuff 
00354     real(kind=dbl_kind) :: d13cresp   ! del13C of respiration (per mil vs PDB)
00355     real(kind=dbl_kind) :: d13cresp1  ! del13C of respiration (per mil vs PDB)
00356     real(kind=dbl_kind) :: d13cresp2  ! del13C of respiration (per mil vs PDB)
00357     real(kind=dbl_kind) :: d13cresp3  ! del13C of respiration (per mil vs PDB)
00358 
00359 !itb...location info
00360     integer(kind=int_kind) :: pt_1x1    ! point location in the global 1x1 degree grid
00361                                       !  (used for forcing tower with 1x1 met data)
00362 
00363 end type param_vars
00364 
00365 
00366 
00367 !------------------------------------------------------------------
00368 !                   PROGNOSTIC VARIABLES
00369 !------------------------------------------------------------------
00370 type prognostic_vars
00371 
00372 
00373     real(kind=dbl_kind) :: td(-nsnow+1:nsoil)
00374     ! soil temperature (K)
00375 ! CSR use td(sib%prog%nsl+1)   real(kind=dbl_kind) :: tg        ! ground surface temp (K)
00376     real(kind=dbl_kind) :: ta        ! CAS temperature (K)
00377     real(kind=dbl_kind) :: tc        ! vegetation temperature (K)
00378     real(kind=dbl_kind) :: tha       ! CAS potential temperature (K)
00379     real(kind=dbl_kind) :: sha       ! CAS water vapor mixing ratio (kg/kg)
00380     real(kind=dbl_kind) :: ea        ! CAS water vapor pressure (hPa or mb)
00381 
00382     real(kind=dbl_kind) :: tke       ! turbulent kinetic energy (UNITS??)
00383 
00384     !...soil/snow arrays
00385 
00386     real(kind=dbl_kind) :: www_liq(-nsnow+1:nsoil)
00387     ! soil liquid (kg/m^2)
00388     real(kind=dbl_kind) :: www_ice(-nsnow+1:nsoil)
00389     ! soil ice (kg/m^2)
00390     real(kind=dbl_kind) :: vol_liq(-nsnow+1:nsoil)
00391     ! soil liquid - unitless, fraction
00392     !   of total layer volume
00393     real(kind=dbl_kind) :: vol_ice(-nsnow+1:nsoil)
00394     ! soil liquid - unitless, fraction
00395     !   of total layer volume
00396 
00397     real(kind=dbl_kind) :: node_z(-nsnow+1:nsoil)
00398     ! layer node depth (m)
00399     real(kind=dbl_kind) :: layer_z(-nsnow:nsoil)
00400     ! layer interface depth (m)
00401     real(kind=dbl_kind) :: dz(-nsnow+1:nsoil)
00402     ! layer thickness (m)
00403 
00404     real(kind=dbl_kind) :: snow_veg  ! vegetation snow cover (kg/m^2)
00405     real(kind=dbl_kind) :: snow_mass ! mass of snow on ground (kg/m^2)
00406     real(kind=dbl_kind) :: snow_depth 
00407     ! depth of snow on ground (m)
00408 
00409 
00410 
00411     real(kind=dbl_kind) :: snow_age  ! non-dimensional snow age
00412 
00413     integer(kind=int_kind) :: nsl    ! number of (actual) snow layers
00414     !   THIS NUMBER WILL BE NEGATIVE WHEN
00415     !   SNOW IS PRESENT
00416     real(kind=dbl_kind) :: capac(2)  ! vegetation and ground surface liquid
00417     !   water interception storage (kg m^-2)
00418     !   (1) canopy   (2) ground
00419 
00420     real(kind=dbl_kind) :: rst(6)    ! stomatal resistance (sec/m)
00421     !   (1) C3 photosynthesis
00422     !   (2) C4 photosynthesis
00423     !   (3) open
00424     !   (4) open
00425     !   (5) open
00426     !   (6) weighted sum over all phystypes
00427 
00428     real(kind=dbl_kind) :: pco2ap     ! (Pa) CAS CO2 partial pressure
00429     real(kind=dbl_kind) :: pco2ap_old ! (Pa) previous timestep pco2ap
00430     real(kind=dbl_kind) :: cas        ! (mole m^-2) CO2 store in Canopy air space
00431     real(kind=dbl_kind) :: cas_old    ! (mole m^-2) previous timestep CO2 store in Canopy air space
00432     real(kind=dbl_kind) :: expand     ! (mole m^-2) CAS expansion loss from previous timestep CO2 store
00433     real(kind=dbl_kind) :: pco2m      ! mixed layer CO2 partial pressure (Pa)
00434 
00435 !itb_cos
00436     real(kind=dbl_kind) :: pcosm      ! mixed layer COS partial pressure (Pa)
00437     real(kind=dbl_kind) :: pcosap     ! CAS COS partial pressure         (Pa)
00438 
00439     !...driver data/forcing
00440     real(kind=dbl_kind) :: sw_dwn    ! surface incident shortwave radiation (W/m^2)
00441     real(kind=dbl_kind) :: sw_dwn1   ! surface incident shortwave radiation (W/m^2)
00442     real(kind=dbl_kind) :: sw_dwn2   ! surface incident shortwave radiation (W/m^2)
00443     real(kind=dbl_kind) :: radvbc    ! visible beam radiation (W/m^2)
00444     real(kind=dbl_kind) :: radvdc    ! visible diffuse radiation (W/m^2)
00445     real(kind=dbl_kind) :: radnbc    ! nir beam radiation (W/m^2)
00446     real(kind=dbl_kind) :: radndc    ! nir diffuse radiation (W/m^2)
00447     real(kind=dbl_kind) :: dlwbot    ! surface incident longwave 
00448     real(kind=dbl_kind) :: dlwbot1   ! surface incident longwave 
00449     real(kind=dbl_kind) :: dlwbot2   ! surface incident longwave 
00450     real(kind=dbl_kind) :: vdcsav    ! jk used to save vdcsib2 for single point runs
00451     !                 radiation (W/m^2)
00452     real(kind=dbl_kind) :: tm        ! mixed layer temperature (K)
00453     real(kind=dbl_kind) :: tm1       ! mixed layer temperature (K)
00454     real(kind=dbl_kind) :: tm2       ! mixed layer temperature (K)
00455     real(kind=dbl_kind) :: thm       ! mixed layer potential temperature (K)
00456     real(kind=dbl_kind) :: sh        ! mixed layer water vapor mixing ratio (kg/kg)
00457     real(kind=dbl_kind) :: sh1       ! mixed layer water vapor mixing ratio (kg/kg)
00458     real(kind=dbl_kind) :: sh2       ! mixed layer water vapor mixing ratio (kg/kg)
00459     real(kind=dbl_kind) :: em        ! mixed layer water vapor 
00460     !  pressure (hPa or mb)
00461     real(kind=dbl_kind) :: ps        ! surface pressure (hPa or mb)
00462     real(kind=dbl_kind) :: ps1       ! surface pressure (hPa or mb)
00463     real(kind=dbl_kind) :: ps2       ! surface pressure (hPa or mb)
00464     real(kind=dbl_kind) :: bps(2)    ! (ps/1000)**kapa
00465     !   multiplying by bps turns a theta
00466     !   into a temperature
00467     real(kind=dbl_kind) :: psb       ! boundary layer mass depth (hPa or mb)
00468     real(kind=dbl_kind) :: zb        ! boundary layer thickness (m)
00469     real(kind=dbl_kind) :: ros       ! surface air density (kg/m^3)
00470     real(kind=dbl_kind) :: cupr      ! cumulus precipitation rate (mm/sec)
00471     real(kind=dbl_kind) :: cupr1     ! cumulus precipitation rate (mm/sec)
00472     real(kind=dbl_kind) :: cupr2     ! cumulus precipitation rate (mm/sec)
00473     real(kind=dbl_kind) :: lspr      ! stratiform precipitation rate (mm/sec) 
00474     real(kind=dbl_kind) :: lspr1     ! stratiform precipitation rate (mm/sec)  
00475     real(kind=dbl_kind) :: lspr2     ! stratiform precipitation rate (mm/sec) 
00476 
00477     real(kind=dbl_kind) :: spdm      ! wind speed (m/sec) 
00478     real(kind=dbl_kind) :: spdm1     ! wind speed (m/sec) 
00479     real(kind=dbl_kind) :: spdm2     ! wind speed (m/sec) 
00480 
00481     real(kind=dbl_kind) :: tcc1      ! cloud cover fraction
00482     real(kind=dbl_kind) :: tcc2      ! cloud cover fraction
00483 
00484     real(kind=dbl_kind) :: d13cca    ! del13C of canopy CO2 (per mil vs PDB)
00485     real(kind=dbl_kind) :: d13cm     ! del13C of ref level (per mil vs PDB)
00486 
00487 end type prognostic_vars
00488 
00489 
00490 
00491 
00492 
00493 !------------------------------------------------------------------
00494 !                   DIAGNOSTIC VARIABLES
00495 !------------------------------------------------------------------
00496 type diagnostic_vars
00497 
00498 
00499 
00500 !itb_cos...diagnostic variables...
00501     real(kind=dbl_kind) :: cosflux   ! COS uptake by plants (mol/m2/sec)
00502     real(kind=dbl_kind) :: cos_flux_pbl ! COS exchange between ref level and CAS (mol/m2/sec)
00503 
00504     real(kind=dbl_kind) :: cos_temp  ! temporary diagnostic
00505     real(kind=dbl_kind) :: cos_temp1  ! temporary diagnostic
00506 
00507     real(kind=dbl_kind) :: cos_grnd  ! COS uptake by the soil (mol/m2/sec)
00508     real(kind=dbl_kind) :: assim2    ! COS assimilation by plants (mol/m2/sec)
00509     real(kind=dbl_kind) :: rcos      ! COS flux scaled by CO2 flux (mol/m2/sec)
00510 
00511     real(kind=dbl_kind) :: coss      ! leaf surface COS (mol COS/ mol air)
00512     real(kind=dbl_kind) :: cosi      ! leaf internal COS (mol COS/ mol air)
00513     real(kind=dbl_kind) :: cosc      ! chloroplast COS (mol COS/ mol air)
00514 
00515     real(kind=dbl_kind) :: ca_soilscale(nsoil) ! this is a 'soilscale' for carbonic anhydrase,
00516                                           ! which consumes COS. this ss is different than
00517                                           ! the regular ss, in that it increases with 
00518                                           ! increasing temp, and decreases with increasing
00519                                           ! soil moisture
00520 
00521     real(kind=dbl_kind),dimension(3) :: ts3_mean   ! mean temperature of the top 3 soil layers
00522 !    real(kind=dbl_kind),dimension(3,3) :: tbar
00523 
00524 
00525 !itb_COS...end
00526 !itb_cos...end
00527 
00528     real(kind=dbl_kind) :: eastar    ! CAS saturation vapor pressure (hPa or mb)
00529     real(kind=dbl_kind) :: rha       ! CAS relative humidity (-)
00530     real(kind=dbl_kind) :: psy       ! psycrometric constant (gamma) (hPa K^-1)
00531     real(kind=dbl_kind) :: salb(2,2) ! total albedo
00532     !   (1,1) - visible, beam
00533     !   (1,2) - visible, diffuse
00534     !   (2,1) - nir, beam
00535     !   (2,2) - nir, diffuse
00536     real(kind=dbl_kind) :: cas_cap_heat
00537     ! CAS heat capacity (J/m^2/K)
00538     real(kind=dbl_kind) :: cas_cap_vap
00539     ! CAS vapor capacity (J Pa^-1 m^-2)
00540     real(kind=dbl_kind) :: cas_cap_co2
00541     ! depth of 'canopy'  (m)
00542 
00543     real(kind=dbl_kind) :: canex     ! snow depth on vegetation factor
00544     !     (unitless)
00545     real(kind=dbl_kind) :: wc        ! canopy wetness fraction (-)
00546     real(kind=dbl_kind) :: wg        ! ground wetness fraction (-)
00547     real(kind=dbl_kind) :: rstfac(4) ! stress factors (-)
00548     !  (1) leaf surface RH stress
00549     !  (2) rootzone water stress
00550     !  (3) temperature stress
00551     !  (4) product of factors 1-3
00552     real(kind=dbl_kind) :: wssp      ! water stress shape parameter. unitless.
00553                                      ! this variable controls the shape of the 
00554                                      ! water stress curve. value set/adjusted
00555                                      ! in begtem. 
00556 
00557 
00558     real(kind=dbl_kind) ::  paw_tot  ! paw, summed over all soil layers (kg m^-2)
00559     real(kind=dbl_kind) ::  paw_max  ! maximum paw, column (kg m^-2)
00560     real(kind=dbl_kind) ::  pawfrac  ! fraction of water available to plants
00561     real(kind=dbl_kind) ::  paw(nsoil) ! plant available water (volumetric)
00562 
00563 
00564     real(kind=dbl_kind) :: areas     ! snow cover fraction (zero to one)
00565     real(kind=dbl_kind) :: a_areas   ! 'apparent' areas, for computation
00566     !   purposes (will have value 0.0 or 1.0)
00567 ! CSR use td(sib%prog%nsl+1)    real(kind=dbl_kind) :: tsnow     ! snow surface temp (K)
00568     real(kind=dbl_kind) :: snow_end(3) !julian day that snow ends
00569                                        ! array positions are for 3 criteria
00570                                        ! 1) snow depth =0
00571                                        ! 2) areas < threshold value (0.05)
00572                                        ! 3) snow_mass < threshold value (1.0)
00573 
00574     real(kind=dbl_kind) :: eff_poros(-nsnow+1:nsoil)
00575     ! effective porosity for liquid 
00576     !  (unitless)
00577     real(kind=dbl_kind) :: snowmelt  ! snow water converted to liquid
00578     !  and added to soil sfc (kg m^-2 sec^-1) 
00579     real(kind=dbl_kind) :: www_tot_soil  ! total soil water-all layers, water+ice (kg m^-2)
00580     real(kind=dbl_kind) :: roff      ! total subsurface runoff out of 
00581               !soil layers during whole sib timestep dtt (mm)
00582     real(kind=dbl_kind) :: roffo     ! overland runoff (mm)
00583     real(kind=dbl_kind) :: qqq       ! part of the subsurface runoff (mm)
00584     real(kind=dbl_kind) :: hr        ! soil surface relative humidity
00585     real(kind=dbl_kind) :: hrr       ! (copy) soil surface relative humidity
00586     real(kind=dbl_kind) :: soilscale(nsoil) 
00587     ! 'R-star' from Denning et al (1996) 
00588     !    (eqn 6) (UNITS?)
00589 
00590     real(kind=dbl_kind) :: soilq10(nsoil)
00591     ! soil temperature respiration dependence 
00592     !    function (UNITS?)
00593     real(kind=dbl_kind) :: resp_grnd ! ground respiration (mol m^-2 sec^-1)
00594     real(kind=dbl_kind) :: resp_tot  ! total respiration (mol m^-2 sec^-1)
00595     real(kind=dbl_kind) :: resp_het  ! heterotrophic resp (mol m^-2 sec^-1)
00596     real(kind=dbl_kind) :: resp_auto ! autotrophic respiration (mol m^-2 sec^-1)
00597     real(kind=dbl_kind) :: www_inflow
00598     ! water inflow at ground surface 
00599     !    (kg m^-2 sec^-1)
00600 
00601     real(kind=dbl_kind) :: cu        ! momentum transfer coefficient (-)
00602     real(kind=dbl_kind) :: ct        ! thermal transfer coefficient (-)
00603     real(kind=dbl_kind) :: ustar     ! friction velocity (m sec^-1)
00604     real(kind=dbl_kind) :: drag(2)   ! drag (kg m^-2 sec^-1)
00605     real(kind=dbl_kind) :: ventmf    ! ventilation mass flux (kg m^-2 sec^-1)
00606     real(kind=dbl_kind) :: thvgm     ! sfc-reference height deficit of moisture
00607     !   UNITS ARE UNCLEAR
00608 
00609     real(kind=dbl_kind) :: ecmass    ! canopy evapotranspiration (kg m^-2 or 
00610     !                              mm water)
00611     real(kind=dbl_kind) :: egmass    ! ground evapotranspiration (kg m^-2 or 
00612     !                              mm water)
00613     real(kind=dbl_kind) :: chf       ! canopy heat storage flux (W m^-2)
00614     real(kind=dbl_kind) :: shf       ! soil heat storage flux (W m^-2)
00615 
00616     !...resistance
00617     real(kind=dbl_kind) :: ra        ! CAS-mixed layer resistance (sec/m) 
00618     real(kind=dbl_kind) :: rb        ! leaf-CAS resistance (sec/m)
00619     real(kind=dbl_kind) :: rc        ! canopy-CAS resistance (stomatal
00620     !   resistance + 2rb) (sec/m)
00621     real(kind=dbl_kind) :: rd        ! ground-CAS resistance (sec/m)
00622     real(kind=dbl_kind) :: rsoil     ! soil surface resistance (sec m^-1)
00623     real(kind=dbl_kind) :: rds       ! rd + rsoil (sec m^-1)
00624 
00625     real(kind=dbl_kind) :: ggl(6)    ! leaf conductance (sec/m)
00626     !   (1) C3 photosynthesis
00627     !   (2) C4 photosynthesis
00628     !   (3) open
00629     !   (4) open
00630     !   (5) open
00631     !   (6) weighted sum over all phystypes
00632 
00633     !...carbon
00634 
00635     real(kind=dbl_kind) :: pco2i(6)  ! leaf internal CO2 partial 
00636     !        pressure (Pa) 
00637     !   (1) C3 photosynthesis
00638     !   (2) C4 photosynthesis
00639     !   (3) open
00640     !   (4) open
00641     !   (5) open
00642     !   (6) weighted sum over all phystypes   
00643     real(kind=dbl_kind) :: pco2c(6)  ! chloroplast CO2 partial pressure (Pa)
00644     !   (1) C3 photosynthesis
00645     !   (2) C4 photosynthesis
00646     !   (3) open
00647     !   (4) open
00648     !   (5) open
00649     !   (6) weighted sum over all phystypes
00650     real(kind=dbl_kind) :: pco2s(6)  ! leaf surface CO2 partial
00651     !        pressure (Pa)
00652     !   (1) C3 photosynthesis
00653     !   (2) C4 photosynthesis
00654     !   (3) open
00655     !   (4) open
00656     !   (5) open
00657     !   (6) weighted sum over all phystypes
00658 
00659     real(kind=dbl_kind) :: thermk    ! canopy gap fraction for thermal IR 
00660     !                    radiation (-)
00661     real(kind=dbl_kind) :: tgeff     ! effective skin temp (K)
00662     ! (takes into effect vegetation, soil
00663     !       and snow)
00664     real(kind=dbl_kind) :: thgeff    ! effective skin potential temp (K)
00665     real(kind=dbl_kind) :: shgeff    ! saturation mixing ratio of effective
00666     !   skin temperature (kg/kg) 
00667     real(kind=dbl_kind) :: radt(3)   ! net radiation (W/m^2)
00668     !   1) canopy leaves
00669     !   2) ground
00670     !   3) snow 
00671     real(kind=dbl_kind) :: radtt(3)   ! net radiation (W/m^2) ttemporary
00672     !   1) canopy leaves
00673     !   2) ground
00674     !   3) snow 
00675     real(kind=dbl_kind) :: p0        ! ground surface precip (after canopy
00676     !  interception, before snow/rain
00677     !  partition) (m/sec)
00678     real(kind=dbl_kind) :: pcpg_rain ! rain fraction of precip reaching ground
00679     !  (m/sec)
00680     real(kind=dbl_kind) :: pcpg_snow ! snow fraction of precip reaching ground
00681     !  (m/sec)
00682     real(kind=dbl_kind) :: cuprt     ! copy of cupr (m/sec)
00683     real(kind=dbl_kind) :: lsprt     ! copy of lspr (m/sec)
00684 
00685     real(kind=dbl_kind) :: radc3(2)  ! absorbed radiation (W/m^2)
00686     ! (1) - radiation absorbed by canopy
00687     ! (2) - radiation absorbed by ground 
00688 
00689     real(kind=dbl_kind) :: radfac(2,2,2)
00690     ! radiation absorption factors
00691     !   (1,1,1)
00692     !   (1,1,2)
00693     !   (1,2,1)
00694     !   (1,2,2)
00695     !   (2,1,1)
00696     !   (2,1,2)
00697     !   (2,2,1)
00698     !   (2,2,2)
00699 
00700     !...diagnostics/output
00701     real(kind=dbl_kind) :: hg        ! ground sfc sensible heat flux (W m^-2)
00702     real(kind=dbl_kind) :: hc        ! canopy sensible heat flux (W m^-2)
00703     real(kind=dbl_kind) :: hs        ! snow sensible heat flux (W m^-2)
00704     real(kind=dbl_kind) :: fss       ! CAS-BL sensible heat flux (W m^-2)
00705     real(kind=dbl_kind) :: fws       ! CAS-BL latent heat flux (W m^-2)
00706     !...ec,eg, and es are intermediate values used in delef.F and sibslv.F
00707     real(kind=dbl_kind) :: ec        ! canopy latent heat flux (W m^-2)
00708     real(kind=dbl_kind) :: eg        ! ground latent heat flux (W m^-2)
00709     real(kind=dbl_kind) :: es        ! snow   latent heat flux (W m^-2)
00710 
00711 
00712     real(kind=dbl_kind) :: egi       ! latent heat flux, ground interception
00713     !   (puddles) (W m^-2) 
00714     real(kind=dbl_kind) :: eci       ! latent heat flux, canopy interception
00715     !   (puddles) (W m^-2) 
00716     real(kind=dbl_kind) :: egs       ! latent heat flux, ground evaporation
00717     !    (W m^-2)
00718     real(kind=dbl_kind) :: ess       ! snow latent heat flux (W m^-2)
00719     real(kind=dbl_kind) :: ect       ! latent heat flux, canopy transpiration
00720     !                 (W m^-2)
00721 
00722 
00723 
00724 
00725 
00726     real(kind=dbl_kind) :: aparkk    ! canopy PAR use factor (-)
00727     real(kind=dbl_kind) :: resp_can(6)  ! canopy auto resp (mol m^-2 sec^-1)
00728     !   (1) C3 photosynthesis
00729     !   (2) C4 photosynthesis
00730     !   (3) open
00731     !   (4) open
00732     !   (5) open
00733     !   (6) sum of physiological types 1-5
00734     real(kind=dbl_kind) :: pfd       ! incident PAR flux density 
00735     !           (moles m^-2 sec^-1)
00736     real(kind=dbl_kind) :: assim(6)  ! gross assimilation (mol m^-2 sec^-1)
00737     !   (1) C3 photosynthesis
00738     !   (2) C4 photosynthesis
00739     !   (3) open
00740     !   (4) open
00741     !   (5) open
00742     !   (6) weighted sum over all phystypes
00743     real(kind=dbl_kind) :: assimn(6) ! net assimilation (mol m^-2 sec^-1)
00744     !   (1) C3 photosynthesis
00745     !   (2) C4 photosynthesis
00746     !   (3) open
00747     !   (4) open
00748     !   (5) open
00749     !   (6) weighted sum over all phystypes
00750 
00751     real(kind=dbl_kind) :: cflux     ! carbon flux between CAS and reference 
00752     !   level (mol C  m^-2 sec^-1)
00753     real(kind=dbl_kind) :: assimnp(6)! assimn scaled by aparkk (mol m^-2 sec^-1)
00754     !   (1) C3 photosynthesis
00755     !   (2) C4 photosynthesis
00756     !   (3) open
00757     !   (4) open
00758     !   (5) open
00759     !   (6) weighted sum over all phystypes
00760     real(kind=dbl_kind) :: antemp(6) ! assimn bottom stopped at zero 
00761     !                  (mol m^-2 sec^-1)
00762     !   (1) C3 photosynthesis
00763     !   (2) C4 photosynthesis
00764     !   (3) open
00765     !   (4) open
00766     !   (5) open
00767     !   (6) weighted sum over all phystypes
00768     real(kind=dbl_kind) :: ansqr(6)  ! intermediate squared antemp 
00769     !   (1) C3 photosynthesis
00770     !   (2) C4 photosynthesis
00771     !   (3) open
00772     !   (4) open
00773     !   (5) open
00774     !   (6) weighted sum over all phystypes
00775     real(kind=dbl_kind) :: omepot(6) ! potential light limited photosynthesis
00776     !                  (mol m^-2 sec^-1)
00777     !   (1) C3 photosynthesis
00778     !   (2) C4 photosynthesis
00779     !   (3) open
00780     !   (4) open
00781     !   (5) open
00782     !   (6) weighted sum over all phystypes 
00783     real(kind=dbl_kind) :: assimpot(6)  
00784     ! potential biochemical limited 
00785     !   photosynthesis (mol m^-2 sec^-1)
00786     !   (1) C3 photosynthesis
00787     !   (2) C4 photosynthesis
00788     !   (3) open
00789     !   (4) open
00790     !   (5) open
00791     !   (6) weighted sum over all phystypes
00792     real(kind=dbl_kind) :: assimci(6)! potential stress limited 
00793     !   photosynthesis (mol m^-2 sec^-1)
00794     !   (1) C3 photosynthesis
00795     !   (2) C4 photosynthesis
00796     !   (3) open
00797     !   (4) open
00798     !   (5) open
00799     !   (6) weighted sum over all phystypes
00800     real(kind=dbl_kind) :: wsfws(6)  ! intermediate assimpot weighted water 
00801     !   stress factor (-)
00802     !   (1) C3 photosynthesis
00803     !   (2) C4 photosynthesis
00804     !   (3) open
00805     !   (4) open
00806     !   (5) open
00807     !   (6) weighted sum over all phystypes
00808     real(kind=dbl_kind) :: wsfht(6)  ! intermediate assimpot weighted high
00809     !   temperature stress factor (-)
00810     !   (1) C3 photosynthesis
00811     !   (2) C4 photosynthesis
00812     !   (3) open
00813     !   (4) open
00814     !   (5) open
00815     !   (6) weighted sum over all phystypes
00816     real(kind=dbl_kind) :: wsflt(6)  ! intermediate assimpot weighted low
00817     !   temperature stress factor (-)
00818     !   (1) C3 photosynthesis
00819     !   (2) C4 photosynthesis
00820     !   (3) open
00821     !   (4) open
00822     !   (5) open
00823     !   (6) weighted sum over all phystypes
00824     real(kind=dbl_kind) :: wci(6)    ! intermediate antemp weighted 
00825     !   intercellular CO2 ()
00826     !   (1) C3 photosynthesis
00827     !   (2) C4 photosynthesis
00828     !   (3) open
00829     !   (4) open
00830     !   (5) open
00831     !   (6) weighted sum over all phystypes
00832     real(kind=dbl_kind) :: whs(6)    ! intermediate antemp weighted 
00833     !   relative humidity (-)
00834     !   (1) C3 photosynthesis
00835     !   (2) C4 photosynthesis
00836     !   (3) open
00837     !   (4) open
00838     !   (5) open
00839     !   (6) weighted sum over all phystypes
00840     real(kind=dbl_kind) :: wags(6)   ! intermediate antemp weighted stomatal
00841     !   conductance ()
00842     !   (1) C3 photosynthesis
00843     !   (2) C4 photosynthesis
00844     !   (3) open
00845     !   (4) open
00846     !   (5) open
00847     !   (6) weighted sum over all phystypes
00848     real(kind=dbl_kind) :: wegs(6)   ! intermediate evaporation weighted 
00849     !   stomatal conductance ()
00850     !   (1) C3 photosynthesis
00851     !   (2) C4 photosynthesis
00852     !   (3) open
00853     !   (4) open
00854     !   (5) open
00855     !   (6) weighted sum over all phystypes
00856 
00857 
00858     !   ISOTOPE variables
00859 
00860 
00861     real(kind=dbl_kind) :: kiecps(6)  
00862     ! Kinetic isotope effect during  
00863     !  photosynthesis by phystype i 
00864     !   (1) C3 photosynthesis
00865     !   (2) C4 photosynthesis
00866     !   (3) open
00867     !   (4) open
00868     !   (5) open
00869     !   (6) weighted sum over all phystypes
00870     real(kind=dbl_kind) :: d13cassimn(6)  
00871     ! del13C of CO2 assimilated by phystype i 
00872     !   (1) C3 photosynthesis
00873     !   (2) C4 photosynthesis
00874     !   (3) open
00875     !   (4) open
00876     !   (5) open
00877     !   (6) weighted sum over all phystypes
00878     real(kind=dbl_kind) :: c13assimn(6)
00879     !total flux of C13 in CO2 assimilated 
00880     !  by phystype i 
00881     !   (1) C3 photosynthesis
00882     !   (2) C4 photosynthesis
00883     !   (3) open
00884     !   (4) open
00885     !   (5) open
00886     !   (6) weighted sum over all phystypes
00887     real(kind=dbl_kind) :: c12assimn(6) 
00888     !total flux of C12 in CO2 assimilated 
00889     !  by phystype i
00890     !   (1) C3 photosynthesis
00891     !   (2) C4 photosynthesis
00892     !   (3) open
00893     !   (4) open
00894     !   (5) open
00895     !   (6) weighted sum over all phystypes
00896     real(kind=dbl_kind) :: rcassimn(6)
00897     ! isotope ratio (13C/12C) of CO2 
00898     !   assimilated phystype i (unitless)
00899     !   (1) C3 photosynthesis
00900     !   (2) C4 photosynthesis
00901     !   (3) open
00902     !   (4) open
00903     !   (5) open
00904     !   (6) weighted sum over all phystypes
00905 
00906     real(kind=dbl_kind) :: flux13c   !
00907     real(kind=dbl_kind) :: flux12c   !
00908     real(kind=dbl_kind) :: flux_turb !
00909 
00910     real(kind=dbl_kind) :: ebal      ! energy imbalance (W/m^2)
00911     real(kind=dbl_kind) :: wbal      ! water imbalance (kg/m^2)
00912     real(kind=dbl_kind) :: gbal      ! ground energy imbalance (W/m^2)
00913     real(kind=dbl_kind) :: cbal      ! canopy energy imbalance (W/m^2)
00914     real(kind=dbl_kind) :: abal      ! canopy airspace energy imbalance (W/m^2)
00915     real(kind=dbl_kind) :: cas_w_storage   ! CAS change in water/timestep (W/m^-2)
00916     real(kind=dbl_kind) :: cas_e_storage   ! CAS change in heat/timestep (W/m^-2)
00917 
00918     real(kind=dbl_kind) :: infil ! infiltration into the soil (kg/m2/s)
00919     real(kind=dbl_kind) :: fac1 ! effective ground cover for thermal radiation (-)
00920 
00921     real(kind=dbl_kind) :: closs ! canopy thermal loss     (W m^-2)
00922     real(kind=dbl_kind) :: gloss ! ground thermal loss     (W m^-2)
00923     real(kind=dbl_kind) :: sloss ! sloss thermal loss      (W m^-2)
00924 
00925     real(kind=dbl_kind) :: capac_old(2)  ! canopy/ground liquid water storage (kg/m2)
00926     real(kind=dbl_kind) :: snow_mass_old  ! ground snow cover (kg/m2)
00927     real(kind=dbl_kind) :: snow_veg_old    ! vegetation snow cover (kg/m^2)
00928 
00929 end type diagnostic_vars
00930 
00931 
00932 
00933 !------------------------------------------------------------------
00934 !                   STATUS VARIABLES
00935 !------------------------------------------------------------------
00936 type sib_status
00937 
00938     real(kind=dbl_kind) :: coszbar
00939     real(kind=dbl_kind) :: cosz
00940     real(kind=dbl_kind) :: dayflag
00941     real(kind=dbl_kind) :: julday
00942     integer(kind=int_kind) :: pt_num  ! (-) point number in nSiB vector
00943 
00944 end type sib_status
00945 
00946 
00947 !------------------------------------------------------------------
00948 !                   TOP LEVEL ENCAPSULATING TYPE 'SIB_T'
00949 !------------------------------------------------------------------
00950 type sib_t
00951 
00952     type(param_vars) :: param          ! parameter variables
00953     type(prognostic_vars) :: prog      ! prognostic variables
00954     type(diagnostic_vars) :: diag      ! diagnostic variables
00955     type(sib_status) :: stat            ! sib status variables
00956 
00957 end type sib_t
00958 
00959 
00960 
00961 
00962 !------------------------------------------------------------------
00963 !                   LOCAL VARIABLES
00964 !------------------------------------------------------------------
00965 type sib_local_vars
00966 
00967 
00968     !...canopy air space (CAS) and canopy
00969 !    real(kind=dbl_kind) :: dtg       ! delta ground surface temp (K)
00970     real(kind=dbl_kind) :: dtd(-nsnow+1:nsoil)
00971     ! delta soil temperature (K)
00972     real(kind=dbl_kind) :: dtc       ! change in canopy temperature (K)
00973 !    real(kind=dbl_kind) :: dts       ! change in snow surface temperature (K)
00974     real(kind=dbl_kind) :: dth       ! change in ref level temperature (K)
00975     real(kind=dbl_kind) :: dqm       ! change in ref level moisture (Pa)
00976     real(kind=dbl_kind) :: dta       ! change in CAS temperature (K)
00977     real(kind=dbl_kind) :: dea       ! change in CAS moisture (Pa)
00978 
00979     real(kind=dbl_kind) :: etc       ! saturation vapor pressure at Tc (hPa)
00980     !   ('e-star' of Tc)
00981     real(kind=dbl_kind) :: getc      ! derivative of etc with respect to temp
00982     !   (d(etc)/dTc (hPa K^-1)
00983     real(kind=dbl_kind) :: etg       ! 'e-star' of ground surface (Pa)
00984     real(kind=dbl_kind) :: getg      ! d(etg)/dTg (hPa K^-1)
00985     real(kind=dbl_kind) :: ets       ! 'e-star' of snow surface (Pa)
00986     real(kind=dbl_kind) :: gets      ! d(ets)/dTs (hPa K^-1)
00987 
00988     real(kind=dbl_kind) :: fc        ! direction of vapor flux, canopy to CAS
00989     !  =1 when flux is canopy to CAS (etc>ea)
00990     !  =0 when flux is CAS to canopy (ea>etc)
00991     real(kind=dbl_kind) :: fg        ! direction of vapor flux, ground to CAS
00992     !  =1 when flux is ground to CAS (etg>ea)
00993     !  =0 when flux is CAS to ground (ea>etg)
00994     real(kind=dbl_kind) :: gect      ! dry fraction of veg / rc
00995     real(kind=dbl_kind) :: geci      ! wet fraction of veg / 2rb
00996     real(kind=dbl_kind) :: gegs      ! dry fraction of ground / rds
00997     real(kind=dbl_kind) :: gegi      ! wet fraction of ground /rd
00998     real(kind=dbl_kind) :: coc       ! gect + geci
00999     real(kind=dbl_kind) :: cog1      ! gegi + gegs*hrr
01000     real(kind=dbl_kind) :: cog2      ! gegi + gegs
01001 
01002 
01003     !...radiation
01004 
01005 
01006     integer(kind=int_kind) :: imelt(-nsnow+1:nsoil)
01007     ! flag for melting/freezing
01008     !   1=> melting, 2=> freezing
01009     real(kind=dbl_kind) :: frac_iceold(-nsnow+1:nsoil)
01010     ! start-of-timestep value for
01011     !   ice fraction of total liquid
01012     real(kind=dbl_kind) :: dtc4      ! d(canopy thermal em)/dT (W m^-2 K^-1)
01013     real(kind=dbl_kind) :: dtg4      ! d(ground thermal em)/dT (W m^-2 K^-1)
01014     real(kind=dbl_kind) :: dts4      ! d(snow thermal em)/dT   (W m^-2 K^-1)
01015     real(kind=dbl_kind) :: lcdtc     ! d(canopy thermal em)/dtc (W m^-2 K^-1)
01016     real(kind=dbl_kind) :: lcdtg     ! d(canopy thermal em)/dtg (W m^-2 K^-1)
01017     real(kind=dbl_kind) :: lcdts     ! d(canopy thermal em)/dts (W m^-2 K^-1)
01018     real(kind=dbl_kind) :: lgdtc     ! d(ground thermal em)/dtc (W m^-2 K^-1)
01019     real(kind=dbl_kind) :: lgdtg     ! d(ground thermal em)/dtg (W m^-2 K^-1)
01020     real(kind=dbl_kind) :: lsdts     ! d(snow thermal em)/dts   (W m^-2 K^-1)
01021     real(kind=dbl_kind) :: lsdtc     ! d(snow thermal em)/dtc   (W m^-2 K^-1)
01022     real(kind=dbl_kind) :: hcdtc     ! d(canopy H)/dtc  (W m^-2 K^-1)
01023     real(kind=dbl_kind) :: hcdta     ! d(canopy H)/dta  (W m^-2 K^-1)
01024     real(kind=dbl_kind) :: hgdta     ! d(ground H)/dta  (W m^-2 K^-1)
01025     real(kind=dbl_kind) :: hgdtg     ! d(ground H)/dtg  (W m^-2 K^-1)
01026     real(kind=dbl_kind) :: hsdta     ! d(snow H)/dta  (W m^-2 K^-1)
01027     real(kind=dbl_kind) :: hsdts     ! d(snow H)/dtsnow  (W m^-2 K^-1)
01028     real(kind=dbl_kind) :: hadta     ! d(CAS H)/dta  (W m^-2 K^-1)
01029     real(kind=dbl_kind) :: hadth     ! d(CAS H)/dtheta  (W m^-2 K^-1)
01030     real(kind=dbl_kind) :: ecdtc     ! d(canopy LE)/dtc (W m^-2 K^-1)
01031     real(kind=dbl_kind) :: ecdea     ! d(canopy LE)/dea (W m^-2 K^-1)
01032     real(kind=dbl_kind) :: egdtg     ! d(ground LE)/dtg (W m^-2 K^-1)
01033     real(kind=dbl_kind) :: egdea     ! d(ground LE)/dea (W m^-2 K^-1)
01034     real(kind=dbl_kind) :: esdts     ! d(snow LE)/dtsnow (W m^-2 Pa^-1)
01035     real(kind=dbl_kind) :: esdea     ! d(snow LE)/dea (W m^-2 K^-1)
01036     real(kind=dbl_kind) :: eadea     ! d(CAS LE)/dea (W m^-2 Pa^-1)
01037     real(kind=dbl_kind) :: eadem     ! d(CAS LE)/dem (W m^-2 Pa^-1)
01038     real(kind=dbl_kind) :: closs     ! canopy thermal loss     (W m^-2)
01039     real(kind=dbl_kind) :: gloss     ! ground thermal loss     (W m^-2)
01040     real(kind=dbl_kind) :: sloss     ! snow thermal loss       (W m^-2)
01041     real(kind=dbl_kind) :: fac1      ! effective ground cover for 
01042     !   thermal radiation (-)
01043 
01044     real(kind=dbl_kind) :: td_old(-nsnow+1:nsoil)
01045     ! prev timestep soil temperature (K)
01046 
01047 end type sib_local_vars
01048 
01049 
01050 end module sibtype
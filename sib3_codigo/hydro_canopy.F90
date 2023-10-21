00001 
00002 !=====================SUBROUTINE HYDRO_CANOPY===========================
00003 
00004 subroutine hydro_canopy(sib,sib_loc)
00005 
00006 use kinds
00007 use sibtype
00008 
00009 use physical_parameters, only: &
00010     tice
00011 
00012 use sib_const_module, only: &
00013     cww,    &
00014     clai,   &
00015     snomel, &
00016     denice, &
00017     denh2o, &
00018     dtt,    &
00019     dti,    &
00020     cpliq,  &
00021     cpice,  &
00022     tkice
00023 
00024 
00025 implicit none
00026 
00027 !----------------------------------------------------------------------
00028 
00029 type(sib_t), intent(inout) :: sib
00030 type(sib_local_vars)     ,intent(inout) :: sib_loc
00031 ! variables local to SiB
00032 
00033 !----------------------------------------------------------------------  
00034 
00035 
00036 !=======================================================================
00037 !
00038 !     CALCULATION OF  INTERCEPTION AND DRAINAGE OF RAINFALL AND SNOW
00039 !     INCORPORATING EFFECTS OF PATCHY SNOW COVER AND TEMPERATURE
00040 !     ADJUSTMENTS.
00041 !
00042 !----------------------------------------------------------------------
00043 !
00044 !     (1) NON-UNIFORM PRECIPITATION
00045 !         CONVECTIVE PPN. IS DESCRIBED BY AREA-INTENSITY
00046 !         RELATIONSHIP :-
00047 !
00048 !                   F(X) = A*EXP(-B*X)+C
00049 !
00050 !         THROUGHFALL, INTERCEPTION AND INFILTRATION
00051 !         EXCESS ARE FUNCTIONAL ON THIS RELATIONSHIP
00052 !         AND PROPORTION OF LARGE-SCALE PPN.
00053 !         REFERENCE: SA-89B, APPENDIX.
00054 !
00055 !
00056 !
00057 !++++++++++++++++++++++++++++++OUTPUT+++++++++++++++++++++++++++++++++++
00058 !
00059 !       TC             CANOPY TEMPERATURE (K)
00060 !       CAPAC(1)       CANOPY LIQUID INTERCEPTION STORE (kg m^-2)
00061 !       SNOW_VEG       CANOPY SNOW INTERCEPTION STORE (Kg M^-2) or (mm water)
00062 !       P0             THROUGHFALL PRECIPITATION (M/SEC)
00063 !
00064 !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
00065 
00066 !     local variables
00067 real(kind=dbl_kind) :: pcoefs(2,2)
00068 real(kind=dbl_kind) :: ap
00069 real(kind=dbl_kind) :: bp
00070 real(kind=dbl_kind) :: cp
00071 real(kind=dbl_kind) :: totalp    ! total precip (meters)
00072 real(kind=dbl_kind) :: pinf
00073 real(kind=dbl_kind) :: fpi
00074 real(kind=dbl_kind) :: xsc   ! canopy excess water (kg/m^2)
00075 real(kind=dbl_kind) :: xss   ! canopy excess snow (kg/m^2)
00076 real(kind=dbl_kind) :: xs
00077 real(kind=dbl_kind) :: capacp    ! copy of veg liquid store
00078 real(kind=dbl_kind) :: snowwp    ! copy of veg snow
00079 real(kind=dbl_kind) :: spechc    ! specific heat of canopy (intercepted
00080 !  water and vegatation )(J m^-2 deg^-1)
00081 real(kind=dbl_kind) :: chiv      ! leaf angle dist factor (unitless)
00082 real(kind=dbl_kind) :: aa
00083 real(kind=dbl_kind) :: bb
00084 real(kind=dbl_kind) :: exrain
00085 real(kind=dbl_kind) :: zload
00086 real(kind=dbl_kind) :: tti       ! direct throughfall in meters
00087 real(kind=dbl_kind) :: tex       ! canopy drainage in meters
00088 real(kind=dbl_kind) :: thru      ! total throughfall (tti + tex)
00089 real(kind=dbl_kind) :: freeze
00090 real(kind=dbl_kind) :: diff
00091 real(kind=dbl_kind) :: ccp
00092 real(kind=dbl_kind) :: cct
00093 real(kind=dbl_kind) :: tsd       ! temperature modified for canopy 
00094 ! interception
00095 real(kind=dbl_kind) :: tta
00096 real(kind=dbl_kind) :: ttb
00097 real(kind=dbl_kind) :: cca
00098 real(kind=dbl_kind) :: ccb
00099 real(kind=dbl_kind) :: ccc
00100 real(kind=dbl_kind) :: arg
00101 real(kind=dbl_kind) :: fliq      ! fraction of liquid in precip (0-1)
00102 real(kind=dbl_kind) :: bifall    ! density of falling snow (kg/m^-3)
00103 real(kind=dbl_kind) :: dz_snowfall ! new snow depth (m)
00104 real(kind=dbl_kind) :: capac1m   ! capac(1) ( canopy water in meters)
00105 
00106 integer(kind=int_kind) :: pcptype    ! precip type; 1=rain 2=snow
00107 integer(kind=int_kind) :: newnode    ! new snow layer indicator
00108 integer(kind=int_kind) :: i,j        ! loop index
00109 
00110 data pcoefs(1,1)/ 20. /, pcoefs(1,2)/ .206E-8 /, &
00111     pcoefs(2,1)/ 0.0001 /, pcoefs(2,2)/ 0.9999 /, bp /20. /
00112 
00113     !-----------------------------------------------------------------------
00114     !
00115     !     PREC ( PI-X )   : EQUATION (C.3), SA-89B
00116     !
00117     !-----------------------------------------------------------------------
00118     dz_snowfall = 0.0
00119 
00120     ap = pcoefs(2,1)
00121     cp = pcoefs(2,2)
00122     totalp  = (sib%diag%cuprt + sib%diag%lsprt) *  dtt
00123 
00124     !itb...check against ridiculously low precip amounts...
00125     if(totalp < 1.0E-10) then
00126         totalp = 0.0
00127         sib%diag%cuprt = 0.0
00128         sib%diag%lsprt = 0.0
00129     endif
00130 
00131     if( sib%prog%snow_veg >  0.0 .or. sib%prog%snow_mass >  0.0 &
00132         .or. sib%prog%tm < tice ) sib%diag%cuprt = 0.0
00133     sib%diag%lsprt = totalp/dtt - sib%diag%cuprt
00134 
00135     if(totalp > 1.e-8) then
00136         ap = sib%diag%cuprt * dtt / totalp * pcoefs(1,1) + &
00137             sib%diag%lsprt * dtt / totalp * pcoefs(2,1)
00138         cp = sib%diag%cuprt * dtt / totalp * pcoefs(1,2) + &
00139             sib%diag%lsprt * dtt / totalp * pcoefs(2,2)
00140     endif
00141 
00142     thru = 0.
00143     fpi  = 0.
00144 
00145     !----------------------------------------------------------------------
00146     !     PRECIP INPUT INTO HYDRO_CANOPY IN M/SEC; TOTALP IS IN METERS
00147     !----------------------------------------------------------------------
00148 
00149     sib%diag%p0 = totalp  !sib%diag%p0 now in meters
00150 
00151     !itb...calculate capac1m
00152     capac1m = sib%prog%capac(1)/denh2o
00153 
00154     xsc = max(0.0_dbl_kind, capac1m - sib%param%satcap(1) )
00155     capac1m = capac1m - xsc
00156     xss = max(0.0_dbl_kind, (sib%prog%snow_veg/denice - sib%param%satcap(1)) ) 
00157     sib%prog%snow_veg = (sib%prog%snow_veg/denice - xss)*denice
00158 
00159     !itb...add excess to total throughfall
00160     thru = thru + xsc + xss
00161 
00162     capacp = capac1m                !m of liquid on veg
00163     snowwp = sib%prog%snow_veg/denice    !m snow on veg
00164 
00165     !...capac + snow_veg needs to be in meters for units to work here.
00166     !...units of spechc will be J m^-2 deg^-1
00167 
00168     spechc = &
00169         min( 0.05_dbl_kind, ( capac1m + sib%prog%snow_veg/denice ) ) &
00170         * cww + sib%param%zlt * clai
00171 
00172     !----------------------------------------------------------------------
00173     !    PROPORTIONAL SATURATED AREA (XS) AND LEAF DRAINAGE(TEX)
00174     !
00175     !     TTI ( D-D )     : EQUATION (C.4), SA-89B
00176     !     XS  ( X-S )     : EQUATION (C.7), SA-89B
00177     !     TEX ( D-C )     : EQUATION (C.8), SA-89B
00178     !
00179     !    SA-89B is Sato et al, Implementing the Simple Biosphere Model 
00180     !    (SiB) in a General Circulation Model: Methodologies and Results
00181     !    NASA Contractor Report 185509 (1989)
00182     !
00183     !-----------------------------------------------------------------------
00184 
00185     chiv = sib%param%chil
00186     if ( abs(chiv) <= 0.01 ) chiv = 0.01
00187 
00188     aa = 0.5 - 0.633 * chiv - 0.33 * chiv * chiv   ! unitless
00189     bb = 0.877 * ( 1. - 2. * aa )                  ! unitless
00190     exrain = aa + bb                               ! unitless
00191 
00192     zload = capac1m + sib%prog%snow_veg/denice            ! meters
00193     fpi   = ( 1.-exp( - exrain*sib%param%zlt/sib%param%vcover ) )* sib%param%vcover
00194 
00195     !...tti is direct throughfall (meters)
00196 
00197     tti   = sib%diag%p0 * ( 1.-fpi )
00198 
00199     !...xs is fraction of canopy where intercepted plus existing storage
00200     !...exceeds capacity (unitless) 
00201 
00202     xs    = 1.
00203 
00204     if ( sib%diag%p0 >= 1.e-9 ) then  
00205 
00206         arg = ( sib%param%satcap(1)-zload )/ ( sib%diag%p0*fpi*ap ) -cp/ap 
00207 
00208         if ( arg >= 1.e-9 ) then                                 
00209             xs = -1./bp * log( arg )                                      
00210             xs = min( xs, 1.0_dbl_kind ) 
00211             xs = max( xs, 0.0_dbl_kind ) 
00212         endif   
00213     endif 
00214 
00215     !...tex is canopy drainage (meters) 
00216 
00217     tex = sib%diag%p0*fpi * &
00218         ( ap/bp*( 1.- exp( -bp*xs )) + cp*xs ) - &
00219         ( sib%param%satcap(1) - zload ) * xs                        
00220     tex = max( tex, 0.0_dbl_kind ) 
00221 
00222     !-------------------------------------------------------------
00223     !    TOTAL THROUGHFALL (THRU) AND STORE AUGMENTATION         
00224     !-----------------------------------------------------------
00225 
00226     !...thru is throughfall (meters) 
00227 
00228     thru = thru + tti + tex 
00229 
00230     !...pinf is rainfall intercepted by canopy (meters)
00231     pinf = sib%diag%p0 - thru
00232 
00233     !...make sure interception is not negative
00234     pinf = MAX(pinf,0.0_dbl_kind)
00235 
00236     if( sib%prog%tm > tice ) then
00237         capac1m = capac1m + pinf
00238     else
00239         sib%prog%snow_veg = (sib%prog%snow_veg/denice + pinf)*denice 
00240     endif       
00241 
00242     !itb...this is the old sib routine, 'ADJUST'
00243     !=======================================================================
00244     !
00245     !     TEMPERATURE CHANGE DUE TO ADDITION OF PRECIPITATION
00246     !
00247     !=======================================================================
00248 
00249     freeze = 0.
00250     diff = ( capac1m+sib%prog%snow_veg/denice - capacp - snowwp )*cww
00251  
00252     ccp = spechc
00253     cct = spechc + diff
00254 
00255     tsd = ( sib%prog%tc * ccp + sib%prog%tm * diff ) / cct
00256 
00257     if ( ( sib%prog%tc >  tice .AND. sib%prog%tm <= tice ) .or. &
00258         ( sib%prog%tc <= tice .AND. sib%prog%tm >  tice ) )then
00259 
00260         tta = sib%prog%tc
00261         ttb = sib%prog%tm
00262         cca = ccp
00263         ccb = diff
00264         if ( tsd <= tice ) then
00265 
00266             !----------------------------------------------------------------
00267             !    FREEZING OF WATER ON CANOPY 
00268             !----------------------------------------------------------------
00269 
00270             ccc = capacp * 0.001 * snomel
00271             if ( sib%prog%tc < sib%prog%tm ) ccc = diff * snomel / cww
00272             tsd = ( tta * cca + ttb * ccb + ccc ) / cct
00273 
00274             freeze = ( tice * cct - ( tta * cca + ttb * ccb ) )
00275             freeze = (min ( ccc, freeze )) / snomel
00276             if(tsd > tice) tsd = tice - 0.01
00277 
00278 
00279         else
00280 
00281             !----------------------------------------------------------------
00282             !    MELTING OF SNOW ON CANOPY 
00283             !----------------------------------------------------------------
00284 
00285             ccc = - sib%prog%snow_veg/denice * snomel 
00286             if ( sib%prog%tc > sib%prog%tm ) ccc = - diff * snomel / cww
00287 
00288             tsd = ( tta * cca + ttb * ccb + ccc ) / cct
00289 
00290             freeze = ( tice * cct - ( tta * cca + ttb * ccb ) )
00291             freeze = (max( ccc, freeze )) / snomel
00292             if(tsd <= tice)tsd = tice - 0.01
00293            
00294 
00295         endif
00296     endif
00297 
00298     sib%prog%snow_veg = (sib%prog%snow_veg/denice + freeze)*denice
00299     capac1m = capac1m - freeze
00300     sib%prog%snow_veg = max(sib%prog%snow_veg,0.0_dbl_kind)
00301     capac1m = max(capac1m,0.0_dbl_kind)
00302 
00303     xs = max( 0.0_dbl_kind, ( capac1m - sib%param%satcap(1) ) )
00304     if ( sib%prog%snow_veg/denice >= 1.0e-7_dbl_kind ) xs = capac1m
00305     capac1m = capac1m - xs
00306     sib%prog%tc = tsd
00307 
00308     !...end of 'ADJUST' code
00309 
00310 
00311     !...now assign to p0 (precip) that precipitation that was
00312     !...NOT intercepted and stored on the canopy
00313 
00314     sib%diag%p0 = thru + xs 
00315 
00316     !itb...change sib%diag%p0 back to rate (mm/sec)
00317     sib%diag%p0 = sib%diag%p0 * dti * 1000.0  ! conversion from meters to mm/sec 
00318 
00319 
00320     !...precipitation onto ground (follows clm_hydro_canopy)
00321 
00322     !...determine precip type
00323 
00324     if(sib%prog%tm >= tice + 2.5) then
00325         pcptype = 1
00326     else
00327         pcptype = 2
00328     endif
00329 
00330     !...percentage of liquid water by mass is arbitrarily set to vary 
00331     !...linearly with air temp, from 0% at tice to 40% max at (tice + 2.0)
00332 
00333     if (pcptype == 1 ) then  ! RAIN
00334         fliq = 1.0
00335         sib%diag%pcpg_snow = 0.0
00336         sib%diag%pcpg_rain = sib%diag%p0
00337 
00338     else
00339         if(sib%prog%tm <= tice) then
00340             fliq = 0.0
00341         elseif(sib%prog%tm < tice + 2.0) then
00342             fliq = -54.61 + 0.2*sib%prog%tm
00343         else
00344             fliq = 0.40
00345         endif
00346         fliq = max(0.0_dbl_kind,fliq)
00347 
00348 
00349         !...use Alta Relationship, Anderson (1976); LaChapelle (1961),
00350         !...U.S. Department of Agriculture Forest Service, Project F,
00351         !...Progress Report 1, Alta Avalanche Study Center: Snow
00352         !...Layer Densification
00353 
00354         if(sib%prog%tm > tice + 2.0) then
00355             bifall = 189.0
00356         elseif(sib%prog%tm > tice - 15.0) then
00357             bifall = 50.0 + 1.7 * (sib%prog%tm - tice + 15.0)**1.5
00358         else
00359             bifall = 50.0
00360         endif
00361 
00362         sib%diag%pcpg_snow = sib%diag%p0 * (1.0 - fliq)
00363         sib%diag%pcpg_rain = sib%diag%p0 * fliq
00364 
00365         !...snowfall rate will be in m/sec
00366         !...BE CAREFUL HERE; convervsion units of kg/m^3 for bifall with 
00367         !...                 kg/m^2/sec for precip (mm/sec) results in
00368         !...                 output of m/sec snow accumulation.
00369 
00370         dz_snowfall = sib%diag%pcpg_snow/bifall 
00371 
00372         !...snow_depth will be in meters...
00373         sib%prog%snow_depth = sib%prog%snow_depth + dz_snowfall * dtt
00374 
00375         !...add snow depth to top snow layer
00376         if(dz_snowfall > 0.0 .and. sib%prog%nsl < 0) then
00377             sib%prog%dz(sib%prog%nsl+1) = sib%prog%dz(sib%prog%nsl+1) + &
00378                 dz_snowfall * dtt
00379         endif
00380 
00381         sib%prog%snow_mass  = sib%prog%snow_mass + sib%diag%pcpg_snow * dtt
00382 
00383     endif
00384 
00385     !...initialize snow layer if accumulation exceeds 10mm..
00386 
00387 
00388     newnode = 0
00389     if(   sib%prog%nsl == 0 .and. sib%diag%pcpg_snow > 0.0 ) then 
00390         newnode                = 1
00391         sib%prog%nsl           = -1
00392         sib%prog%dz(0)         = sib%prog%snow_depth
00393         sib%prog%node_z(0)     = -0.5 * sib%prog%dz(0)
00394         sib%prog%layer_z(-1)   = -sib%prog%dz(0)
00395         sib%prog%layer_z(0)    = 0.0
00396         sib%prog%snow_age      = 0.0
00397         sib%prog%td(0)         = MIN(tice, sib%prog%ta)
00398         sib%prog%www_ice(0)    = sib%prog%snow_mass
00399         sib%prog%www_liq(0)    = 0.0
00400         sib_loc%frac_iceold(0) = 1.0
00401         sib%param%shcap(0) = cpliq*sib%prog%www_liq(0) + &
00402             cpice*sib%prog%www_ice(0)
00403         !--itb this is a patch
00404         sib%param%tksoil(0)       = tkice
00405     endif
00406 
00407     !itb...new snowfall is added here
00408     if(sib%prog%nsl < 0 .and. newnode == 0) then
00409         sib%prog%www_ice(sib%prog%nsl+1) = sib%prog%www_ice(sib%prog%nsl+1) + &
00410             dtt * sib%diag%pcpg_snow
00411         sib%prog%dz(sib%prog%nsl+1) = sib%prog%dz(sib%prog%nsl+1) + &
00412             dz_snowfall * dtt
00413     endif
00414 
00415     sib%prog%capac(1) = capac1m * denh2o
00416 
00417 
00418 end subroutine hydro_canopy
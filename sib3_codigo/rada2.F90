00001 !=================SUBROUTINE RADA2======================================
00002 subroutine rada2(sib,sib_loc)
00003 
00004 
00005 
00006     !=======================================================================
00007     !
00008     !     CALCULATION OF ALBEDOS VIA TWO STREAM APPROXIMATION( DIRECT
00009     !     AND DIFFUSE ) AND PARTITION OF RADIANT ENERGY
00010     !
00011     !-----------------------------------------------------------------------
00012 
00013 
00014     !++++++++++++++++++++++++++++++OUTPUT+++++++++++++++++++++++++++++++++++
00015     !
00016     !       SALB(2,2)      SURFACE ALBEDOS 
00017     !       TGEFF4         EFFECTIVE SURFACE RADIATIVE TEMPERATURE (K) 
00018     !       RADFAC(2,2,2)  RADIATION ABSORPTION FACTORS 
00019     !       THERMK         CANOPY GAP FRACTION FOR TIR RADIATION 
00020     !
00021     !++++++++++++++++++++++++++DIAGNOSTICS++++++++++++++++++++++++++++++++++
00022     !
00023     !       ALBEDO(2,2,2)  COMPONENT REFLECTANCES 
00024     !
00025     !
00026     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
00027     !
00028     !      REFERENCE
00029     !
00030     !      Sellers, P.J., 1985: Canopy Reflectance, Photosynthesis and
00031     !                     Respiration. Int. J. Remote Sensing, 6(8)
00032     !                     1335-1372.
00033 
00034 
00035 
00036     use kinds
00037     use sibtype
00038     use sib_const_module, only:  &
00039         zlnd
00040     use physical_parameters, only :  &
00041         tice
00042     implicit none   
00043 
00044     !----------------------------------------------------------------------
00045 
00046     type(sib_t), intent(inout) :: sib
00047     type(sib_local_vars)     ,intent(inout) :: sib_loc
00048     ! variables local to SiB
00049 
00050     !----------------------------------------------------------------------  
00051 
00052     !...LOCAL VARIABLES...
00053     integer(kind=int_kind) :: iwave, irad   ! loop variables
00054 
00055     real(kind=dbl_kind) :: fff        ! looks like a minimum cosz value
00056     real(kind=dbl_kind) :: facs       ! 1/20th of Td(1) - Tice, constrained
00057     !  to be between 0.0 and 0.4  
00058     real(kind=dbl_kind) :: fmelt      ! 1-facs
00059     real(kind=dbl_kind) :: scov
00060     real(kind=dbl_kind) :: reff1
00061     real(kind=dbl_kind) :: reff2
00062     real(kind=dbl_kind) :: tran1
00063     real(kind=dbl_kind) :: tran2
00064     real(kind=dbl_kind) :: scat
00065     real(kind=dbl_kind) :: chiv      ! copy of sib_param%chil
00066     real(kind=dbl_kind) :: aa
00067     real(kind=dbl_kind) :: bb
00068     real(kind=dbl_kind) :: proj
00069     real(kind=dbl_kind) :: extkb
00070     real(kind=dbl_kind) :: zmew
00071     real(kind=dbl_kind) :: acss
00072     real(kind=dbl_kind) :: upscat
00073     real(kind=dbl_kind) :: betao
00074     real(kind=dbl_kind) :: be
00075     real(kind=dbl_kind) :: ce
00076     real(kind=dbl_kind) :: bot
00077     real(kind=dbl_kind) :: de
00078     real(kind=dbl_kind) :: fe
00079     real(kind=dbl_kind) :: ge
00080     real(kind=dbl_kind) :: hh1
00081     real(kind=dbl_kind) :: hh2
00082     real(kind=dbl_kind) :: hh3
00083     real(kind=dbl_kind) :: hh4
00084     real(kind=dbl_kind) :: hh5
00085     real(kind=dbl_kind) :: hh6
00086     real(kind=dbl_kind) :: hh7
00087     real(kind=dbl_kind) :: hh8
00088     real(kind=dbl_kind) :: hh9
00089     real(kind=dbl_kind) :: hh10
00090     real(kind=dbl_kind) :: psi
00091     real(kind=dbl_kind) :: zat
00092     real(kind=dbl_kind) :: power1
00093     real(kind=dbl_kind) :: power2
00094     real(kind=dbl_kind) :: epsi
00095     real(kind=dbl_kind) :: ek
00096     real(kind=dbl_kind) :: albedo(2,2,2)
00097     real(kind=dbl_kind) :: f1
00098     real(kind=dbl_kind) :: zp
00099     real(kind=dbl_kind) :: den
00100     real(kind=dbl_kind) :: zmk
00101     real(kind=dbl_kind) :: tranc1(2)
00102     real(kind=dbl_kind) :: tranc2(2)
00103     real(kind=dbl_kind) :: tranc3(2)
00104     real(kind=dbl_kind) :: tsurf
00105     real(kind=dbl_kind) :: tg4
00106     real(kind=dbl_kind) :: tc4
00107     real(kind=dbl_kind) :: fac2
00108     real(kind=dbl_kind) :: zkat
00109 
00110 
00111     !
00112     !----------------------------------------------------------------------
00113     !
00114     !
00115     !     MODIFICATION FOR EFFECT OF SNOW ON UPPER STOREY ALBEDO
00116     !         SNOW REFLECTANCE   = 0.80, 0.40 . MULTIPLY BY 0.6 IF MELTING
00117     !         SNOW TRANSMITTANCE = 0.20, 0.54
00118     !
00119     !
00120     !-----------------------------------------------------------------------
00121     !
00122     !
00123 
00124     sib%diag%canex         = 1.-( sib%prog%snow_veg*5.-sib%param%z1)/  &
00125         (sib%param%z2-sib%param%z1)
00126     sib%diag%canex         = max( 0.1_dbl_kind, sib%diag%canex )
00127     sib%diag%canex         = min( 1.0_dbl_kind, sib%diag%canex )
00128 
00129 
00130     !...both satcap values are in meters: multiply by density to get
00131     !...kg/m^2...
00132     sib%param%satcap(1) = sib%param%zlt * 0.0001 * sib%diag%canex
00133 !    sib%param%satcap(2) = 0.0002           ! lahouari
00134     sib%param%satcap(2) = 0.01           ! Baker
00135 
00136     sib%diag%areas = sib%prog%snow_depth / (zlnd*10.0 + sib%prog%snow_depth)
00137 
00138     !itb...areas criteria for vanishing snow
00139     if(sib%diag%areas < 0.25 .and. sib%stat%julday > 3 ) then
00140       sib%diag%snow_end(2) = MIN(sib%diag%snow_end(2),(sib%stat%julday))
00141     endif
00142 
00143     fff = max(0.01746_dbl_kind,sib%stat%cosz)
00144 
00145     !itb...facs only accounts for ground sfc temperature-no snow influence 
00146     !      facs  = ( sib%prog%td(1) - tice ) * 0.04
00147 
00148     !itb...so we'll change it...
00149     facs = (sib%prog%td(sib%prog%nsl+1) - tice) * 0.04
00150 
00151     facs  = max( 0.0_dbl_kind , facs)
00152     facs  = min( 0.4_dbl_kind, facs)
00153     fmelt = 1.0 - facs
00154 
00155     !-----------------------------------------------------------------------
00156     do iwave = 1, 2
00157 
00158         scov =  min( 0.5_dbl_kind, (sib%prog%snow_veg/1000.0)/sib%param%satcap(1) )
00159         reff1 = ( 1. - scov ) * sib%param%ref(iwave,1) + scov * ( 1.2 -     &
00160             iwave * 0.4 ) * fmelt
00161         reff2 = ( 1. - scov ) * sib%param%ref(iwave,2) + scov * ( 1.2 -     &
00162             iwave * 0.4 ) * fmelt
00163         tran1 = sib%param%tran(iwave,1) * ( 1. - scov )                     &
00164             + scov * ( 1.- ( 1.2 - iwave * 0.4 ) * fmelt )         &
00165             * sib%param%tran(iwave,1)
00166         tran2 = sib%param%tran(iwave,2) * ( 1. - scov )                     &
00167             + scov * ( 1.- ( 1.2 - iwave * 0.4 ) * fmelt ) * 0.9   &
00168             * sib%param%tran(iwave,2)
00169         !-----------------------------------------------------------------------
00170         !
00171         !     CALCULATE AVERAGE SCATTERING COEFFICIENT, LEAF PROJECTION AND
00172         !     OTHER COEFFICIENTS FOR TWO-STREAM MODEL.
00173         !
00174         !      SCAT  (OMEGA)         : EQUATION (1,2) , SE-85
00175         !      PROJ  (G(MU))         : EQUATION (13)  , SE-85
00176         !      EXTKB (K, G(MU)/MU)   : EQUATION (1,2) , SE-85
00177         !      ZMEW  (INT(MU/G(MU))  : EQUATION (1,2) , SE-85
00178         !      ACSS  (A-S(MU))       : EQUATION (5)   , SE-85
00179         !      EXTK  (K, VARIOUS)    : EQUATION (13)  , SE-85
00180         !      UPSCAT(OMEGA*BETA)    : EQUATION (3)   , SE-85
00181         !      BETAO (1BETA-0)       : EQUATION (4)   , SE-85 
00182         !
00183         !-----------------------------------------------------------------------
00184 
00185         scat = sib%param%green*( tran1 + reff1 ) +( 1.-sib%param%green ) *  &
00186             ( tran2 + reff2)
00187         chiv = sib%param%chil
00188 
00189         if ( abs(chiv) .LE. 0.01 ) chiv = 0.01
00190 
00191         aa = 0.5 - 0.633 * chiv - 0.33 * chiv * chiv
00192         bb = 0.877 * ( 1. - 2. * aa )
00193 
00194         proj = aa + bb * fff
00195         extkb = ( aa + bb * fff ) / fff
00196         zmew = 1. / bb * ( 1. - aa / bb   &
00197             * log ( ( aa + bb ) / aa ) )
00198         acss = scat / 2. * proj / ( proj + fff * bb )
00199         acss = acss * ( 1. - fff * aa     &
00200             / ( proj + fff * bb ) * log ( ( proj   &
00201             +   fff * bb + fff * aa ) / ( fff * aa ) ) )
00202 
00203         upscat = sib%param%green * tran1 + ( 1.- sib%param%green ) * tran2
00204         upscat = 0.5 * ( scat + ( scat - 2. * upscat ) *   &
00205             (( 1. - chiv ) / 2. ) ** 2 )
00206         betao = ( 1. + zmew * extkb )   &
00207             / ( scat * zmew * extkb ) * acss
00208 
00209         !-----------------------------------------------------------------------
00210         !
00211         !     Intermediate variables identified in appendix of SE-85.
00212         !
00213         !      BE          (B)     : APPENDIX      , SE-85
00214         !      CE          (C)     : APPENDIX      , SE-85
00215         !      BOT         (SIGMA) : APPENDIX      , SE-85
00216         !      HH1         (H1)    : APPENDIX      , SE-85
00217         !      HH2         (H2)    : APPENDIX      , SE-85
00218         !      HH3         (H3)    : APPENDIX      , SE-85
00219         !      HH4         (H4)    : APPENDIX      , SE-85
00220         !      HH5         (H5)    : APPENDIX      , SE-85
00221         !      HH6         (H6)    : APPENDIX      , SE-85
00222         !      HH7         (H7)    : APPENDIX      , SE-85
00223         !      HH8         (H8)    : APPENDIX      , SE-85
00224         !      HH9         (H9)    : APPENDIX      , SE-85
00225         !      HH10        (H10)   : APPENDIX      , SE-85
00226         !      PSI         (H)     : APPENDIX      , SE-85
00227         !      ZAT         (L-T)   : APPENDIX      , SE-85
00228         !      EPSI        (S1)    : APPENDIX      , SE-85
00229         !      EK          (S2)    : APPENDIX      , SE-85
00230         !-----------------------------------------------------------------------
00231 
00232         be = 1. - scat + upscat
00233         ce = upscat
00234         bot = ( zmew * extkb ) ** 2 + ( ce**2 - be**2 )
00235 
00236         if ( abs(bot) <= 1.e-10) then
00237             scat = scat* 0.98
00238             be = 1. - scat + upscat
00239             bot = ( zmew * extkb ) ** 2 + ( ce**2 - be**2 )
00240         endif
00241 
00242         de = scat * zmew * extkb * betao
00243         fe = scat * zmew * extkb * ( 1. - betao )
00244         hh1 = -de * be + zmew * de * extkb - ce * fe
00245         hh4 = -be * fe - zmew * fe * extkb - ce * de
00246 
00247         psi = sqrt(be**2 - ce**2)/zmew
00248 
00249         zat = sib%param%zlt/sib%param%vcover*sib%diag%canex
00250 
00251         power1 = min( psi*zat, 50.0_dbl_kind )
00252         power2 = min( extkb*zat, 50.0_dbl_kind )
00253         epsi = exp( - power1 )
00254         ek = exp ( - power2 )
00255 
00256         albedo(2,iwave,1) = sib%param%soref(iwave)*(1.-sib%diag%areas)  &
00257             + ( 1.2-iwave*0.4 )*fmelt * sib%diag%areas
00258         albedo(2,iwave,2) = sib%param%soref(iwave)*(1.-sib%diag%areas)  &
00259             + ( 1.2-iwave*0.4 )*fmelt * sib%diag%areas
00260         ge = albedo(2,iwave,1)/albedo(2,iwave,2)
00261 
00262         !----------------------------------------------------------------
00263         !     CALCULATION OF DIFFUSE ALBEDOS
00264         !
00265         !     ALBEDO(1,IWAVE,2) ( I-UP ) : APPENDIX , SE-85
00266         !----------------------------------------------------------------
00267 
00268         f1 = be - ce / albedo(2,iwave,2)
00269         zp = zmew * psi
00270 
00271         den = ( be + zp ) * ( f1 - zp ) / epsi -   &
00272             ( be - zp ) * ( f1 + zp ) * epsi
00273         hh7 = ce * ( f1 - zp ) / epsi / den
00274         hh8 = -ce * ( f1 + zp ) * epsi / den
00275         f1 = be - ce * albedo(2,iwave,2)
00276         den = ( f1 + zp ) / epsi - ( f1 - zp ) * epsi
00277 
00278         hh9 = ( f1 + zp ) / epsi / den
00279         hh10 = - ( f1 - zp ) * epsi / den
00280         tranc2(iwave) = hh9 * epsi + hh10 / epsi
00281 
00282         albedo(1,iwave,2) =  hh7 + hh8
00283 
00284         !-----------------------------------------------------------------
00285         !     CALCULATION OF DIRECT ALBEDOS AND CANOPY TRANSMITTANCES.
00286         !
00287         !      ALBEDO(1,IWAVE,1) ( I-UP )   : EQUATION(11)   , SE-85
00288         !      TRANC(IWAVE)      ( I-DOWN ) : EQUATION(10)   , SE-85
00289         !
00290         !-----------------------------------------------------------------
00291         f1 = be - ce / albedo(2,iwave,2)
00292         zmk = zmew * extkb
00293 
00294         den = ( be + zp ) * ( f1 - zp ) / epsi -     &
00295             ( be - zp ) * ( f1 + zp ) * epsi
00296         hh2 = ( de - hh1 / bot * ( be + zmk ) )      &
00297             * ( f1 - zp ) / epsi -                   &
00298             ( be - zp ) * ( de - ce*ge - hh1 / bot   &
00299             * ( f1 + zmk ) ) * ek
00300         hh2 = hh2 / den
00301         hh3 = ( be + zp ) * (de - ce * ge -          &
00302             hh1 / bot * ( f1 + zmk )) * ek -         &
00303             ( de - hh1 / bot * ( be + zmk ) ) *      &
00304             ( f1 + zp ) * epsi
00305         hh3 = hh3 / den
00306         f1 = be - ce * albedo(2,iwave,2)
00307         den = ( f1 + zp ) / epsi - ( f1 - zp ) * epsi
00308         hh5 = - hh4 / bot * ( f1 + zp ) / epsi -     &
00309             ( fe + ce*ge*albedo(2,iwave,2) +         &
00310             hh4 / bot * ( zmk - f1 ) ) * ek
00311         hh5 = hh5 / den
00312         hh6 =   hh4 / bot * ( f1 - zp ) * epsi +     &
00313             ( fe + ce * ge * albedo(2,iwave,2) +     &
00314             hh4 / bot*( zmk - f1 ) ) * ek
00315         hh6 = hh6 / den
00316         tranc1(iwave) = ek
00317         tranc3(iwave) = hh4 / bot * ek + hh5 * epsi + hh6 / epsi
00318 
00319         albedo(1,iwave,1) = hh1 / bot + hh2 + hh3
00320         !
00321         !----------------------------------------------------------------------
00322         !
00323         !
00324         !----------------------------------------------------------------------
00325         !     CALCULATION OF TERMS WHICH MULTIPLY INCOMING SHORT WAVE FLUXES
00326         !     TO GIVE ABSORPTION OF RADIATION BY CANOPY AND GROUND
00327         !
00328         !      RADFAC      (F(IL,IMU,IV)) : EQUATION (19,20) , SE-86
00329         !----------------------------------------------------------------------
00330         !
00331         sib%diag%radfac(2,iwave,1) = ( 1.-sib%param%vcover )   &
00332             * ( 1.-albedo(2,iwave,1) ) + sib%param%vcover      &
00333             * ( tranc1(iwave) * ( 1.-albedo(2,iwave,1) )    &
00334             + tranc3(iwave) * ( 1.-albedo(2,iwave,2) ) )
00335 
00336         sib%diag%radfac(2,iwave,2) = ( 1.-sib%param%vcover )   &
00337             * ( 1.-albedo(2,iwave,2) ) + sib%param%vcover      &
00338             *  tranc2(iwave) * ( 1.-albedo(2,iwave,2) )
00339 
00340         sib%diag%radfac(1,iwave,1) = sib%param%vcover          &
00341             * ( ( 1.-albedo(1,iwave,1) )                    &
00342             - tranc1(iwave) * ( 1.-albedo(2,iwave,1) )      &
00343             - tranc3(iwave) * ( 1.-albedo(2,iwave,2) ) )
00344 
00345         sib%diag%radfac(1,iwave,2) = sib%param%vcover          &
00346             * ( ( 1.-albedo(1,iwave,2) )                    &
00347             - tranc2(iwave) * ( 1.-albedo(2,iwave,2) ) )
00348         !
00349         !----------------------------------------------------------------------
00350         !     CALCULATION OF TOTAL SURFACE ALBEDOS ( SALB ) WITH WEIGHTING
00351         !     FOR COVER FRACTIONS.
00352         !----------------------------------------------------------------------
00353         !
00354         do irad = 1, 2
00355             sib%diag%salb(iwave,irad) = ( 1.-sib%param%vcover )   & 
00356                 * albedo(2,iwave,irad) +                       &
00357                 sib%param%vcover * albedo(1,iwave,irad)
00358         enddo
00359         !
00360         !----------------------------------------------------------------------
00361         !
00362     enddo  ! iwave loop
00363     !
00364     !----------------------------------------------------------------------
00365     !
00366     !     CALCULATION OF LONG-WAVE FLUX TERMS FROM CANOPY AND GROUND
00367     !
00368     !----------------------------------------------------------------------
00369     !
00370     tsurf = min(tice,sib%prog%td(sib%prog%nsl+1))*sib%diag%areas   &
00371         + sib%prog%td(1)*(1.-sib%diag%areas)
00372     tc4 = sib%prog%tc*sib%prog%tc*sib%prog%tc*sib%prog%tc
00373     tg4 = tsurf*tsurf*tsurf*tsurf   
00374 
00375     zkat = 1./zmew * sib%param%zlt / sib%param%vcover
00376     zkat = min( 50.0_dbl_kind , zkat )
00377     zkat = max( 1.E-5_dbl_kind, zkat )
00378     sib%diag%thermk = exp(-zkat)
00379 
00380     sib_loc%fac1 =  sib%param%vcover * ( 1.-sib%diag%thermk )
00381 
00382 end subroutine rada2
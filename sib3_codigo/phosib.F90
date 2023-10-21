00001 
00002 
00003 !==================SUBROUTINE PHOSIB===================================
00004 subroutine phosib(sib,sib_loc)
00005 
00006     use kinds
00007     use sibtype
00008     use cfrax
00009 
00010 !itb_cos
00011     use cos
00012 
00013 
00014     use sib_const_module, only: &
00015         po2m, &
00016         pdb,  &
00017         dtt,  &
00018         dti
00019     use physical_parameters, only: &
00020         p0 => p0_sfc, &
00021         tice
00022 
00023     implicit none
00024 
00025 
00026     !----------------------------------------------------------------------
00027 
00028     type(sib_t), intent(inout) :: sib
00029 
00030     type(sib_local_vars)     ,intent(inout) :: sib_loc
00031     ! variables local to SiB
00032 
00033     !----------------------------------------------------------------------  
00034 
00035 
00036 
00037     !=======================================================================
00038     !
00039     !     CALCULATION OF CANOPY CONDUCTANCE USING THE INTEGRATED   
00040     !     MODEL RELATING ASSIMILATION AND STOMATAL CONDUCTANCE.
00041     !     UNITS ARE CONVERTED FROM MKS TO BIOLOGICAL UNITS IN THIS ROUTINE.
00042     !     BASE REFERENCE IS SE-92A
00043     !
00044     !                          UNITS
00045     !                         -------
00046     !
00047     !      PCO2M, PCO2A, PCO2Ap, PCO2I, PO2M        : PASCALS
00048     !      CO2A, CO2S, CO2I, H2OA, H2OS, H2OA       : MOL MOL-1
00049     !      VMAX0, RESPN, ASSIM, GS, GB, GA, PFD     : MOL M-2 S-1
00050     !      EFFCON                                   : MOL CO2 MOL QUANTA-1
00051     !      GCAN, 1/RB, 1/RA, 1/RST                  : M S-1
00052     !      EVAPKG                                   : KG M-2 S-1
00053     !      Q                                        : KG KG-1
00054     !
00055     !                       CONVERSIONS
00056     !                      -------------
00057     !
00058     !      1 MOL H2O           = 0.018 KG
00059     !      1 MOL CO2           = 0.044 KG
00060     !      H2O (MOL MOL-1)     = EA / PSUR ( MB MB-1 )
00061     !      H2O (MOL MOL-1)     = Q*MM/(Q*MM + 1)
00062     !pl the next line applies to the Ci to Cs pathway
00063     !      GS  (CO2)           = GS (H2O) * 1./1.6
00064     !pl 44.6 is the number of moles of air per cubic meter
00065     !      GS  (MOL M-2 S-1 )  = GS (M S-1) * 44.6*TF/T*P/PO
00066     !      PAR (MOL M-2 S-1 )  = PAR(W M-2) * 4.6*1.E-6
00067     !      MM  (MOLAIR/MOLH2O) = 1.611
00068     !
00069     !
00070     !                         OUTPUT
00071     !                      -------------
00072     !
00073     !      ASSIMN              = CANOPY NET ASSIMILATION RATE
00074     !      EA                  = CANOPY AIR SPACE VAPOR PRESSURE
00075     !      1/RST               = CANOPY CONDUCTANCE
00076     !      PCO2I               = INTERNAL CO2 CONCENTRATION
00077     !      resp_can            = CANOPY Autotrophic RESPIRATION
00078     !      resp_grnd           = GROUND RESPIRATION
00079     !
00080     !----------------------------------------------------------------------
00081     !
00082     !         RSTFAC(1) ( F(H-S) )               : EQUATION (17,18), SE-92A
00083     !         RSTFAC(2) ( F(SOIL) )              : EQUATION (12 mod), SE-89
00084     !         RSTFAC(3) ( F(TEMP) )              : EQUATION (5b)   , CO-92
00085     !         RSTFAC(4) ( F(H-S)*F(SOIL)*F(TEMP))
00086     !
00087     !-----------------------------------------------------------------------
00088 
00089 
00090     !++++++++++++++++++++++++++++++OUTPUT+++++++++++++++++++++++++++++++++++
00091     !
00092     !       ASSIMN         CARBON ASSIMILATION FLUX (MOL M-2 S-1) 
00093     !       RST            CANOPY RESISTANCE (S M-1)
00094     !       RSTFAC(4)      CANOPY RESISTANCE STRESS FACTORS 
00095     !
00096     !++++++++++++++++++++++++++DIAGNOSTICS++++++++++++++++++++++++++++++++++
00097     !
00098     !       resp_can       CANOPY Autotrophic RESPIRATION (MOL M-2 S-1)
00099     !       resp_grnd      GROUND RESPIRATION (MOL M-2 S-1)
00100     !       PCO2I          CANOPY INTERNAL CO2 CONCENTRATION (MOL MOL-1)
00101     !       GSH2O          CANOPY CONDUCTANCE (MOL M-2 S-1)
00102     !       H2OS           CANOPY SURFACE H2O CONCENTRATION (MOL MOL-1)
00103     !
00104     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
00105 
00106     !     Modifications:
00107     !       - gs (stomatal conductance reduced for freezing soils per Jim Collatz
00108     !         dd 950221      
00109     !
00110     !      Modified for multitasking - introduced gather/scatter indices
00111     !          - DD 951206
00112     !
00113     !itb   Added in pco2c (chloroplast partial co2) for neil's fractionation
00114     !itb   calculations
00115     !itb       - IB Sep99
00116     !      Kevin Schaefer resp_can/assim multiplied by aparkk when calculated (5/31/05)
00117 
00118 
00119     !Bio...LOCAL VARIABLES
00120     integer ::  icconv    !
00121 
00122     integer :: 
00123         ic,   ! iteration loop
00124         ic1,  !
00125         i      ! loop variable
00126 
00127     real(kind=dbl_kind) :: co2cap    ! conversion factor from ppm to mole/m2 in the CAS
00128     real(kind=dbl_kind) :: c3        ! C3 flag
00129     real(kind=dbl_kind) :: c4        ! C4 flag
00130     real(kind=dbl_kind) :: scatp     ! 
00131     real(kind=dbl_kind) :: scatg     !
00132     real(kind=dbl_kind) :: park      !
00133     real(kind=dbl_kind) :: qt        !
00134     real(kind=dbl_kind) :: respn     !
00135     real(kind=dbl_kind) :: vm        !
00136     real(kind=dbl_kind) :: templ     !
00137     real(kind=dbl_kind) :: temph     !
00138 !itb_frost
00139     real(kind=dbl_kind) :: tempf     ! frost stress
00140 !itb_frost
00141     real(kind=dbl_kind) :: zkc       !
00142     real(kind=dbl_kind) :: zko       !
00143     real(kind=dbl_kind) :: spfy      !
00144     real(kind=dbl_kind) :: gammas    !
00145     real(kind=dbl_kind) :: gah2o     !
00146     real(kind=dbl_kind) :: gbh2o     !
00147     real(kind=dbl_kind) :: gsh2o     !
00148     real(kind=dbl_kind) :: xgco2m    !
00149     real(kind=dbl_kind) :: rrkk      !
00150     real(kind=dbl_kind) :: soilfrz   !
00151     real(kind=dbl_kind) :: soilfrztg !
00152     real(kind=dbl_kind) :: soilfrztd !
00153     real(kind=dbl_kind) :: bintc     !
00154     real(kind=dbl_kind) :: omss      !
00155     real(kind=dbl_kind) :: range1     !
00156     real(kind=dbl_kind) :: par       !
00157     real(kind=dbl_kind) :: co2a      ! CAS CO2 concentration (mol C/mol air)
00158     real(kind=dbl_kind) :: co2m      ! reference level CO2 concentration 
00159     !   (mol C/mol air)
00160     real(kind=dbl_kind) :: co2s      ! leaf surface CO2 concentration 
00161     !   (mol C/mol air)
00162     real(kind=dbl_kind) :: pco2a     ! intermediate CAS CO2 concentration 
00163     !   (Pa)
00164     real(kind=dbl_kind) :: pco2s     ! intermediate leaf surface CO2 
00165     !   partial pressure (Pa)
00166     real(kind=dbl_kind) :: pco2i     ! intermediate stomatal (internal) 
00167     !   CO2 partial pressure (Pa)
00168     real(kind=dbl_kind) :: pco2c     ! intermediate leaf chloroplast CO2 
00169     !   partial pressure (Pa)
00170     real(kind=dbl_kind) :: h2oi      !
00171     real(kind=dbl_kind) :: h2oa      !
00172     real(kind=dbl_kind) :: h2os      !
00173     real(kind=dbl_kind) :: h2osrh    ! ratio of leaf sfc/leaf internal RH
00174     real(kind=dbl_kind) :: ecmole    !
00175     real(kind=dbl_kind) :: pco2y(6)  !
00176     real(kind=dbl_kind) :: eyy(6)    !
00177     real(kind=dbl_kind) :: assimny(6)!
00178     real(kind=dbl_kind) :: assimy(6) !
00179     real(kind=dbl_kind) :: gsh2oinf  !
00180     real(kind=dbl_kind) :: drst(5)   ! delta of stomatal resistance (sec/m)
00181     real(kind=dbl_kind) :: pco2ipot  !
00182     real(kind=dbl_kind) :: omcpot    !
00183     real(kind=dbl_kind) :: sqrtin    !
00184     real(kind=dbl_kind) :: omppot    !
00185     real(kind=dbl_kind) :: omspot    !
00186     real(kind=dbl_kind) :: omcci     !
00187     real(kind=dbl_kind) :: ompci     !
00188     real(kind=dbl_kind) :: omsci     !
00189     real(kind=dbl_kind) :: dompdomc  !
00190     real(kind=dbl_kind) :: ascitemp  !
00191     real(kind=dbl_kind) :: ccomc     !
00192     real(kind=dbl_kind) :: ccoms     !
00193     real(kind=dbl_kind) :: cwsfws    !
00194     real(kind=dbl_kind) :: cwsfht    !
00195     real(kind=dbl_kind) :: cwsflt    !
00196     real(kind=dbl_kind) :: rstfac3(6)! intermediate temperature stress factor
00197     real(kind=dbl_kind) :: zln2      ! used in calculating pdamp,qdamp
00198     real(kind=dbl_kind) :: ghalf     ! used in calculating pdamp,qdamp
00199     real(kind=dbl_kind) :: dttin     ! used in calculating pdamp,qdamp
00200     real(kind=dbl_kind) :: dmin      ! used in calculating pdamp,qdamp
00201     real(kind=dbl_kind) :: pdamp
00202     real(kind=dbl_kind) :: qdamp
00203     real(kind=dbl_kind) :: tprcor    ! temperature correction (K)
00204     real(kind=dbl_kind) :: test
00205 
00206 !itb_jab...
00207     real(kind=dbl_kind) :: gmeso     ! mesophyll conductance
00208 !itb_jab
00209 
00210 
00211 !itb...playing with carbon mass balance...
00212     real(kind=dbl_kind) :: co2a_star ! intermediate value of co2a to be used with
00213                                       ! time filter when conditions warrant
00214 
00215     real(kind=dbl_kind) :: gah2o_crit ! critical value of CAS-ref level conductance. 
00216                                        ! Conductance above this value invokes time filter.
00217     real(kind=dbl_kind) :: rstar    ! universal gas constant (N m mole^-1 K^-1)
00218 
00219     rstar = 8.3143
00220 
00221 
00222     !pl introduce a co2 capacity 
00223     !pl this will basically be the mass of air under the top of the canopy (in
00224     !pl this case (CHEAS-RAMS) O(10-30m), that is, ground to displacemnt height.
00225 
00226     !pl all the carbon fluxes are expresse as Mol C / m2 s and resistances for
00227     !pl carbon are in m2 s / mol air
00228 
00229     !pl one mole of gas occupies 22.4 cubic dm
00230     !pl 1 cubic meter contains therefore 1000./22.4  = 44.6 moles of gas
00231     !pl the units for the carbon capacity are mol air /m2. 
00232     !pl (e.g. here 893 moles if thickness of the layer is 20m)
00233     !pl this means that the units for pc02ap should be mol co2 / mol air, but
00234     !pl it is also possible to keep just co2 pressure and convert
00235 
00236     !
00237     !   calculate damping factors
00238     !
00239     zln2 = 6.9314718e-1
00240     ghalf = 1.0257068e1
00241     dttin = 3.6e3 
00242     dmin = 6.0e1
00243     pdamp = exp (-1.0 * zln2*(dtt*dmin)/(dttin*ghalf))
00244     qdamp = 1.0 - pdamp
00245     tprcor = tice*sib%prog%ps*100.0/p0
00246     co2cap = sib%diag%cas_cap_co2 * 44.6 * tprcor/sib%prog%ta  ! moles air / m2
00247     co2cap = sib%diag%cas_cap_co2 * sib%prog%ps*100.0 /rstar/sib%prog%ta
00248 
00249 
00250     !-----------------------------------------------------------------------
00251     !
00252     !     CALCULATION OF CANOPY PAR USE PARAMETER.
00253     !
00254     !      APARKK      (PI)     : EQUATION (31) , SE-92A
00255     !-----------------------------------------------------------------------
00256 
00257     scatp =     sib%param%green  *   (  sib%param%tran(1,1) +  sib%param%ref(1,1) )   &
00258    +  ( 1.- sib%param%green ) *  (  sib%param%tran(1,2) +  sib%param%ref(1,2) )
00259 
00260     scatg =  sib%param%tran(1,1) +  sib%param%ref(1,1)
00261 
00262     park = sqrt(1.-scatp) *  sib%param%gmudmu
00263 
00264     !itb...Niall integrates physfrac into aparkk. I'm not sure I like
00265     !itb...doing it that way--SO I WON'T, FOR NOW...
00266 
00267     sib%diag%aparkk   = sib%param%aparc / park * sib%param%green
00268 
00269 
00270 
00271     !itb...start PHYSIOLOGY LOOP here...
00272     !itb...potentially 5 different physiologies can share the same
00273     !itb...soil and CAS (making it different from a normal tile).
00274     !itb...loop will cycle out of an unused physiology type.
00275 
00276     !
00277     ! zero out physiology-specific values
00278     !
00279     sib%prog%rst(6)     = 0.0
00280 
00281     
00282         sib%diag%pco2c(6)   = 0.0
00283         sib%diag%pco2i(6)   = 0.0
00284         sib%diag%pco2s(6)   = 0.0
00285         sib%diag%assimn(6)  = 0.0
00286         sib%diag%assim(6)   = 0.0
00287         sib%diag%ggl(6)     = 0.0
00288         sib%diag%antemp(6)  = 0.0
00289         sib%diag%omepot(6)  = 0.0
00290         sib%diag%ansqr(6)   = 0.0
00291         sib%diag%wsfws(6)   = 0.0
00292         sib%diag%wsflt(6)   = 0.0
00293         sib%diag%wsfht(6)   = 0.0
00294         sib%diag%wci(6)     = 0.0
00295         sib%diag%whs(6)     = 0.0
00296         sib%diag%wags(6)    = 0.0
00297         sib%diag%wegs(6)    = 0.0
00298         sib%diag%assimnp(6) = 0.0
00299         sib%diag%assimci(6) = 0.0
00300         sib%diag%assimpot(6)= 0.0
00301         sib%diag%resp_can(6)= 0.0
00302         
00303         rstfac3(6)          = 0.0
00304 
00305     phys_loop : do i=1,5
00306 
00307         if ( sib%param%physfrac(i) == 0.0 ) cycle phys_loop
00308 
00309         if( sib%param%phystype(i) == 3) then
00310             c3 = 1.
00311             c4 = 0.
00312         elseif(sib%param%phystype(i) == 4) then
00313             c3 = 0.
00314             c4 = 1.
00315         else
00316             print*,'loop index=',i,' phystype=',sib%param%phystype(i)
00317             stop'ERROR:UNKNOWN PHYSIOLOGY TYPE IN PHOSIB'
00318 
00319         endif
00320 
00321         !-----------------------------------------------------------------------
00322         !
00323         !     Q-10 AND STRESS TEMPERATURE EFFECTS
00324         !
00325         !      QT          (QT)    : TABLE (2)     , SE-92A
00326         !-----------------------------------------------------------------------
00327 
00328         qt = 0.1 * ( sib%prog%tc - sib%param%trop(i) )
00329 
00330         respn = sib%param%respcp(i) * sib%param%vmax0(i) * sib%diag%rstfac(2)
00331 
00332         !itb...patch to prevent underflow if temp is too cool...
00333         if(sib%prog%tc >= sib%param%trdm(i) )then
00334             sib%diag%resp_can(i) = respn * 2.0**qt                              &
00335                 /( 1. + EXP( sib%param%trda(i)*(sib%prog%tc-sib%param%trdm(i) )))
00336         else
00337             sib%diag%resp_can(i) = respn * 2.0**qt
00338         endif
00339         sib%diag%resp_can(i)=sib%diag%resp_can(i)*sib%diag%aparkk
00340 
00341         vm = sib%param%vmax0(i) * 2.1**qt
00342 
00343         templ = 1. + EXP(sib%param%slti(i) * (sib%param%hltii(i) - sib%prog%tc))
00344 
00345         temph = 1. + EXP(sib%param%shti(i) * (sib%prog%tc - sib%param%hhti(i) ))
00346 
00347 !itb_frost...
00348         if(sib%prog%tc < sib%param%tcmin) then
00349           sib%param%tcmin = sib%prog%tc
00350         endif
00351         
00352 !itb_frost...bottom-stop tcmin at -20C
00353         sib%param%tcmin = MAX(sib%param%tcmin, 253.15_dbl_kind)
00354 
00355 !itb_frost...frost recovery at 2C/day
00356         if (sib%prog%tc > tice)    &
00357             sib%param%tcmin = sib%param%tcmin + ((4.0_dbl_kind * dtt)     &
00358                                                        /86400.0_dbl_kind)
00359 
00360         tempf = 1. + EXP(sib%param%sfti(i) * (sib%param%hfti(i) -      &
00361                                                      sib%param%tcmin))   
00362 
00363         
00364 !itb_frost...combine frost and lo-temp inhibition in templ
00365         templ = templ * tempf
00366 
00367         rstfac3(i) = 1./( templ*temph)
00368         
00369         vm    = vm/temph * sib%diag%rstfac(2)*c3 &
00370             + vm * sib%diag%rstfac(2)*rstfac3(i) * c4
00371 
00372         !-----------------------------------------------------------------------
00373         !
00374         !     MICHAELIS-MENTEN CONSTANTS FOR CO2 AND O2, CO2/O2 SPECIFICITY,
00375         !     COMPENSATION POINT       
00376         !
00377         !      ZKC          (KC)     : TABLE (2)     , SE-92A
00378         !      ZKO          (KO)     : TABLE (2)     , SE-92A
00379         !      SPFY         (S)      : TABLE (2)     , SE-92A
00380         !      GAMMAS       (GAMMA-*): TABLE (2)     , SE-92A
00381         !      OMSS         (OMEGA-S): EQUATION (13) , SE-92A
00382         !      BINTC        (B*ZLT)  : EQUATION (35) , SE-92A
00383         !-----------------------------------------------------------------------
00384 
00385         zkc     = 30. * 2.1**qt
00386         zko     = 30000. * 1.2**qt
00387         spfy    = 2600. * 0.57**qt
00388         gammas  = 0.5 * po2m/spfy * c3
00389         !itb...check for underflow...
00390         
00391         if(sib%prog%radvbc < 1.E-6) then
00392             sib%diag%pfd=0.0
00393         else
00394             sib%diag%pfd = 4.6E-6 * sib%param%gmudmu * &
00395                 ( sib%prog%radvbc + sib%prog%radvdc )
00396         endif
00397 
00398 
00399         !...convert resistance to conductance  to  mol/ (m2 sec)
00400         !...(44.6 mol m^-3 conversion factor)
00401         if ( sib%prog%rst(i) == 0. ) sib%prog%rst(i) = sib%prog%rst(1)
00402         gsh2o  = 1.0/sib%prog%rst(i) * 44.032476*tprcor/sib%prog%tc
00403         gbh2o  = 0.5/sib%diag%rb     * 44.032476*tprcor/sib%prog%tc
00404         gah2o  = 1.0/sib%diag%ra     * 44.032476*tprcor/sib%prog%tm
00405 
00406 !Itb_jab
00407         if(sib%param%biome == 4. .OR. sib%param%biome == 5.) then
00408           gmeso = 2000.0_dbl_kind
00409         else
00410           gmeso = 4000.0_dbl_kind
00411         endif
00412 !itb_jab
00413         Xgco2m = gmeso * sib%param%vmax0(i) * sib%diag%aparkk * sib%diag%rstfac(2)
00414 
00415         rrkk   = zkc*( 1. + po2m/zko ) * c3 &
00416             + sib%param%vmax0(i)/5.* ( 1.8**qt) * c4
00417 
00418         par    = sib%diag%pfd * sib%param%effcon(i) * ( 1.-scatg )
00419 
00420         soilfrztg = 1.+exp(-1.5 * &
00421             (max(270.0_dbl_kind,sib%prog%td(1))-273.16))
00422         soilfrztd = 1.+exp(-1.5 * &
00423             (max(270.0_dbl_kind,sib%prog%td(2))-273.16))
00424         soilfrz   = max(1./soilfrztg, 1./soilfrztd)
00425         soilfrz   = max( soilfrz, 0.05_dbl_kind)
00426 
00427         bintc  = sib%param%binter(i) * sib%param%zlt * sib%param%green * &
00428             sib%diag%rstfac(2) * soilfrz
00429 
00430         omss   = ( sib%param%vmax0(i) / 2.0 ) * ( 1.8**qt ) &
00431             /templ * sib%diag%rstfac(2) * c3 &
00432             + rrkk * sib%diag%rstfac(2) * c4
00433 
00434         !-----------------------------------------------------------------------
00435         !
00436         !     FIRST GUESS IS MIDWAY BETWEEN COMPENSATION POINT AND MAXIMUM
00437         !     ASSIMILATION RATE.
00438         !
00439         !-----------------------------------------------------------------------
00440 
00441 
00442         range1  = sib%prog%pco2m * ( 1. - 1.6/sib%param%gradm(i) ) - gammas
00443         icconv = 1
00444 
00445         do ic = 1, 6
00446             pco2y(ic) = 0.
00447             eyy(ic)   = 0.
00448         enddo
00449 
00450         !Bio...HERE IS THE ITERATION LOOP.
00451         !Bio...
00452         !Bio...We iterate on PCO2C-sortin makes a 'first guess' at
00453         !Bio...then orders PCO2C/Error pairs on increasing error size,
00454         !Bio...then uses a combination of linear/quadratic fit to obtain 
00455         !Bio...the 'next best guess' as iteration count increases.
00456         !Bio...CYCALC uses that value of PCO2C to get the next value 
00457         !Bio...of ASSIMN. CO2A and CO2S follow.
00458 
00459         do ic = 1, 6
00460 
00461             call sortin( eyy, pco2y, range1, gammas, ic)
00462 
00463             call cycalc( sib%diag%aparkk, VM, sib%param%atheta(i), &
00464                 sib%param%btheta(i),par,gammas, sib%diag%resp_can(i),    &
00465                 rrkk, omss, c3, c4,pco2y(ic), assimny(ic),      &
00466                 assimy(ic))
00467 
00468 
00469 
00470             !pl now prognose the new CAS CO2 according to flux divergence
00471             !pl we are going to do this in mol C / mol air (same as PaC/PaAir)
00472 
00473             co2a    = sib%prog%pco2ap /   (sib%prog%ps*100.)
00474             co2m    = sib%prog%pco2m  /   (sib%prog%ps*100.)   
00475 
00476             co2a   = (  co2a + (dtt/co2cap) *  & 
00477                 (sib%diag%resp_grnd - assimny(ic)  &
00478                 +co2m*gah2o        ) )         &
00479                 / (1+dtt*gah2o/ co2cap ) 
00480 
00481 
00482             pco2a = co2a * sib%prog%ps * 100.
00483 
00484             !itb...intermediate leaf surface CO2
00485             pco2s = pco2a - (1.4/gbh2o * assimny(ic) &
00486                 * sib%prog%ps*100.)
00487 
00488             !itb...intermediate leaf internal CO2
00489             pco2i = pco2s - assimny(ic) * sib%prog%ps * 100.0 * 1.6/gsh2o
00490 
00491             !itb...intermediate leaf chloroplast CO2-this is what we iterate on 
00492             pco2c = pco2i - c3 * assimny(ic) * sib%prog%ps * 100.0 * 1.0/xgco2m
00493 
00494             Eyy(ic) = pco2y(ic) - pco2c
00495 
00496             if(ic.ge.2) then
00497                 ic1 = ic-1
00498                 if(abs(eyy(ic1)).ge.0.1)then
00499                     icconv = ic
00500                 else
00501                     eyy(ic) = eyy(ic1)
00502                     pco2y(ic) = pco2y(ic1)
00503                 endif
00504             endif
00505 
00506         enddo !iteration loop
00507 
00508 
00509         !itb...have iterated for physiology-specific values, now save them
00510 
00511         sib%diag%pco2c(i)  = pco2y(icconv)
00512         sib%diag%assimn(i) = assimny(icconv)
00513         sib%diag%assim(i)  = assimy(icconv)
00514         sib%diag%pco2i(i)  = sib%diag%pco2c(i) +  &
00515             c3 * sib%diag%assimn(i)/xgco2m*sib%prog%ps*100.0
00516         sib%diag%pco2s(i)  = sib%diag%pco2i(i) +  &
00517             sib%diag%assimn(i)/gsh2o *sib%prog%ps*100.0
00518 
00519         !
00520         !  update stomatal resistance...
00521         !
00522         h2oi   = sib_loc%etc / sib%prog%ps
00523         h2oa   =  sib%prog%ea / sib%prog%ps
00524 
00525         ecmole = 55.56 * sib%diag%ecmass * dti 
00526 
00527         h2os = h2oa + ecmole / gbh2o
00528         h2os  = min( h2os, h2oi )
00529         h2os  = max( h2os, 1.0e-7_dbl_kind)
00530         h2osrh = h2os / h2oi
00531 
00532 !itb...soft landing: add curvature to h2osrh at low RH to 
00533 !itb...lessen positive feedback
00534  
00535         if(h2osrh < 0.6_dbl_kind) h2osrh = h2osrh +    &
00536                                   (0.6_dbl_kind - h2osrh) ** 2.5
00537 
00538         sib%diag%rstfac(1) = h2osrh
00539 
00540 
00541 
00542 
00543         !Bio relaxed this condition to 1/10 of previous (.05 vs .5). 
00544         !Bio The old way made
00545         !Bio the CO2 on top of the leaves always at least 1/2 of the value at the
00546         !Bio reference level.
00547         co2s = MAX(sib%diag%pco2s(i),sib%prog%pco2m*0.05) / (sib%prog%ps*100.)
00548 
00549         !Bio Ball-Berry equation right here !     
00550         gsh2oinf = (sib%param%gradm(i) *  &
00551             MAX(1.0e-12_dbl_kind,sib%diag%assimn(i))  &
00552             * h2osrh * soilfrz / co2s) + bintc
00553 
00554         !Bio this is the change in stomatal resistance
00555         !itb...this has been brought here from ADDINC 
00556 
00557         drst(i) = sib%prog%rst(i) * qdamp * ((gsh2o-gsh2oinf)/  &
00558             (pdamp*gsh2o+qdamp*gsh2oinf))
00559 
00560         bintc = bintc * sib%prog%tc / ( 44.032476 * tprcor)
00561 
00562         sib%prog%rst(i) = sib%prog%rst(i) + drst(i)
00563 
00564         ! bintc(i)- smallest canopy stomatal conductance needs to be passed in here.
00565         ! ---- c.zhang, 2/3/93
00566 
00567         sib%prog%rst(i)=MIN( 1./bintc, sib%prog%rst(i) )
00568 
00569         !...leaf conductance...
00570         sib%diag%ggl(i) = 1.0 / (sib%prog%rst(i)* sib%diag%rc)
00571 
00572 
00573         !-----------------------------------------------------------------------
00574         ! CALCULATION OF POTENTIAL ASSIMILATION
00575         !-----------------------------------------------------------------------
00576 
00577         ! Make assimn a top leaf, not the canopy.
00578         sib%diag%assimnp(i) = sib%diag%assimn(i) / sib%diag%aparkk
00579 
00580         ! Bottom stopped assim.
00581         sib%diag%antemp(i) = MAX(0.0_dbl_kind,sib%diag%assimnp(i))
00582 
00583         ! Potential intercellular co2.
00584         pco2ipot = sib%prog%ps*100.* (co2s-(1.6 * sib%diag%assimnp(i)/   &
00585             ((sib%param%gradm(i) * sib%diag%antemp(i) / co2s) + bintc)))
00586 
00587         ! Potential rubisco limitation.
00588         omcpot = sib%param%vmax0(i)*2.1**qt*((pco2ipot-gammas)/             &
00589             (pco2ipot+rrkk)*c3 + c4)
00590 
00591         ! Potential light limitation.
00592         sib%diag%omepot(i) = par*((pco2ipot-gammas)/                     &
00593             (pco2ipot+2.*gammas)*c3 + c4)
00594 
00595         ! Quad 1.
00596         sqrtin = MAX(0.0_dbl_kind,((sib%diag%omepot(i) + omcpot)**2-     &
00597             4.* sib%param%atheta(i) * sib%diag%omepot(i) * omcpot))
00598 
00599         ! Quad 1. Intermediate  top leaf photosynthesis.
00600         omppot = ((sib%diag%omepot(i)+omcpot)-SQRT(sqrtin))/  &
00601             (2.*sib%param%atheta(i) )
00602 
00603         ! Potential sink or pep limitation.
00604         omspot = (sib%param%vmax0(i) / 2.0)*(1.8**qt)*c3  &
00605             + rrkk*pco2ipot*c4
00606 
00607         ! Quad 2.
00608         sqrtin=MAX(0.0_dbl_kind,((omppot+omspot)**2-4.*sib%param%btheta(i)*   &
00609             omppot*omspot))
00610 
00611         ! Quad 2. Final Potential top leaf photosynthesis.
00612         if ( omppot < 1.0e-14 ) omppot = 0.0_dbl_kind
00613 
00614         sib%diag%assimpot(i) = ((omspot + omppot)-SQRT(sqrtin))/           &
00615             (2. * sib%param%btheta(i))
00616         !-----------------------------------------------------------------------
00617         ! CALCULATION OF STRESS FACTOR LIMITED ASSIMILATION
00618         !-----------------------------------------------------------------------
00619 
00620         ! Stressed rubisco limitation.
00621         omcci = vm*((pco2ipot-gammas)/(pco2ipot+rrkk)*c3+ c4)
00622 
00623         ! Quad 1.
00624         sqrtin = MAX(0.0_dbl_kind,(sib%diag%omepot(i) + omcci) **2 -       &
00625             4.*sib%param%atheta(i) * sib%diag%omepot(i) * omcci)
00626 
00627         ! Quad 1. Intermediate stress limited top leaf photosynthesis.
00628         ompci = ((sib%diag%omepot(i) + omcci) - SQRT(sqrtin))              &
00629             /(2.*sib%param%atheta(i))
00630 
00631         ! Stressed sink or pep limitation.
00632         omsci = omss*(c3 + pco2ipot*c4)
00633 
00634         ! Quad 2.
00635         sqrtin = MAX(0.0_dbl_kind,(ompci + omsci)**2-4. * sib%param%btheta(i)  & 
00636             * ompci * omsci)
00637 
00638         ! Quad 2. Final stress limited top leaf photosynthesis.
00639         sib%diag%assimci(i) = ((omsci+ompci)-SQRT(sqrtin))/(2.*sib%param%btheta(i))
00640 
00641         !-----------------------------------------------------------------------
00642         ! CALCULATION OF CONTROL COEFFICIENTS
00643         !-----------------------------------------------------------------------
00644 
00645         ! Intermediate.
00646         dompdomc = (ompci-sib%diag%omepot(i) )/                            &
00647             (2.*sib%param%atheta(i)*ompci-omcci-sib%diag%omepot(i))
00648 
00649         ! Bottom stopped final stress limited top leaf photosynthesis.
00650         ascitemp = MAX(sib%diag%assimci(i),1.0e-12_dbl_kind)
00651 
00652         ! Rubisco control coefficient.
00653         ccomc = (dompdomc*(sib%diag%assimci(i)-omsci)/  &
00654             (2.*sib%param%btheta(i)*sib%diag%assimci(i)-ompci-omsci))*  &
00655             omcci/ascitemp
00656 
00657         ! Sink or pep control coefficient.
00658         ccoms = ((sib%diag%assimci(i)-ompci)/  &
00659             (2.*sib%param%btheta(i)*sib%diag%assimci(i)-ompci-omsci))*  &
00660             omsci/ascitemp
00661 
00662         !-----------------------------------------------------------------------
00663         !  OUTPUT:  POTENTIAL ASSIMILATION RATES TO BE SUMMED
00664         !-----------------------------------------------------------------------
00665         ! Canopy values (overwrites top leaf).
00666 
00667         sib%diag%omepot(i)   = sib%diag%omepot(i)   * sib%diag%aparkk
00668         sib%diag%assimpot(i) = sib%diag%assimpot(i) * sib%diag%aparkk
00669         sib%diag%assimci(i)  = sib%diag%assimci(i)  * sib%diag%aparkk
00670         sib%diag%antemp(i)   = sib%diag%antemp(i)   * sib%diag%aparkk
00671         sib%diag%ansqr(i)    = sib%diag%antemp(i)   * sib%diag%antemp(i)
00672         sib%diag%assimnp(i)  = sib%diag%assimnp(i)  * sib%diag%aparkk
00673 
00674         !-----------------------------------------------------------------------
00675         ! OUTPUT:  WEIGHTED STRESS FACTORS AND OTHER DIAGNOSTIC OUTPUTS TO BE SUMMED
00676         !-----------------------------------------------------------------------
00677 
00678         ! Water stress.
00679         sib%diag%wsfws(i) = sib%diag%assimpot(i)*(1.-sib%diag%rstfac(2))*  &
00680             (ccomc+ccoms)
00681 
00682         ! High temperature stress.
00683         sib%diag%wsfht(i) = sib%diag%assimpot(i)*(1.-1./temph)*ccomc
00684 
00685         ! Low temperature stress.
00686         sib%diag%wsflt(i) = sib%diag%assimpot(i)*(1.-1./templ)*  &
00687             (ccoms*c3+ccomc*c4)
00688 
00689         !  protection for wsfws, wsfht, and wsflt from <0 or >>xxx(2/24/93)
00690         cwsfws = (1.-sib%diag%rstfac(2))*(ccomc+ccoms)
00691         if(cwsfws .gt.1. .or. cwsfws .lt. 0.) sib%diag%wsfws(i)=0.
00692 
00693         cwsfht = (1.-1./temph)*ccomc
00694         if(cwsfht > 1.0_dbl_kind .or. cwsfht < 0.0_dbl_kind)  &
00695             sib%diag%wsfht(i)=0.
00696 
00697         cwsflt = (1.-1./templ)*(ccoms*c3+ccomc*c4)
00698         if(cwsflt > 1.0_dbl_kind .or. cwsflt < 0.0_dbl_kind)  &
00699             sib%diag%wsflt(i)=0.
00700 
00701 
00702         ! Intermediate assimilation weighted Ci.
00703         sib%diag%wci(i) = sib%diag%antemp(i) * sib%diag%pco2i(i)
00704 
00705         ! Intermediate assimilation weighted relative humidty stress factor.
00706         sib%diag%whs(i) = sib%diag%antemp(i) * sib%diag%rstfac(1)
00707 
00708         ! Intermediate assimilation weighted stomatal conductance.
00709         sib%diag%wags(i) = gsh2o * sib%diag%antemp(i)
00710 
00711         ! Intermediate evaporation weighted stomatal conductance.(Step 1.
00712         !   Step 2 after subroutine update)
00713         sib%diag%wegs(i) = gsh2o
00714 
00715         !
00716         !itb...determine the weighted mean value for the gridcell
00717         !
00718         sib%diag%pco2c(6)   = sib%diag%pco2c(6)  + sib%diag%pco2c(i)  *  &
00719             sib%param%physfrac(i)
00720         sib%diag%pco2i(6)   = sib%diag%pco2i(6)  + sib%diag%pco2i(i)  *  &
00721             sib%param%physfrac(i)
00722         sib%diag%pco2s(6)   = sib%diag%pco2s(6)  + sib%diag%pco2s(i)  *  &
00723             sib%param%physfrac(i)
00724         sib%diag%assimn(6)  = sib%diag%assimn(6) + sib%diag%assimn(i) *  &
00725             sib%param%physfrac(i)
00726         sib%diag%assim(6)   = sib%diag%assim(6)  + sib%diag%assim(i)  *  &
00727             sib%param%physfrac(i)
00728         rstfac3(6)     = rstfac3(6)    + rstfac3(i)    * sib%param%physfrac(i) 
00729         sib%prog%rst(6)     = sib%prog%rst(6)    + sib%prog%rst(i)    *  &
00730             sib%param%physfrac(i)
00731         sib%diag%ggl(6)     = sib%diag%ggl(6)    + sib%diag%ggl(i)    *  &
00732             sib%param%physfrac(i)
00733         sib%diag%antemp(6)  = sib%diag%antemp(6) + sib%diag%antemp(i) *  &
00734             sib%param%physfrac(i)
00735         sib%diag%omepot(6)  = sib%diag%omepot(6) + sib%diag%omepot(i) *  &
00736             sib%param%physfrac(i)
00737         sib%diag%ansqr(6)   = sib%diag%ansqr(6)  + sib%diag%ansqr(i)  *  &
00738             sib%param%physfrac(i)
00739         sib%diag%wsfws(6)   = sib%diag%wsfws(6)  + sib%diag%wsfws(i)  *  &
00740             sib%param%physfrac(i)
00741         sib%diag%wsflt(6)   = sib%diag%wsflt(6)  + sib%diag%wsflt(i)  *  &
00742             sib%param%physfrac(i)
00743         sib%diag%wsfht(6)   = sib%diag%wsfht(6)  + sib%diag%wsfht(i)  *  &
00744             sib%param%physfrac(i)
00745         sib%diag%wci(6)     = sib%diag%wci(6)    + sib%diag%wci(i)    *  &
00746             sib%param%physfrac(i)
00747         sib%diag%whs(6)     = sib%diag%whs(6)    + sib%diag%whs(i)    *  &
00748             sib%param%physfrac(i)
00749         sib%diag%wags(6)    = sib%diag%wags(6)   + sib%diag%wags(i)   *  &
00750             sib%param%physfrac(i)
00751         sib%diag%wegs(6)    = sib%diag%wegs(6)   + sib%diag%wegs(i)   *  &
00752             sib%param%physfrac(i)
00753         sib%diag%assimnp(6) = sib%diag%assimnp(6)+ sib%diag%assimnp(i)*  &
00754             sib%param%physfrac(i)
00755         sib%diag%assimci(6) = sib%diag%assimci(6)+ sib%diag%assimci(i)*  &
00756             sib%param%physfrac(i)
00757         sib%diag%assimpot(6)= sib%diag%assimpot(6)+sib%diag%assimpot(i)* &
00758             sib%param%physfrac(i)
00759         sib%diag%resp_can(6)= sib%diag%resp_can(6)+ sib%diag%resp_can(i)*  &
00760             sib%param%physfrac(i)
00761 
00762         !...CFRAX...
00763         !...at the end of each physiology loop, call the CFRAX code
00764 
00765         call cfrax_physloop(sib,i,c3)
00766 
00767         !...CFRAX...
00768 
00769     enddo phys_loop   ! PHYSIOLOGY LOOP
00770 
00771     !pl now do the real C_A forecast with the iterated fluxes.
00772 
00773     co2a    = sib%prog%pco2ap /   (sib%prog%ps*100.)
00774     co2m    = sib%prog%pco2m  /   (sib%prog%ps*100.) 
00775 
00776     !itb...carbon flux between CAS and reference level (mol C m^-2 sec^-1)
00777     sib%diag%cflux = gah2o*(co2a-co2m)
00778 
00779     sib%prog%expand=sib%prog%pco2ap*sib%diag%cas_cap_co2/rstar/sib%prog%ta- sib%prog%cas_old
00780   
00781 ! original semi-implicit time differencing
00782     co2a = (co2a + (dtt/co2cap) * (sib%diag%resp_grnd - sib%diag%assimn(6)    & 
00783         +co2m*gah2o ) ) / (1+dtt*gah2o/co2cap)
00784     sib%prog%pco2ap = co2a * sib%prog%ps * 100.
00785 !
00786 ! basic forward time differencing
00787 !    sib%prog%pco2ap=sib%prog%pco2ap+(sib%diag%resp_grnd - sib%diag%assimn(6)  &
00788 !    -sib%diag%cflux)*dtt*rstar*sib%prog%ta/sib%diag%cas_cap_co2
00789 
00790 ! semi-implicit time differencing with cflux damping
00791 !    if (sib%diag%cflux*dtt > 0.01*sib%prog%cas_old) then 
00792 
00793 !      co2a_star = (co2a + (dtt/co2cap) * (sib%diag%resp_grnd - sib%diag%assimn(6)    & 
00794 !        +co2m*gah2o ) ) / (1+dtt*gah2o/co2cap)
00795 
00796 !      co2a = 0.4*co2a_star + 0.6* co2a
00797 !      sib%prog%pco2ap = co2a * sib%prog%ps * 100.
00798 !      print*, sib%prog%pco2ap,sib%prog%pco2m
00799 !    else
00800 !      co2a = (co2a + (dtt/co2cap) * (sib%diag%resp_grnd - sib%diag%assimn(6)    & 
00801 !        +co2m*gah2o ) ) / (1+dtt*gah2o/co2cap)
00802 !      sib%prog%pco2ap = co2a * sib%prog%ps * 100.
00803 !    endif
00804     sib%diag%cflux = gah2o*(co2a-co2m)
00805 
00806 !
00807 ! moles per m2 CO2 in canopy air space
00808     sib%prog%cas=sib%prog%pco2ap*sib%diag%cas_cap_co2/rstar/sib%prog%ta
00809 
00810     sib%diag%rstfac(3) = rstfac3(6)
00811     sib%diag%rstfac(4) = sib%diag%rstfac(1) * sib%diag%rstfac(2) *  &
00812         sib%diag%rstfac(3)
00813 
00814     !...CFRAX...
00815     !...one last call to calculate canopy-mean discrimination values
00816 
00817     call cfrax_final(sib)
00818 
00819     !...CFRAX...
00820 
00821 !itb_cos...everything done; now calculate COS flux/concentration
00822 
00823         !itb...recalculate gsh2o for passage into OCS subroutine
00824         gsh2o  = 1.0/sib%prog%rst(i) * 44.032476*tprcor/sib%prog%tc
00825 
00826    call cos_calc(sib,co2cap,gah2o,gbh2o,gsh2o,co2m,co2a,xgco2m,qt,gmeso)
00827 end subroutine phosib
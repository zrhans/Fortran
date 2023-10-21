00001 
00002 module cfrax
00003 
00004     !  CFRAX calculates 13C and 12C fluxes and concentrations in the canopy,
00005     !  mixed layer, carbon assimilation (photosynthate), respired soil carbon,
00006     !  assuming that discrimination against 13C during photosynthesis is a 
00007     !  function of discrimination during diffusion and the pCO2i/pCO2c ratio.
00008     !  C4 discrimination against 13C only results from diffusion. 
00009 
00010 
00011     !itb...modified 01 Oct 99 to try to settle some mass-balance problems
00012     !itb...we've been having - new implicit eqn's for canopy C12 and C13.
00013 
00014     use kinds
00015     use sibtype
00016 
00017     use sib_const_module, only: &
00018         pdb,       & 
00019         kieclfbl,  &
00020         kiecstom,  &
00021         kieclphas, &
00022         kiecdis,   &
00023         kiecrbsco, &
00024         tref,      &
00025         pref,      &
00026         dtt
00027 
00028 
00029 
00030     implicit none
00031 
00032 
00033     !...LOCAL VARIABLES...
00034     real(kind=dbl_kind) :: co2a_conc   ! CAS CO2 concentration (mol m^-3)
00035     real(kind=dbl_kind) :: co2m_conc   ! ref lev CO2 concentration (mol m^-3)
00036     real(kind=dbl_kind) :: rca         ! C13/C12 ratio of canopy CO2 
00037     real(kind=dbl_kind) :: rcm         ! C13/C12 ratio of ref level CO2
00038     real(kind=dbl_kind) :: c13cm       ! concentration of C13 in mixed 
00039     !  layer CO2
00040     real(kind=dbl_kind) :: c12cm       ! concentration of C12 in mixed 
00041     !  layer CO2 
00042     real(kind=dbl_kind) :: rcresp      ! C13/C12 ratio of respiratory CO2
00043     real(kind=dbl_kind) :: c13resp     ! flux of C13 in respiratory CO2
00044     !    (mol m^-2 sec^-1)
00045     real(kind=dbl_kind) :: c12resp     ! flux of C12 in respiratory CO2
00046     !    (mol m^-2 sec^-1)
00047     real(kind=dbl_kind) :: c13ca       ! concentration of C13 in canopy CO2 
00048     real(kind=dbl_kind) :: c12ca       ! concentration of C12 in canopy CO2
00049 
00050 !itb_iso 
00051     real(kind=dbl_kind) :: d13c_gresp  ! del 13C of combined autotrophic and 
00052                                        ! heterotrophic resp
00053 !itb_iso  
00054     
00055 
00056     contains
00057 
00058     subroutine cfrax_physloop(sib,i,c3)
00059 
00060         use kinds
00061         use sibtype
00062 
00063         implicit none
00064 
00065         !----------------------------------------------------------------------
00066 
00067         type(sib_t), intent(inout) :: sib
00068 
00069         !----------------------------------------------------------------------  
00070 
00071         !...INPUT VARIABLES
00072         integer(kind=int_kind),intent(in) :: i        ! phystype loop index
00073         real(kind=dbl_kind),intent(in)    :: c3       ! C3 flag
00074 
00075         !...convert canopy and mixed layer CO2 pressures from Pa to moles/m^3 
00076         !...uses PV = nRT; at STP and 44.6 moles of gas per m3.  
00077 
00078 
00079 !        co2a_conc = sib%prog%pco2ap * (tref/sib%prog%ta)*(44.6/pref)
00080 !        co2m_conc = sib%prog%pco2m  * (tref/sib%prog%ta)*(44.6/pref)
00081 !itb_iso: modified, per John Miller's suggestion
00082         co2a_conc = sib%prog%pco2ap / (sib%prog%ta * 8.314)
00083         co2m_conc = sib%prog%pco2m  / (sib%prog%ta * 8.314)
00084 
00085 
00086         !...d13Cca and d13Cm are converted to concentrations (moles/m3) of 13C 
00087         !...and 12C by first calculating isotope ratios (13C/12C) of the canopy     
00088         !...(Ca) and mixed layer (m). 
00089 
00090         rca   = ((sib%prog%d13cca * pdb) / 1000.) + pdb
00091 
00092         c13ca = (rca * co2a_conc) / (1. + rca)
00093         c12ca = co2a_conc / (1. + rca)
00094 
00095         rcm   = ((sib%prog%d13cm * pdb) / 1000.) + pdb
00096 
00097         c13cm = (rcm * co2m_conc) / (1. + rcm)
00098         c12cm = co2m_conc / (1. + rcm)
00099 
00100 
00101         !...13c and 12c fluxes (moles/m2/sec) arising from respiration are        
00102         !...calculated using conversions between delta notation and epsilon 
00103         !...notation and 13C/12C ratios.
00104 
00105 !itb_iso 
00106         d13c_gresp = ( sib%param%d13c_het * sib%diag%resp_het ) / sib%diag%resp_grnd    +     &
00107                     ( sib%param%d13c_auto(i) * sib%diag%resp_auto ) / sib%diag%resp_grnd
00108 
00109         Sib%param%d13cresp = d13c_gresp
00110 !print*,'del13c_resp=',d13c_gresp
00111 
00112 
00113 !itb_iso 
00114 
00115 
00116         rcresp  = ((d13c_gresp * pdb) / 1000.) + pdb
00117         c13resp = (rcresp * sib%diag%resp_grnd) / (1. + rcresp)
00118         c12resp = sib%diag%resp_grnd / (1. + rcresp)
00119 
00120         ! C13/C12 discrimination for C3 plants.  The isotope effect during C3
00121         ! photosynthesis is a function of a combination of the isotope effects
00122         ! associated with molecular transport of CO2 across the leaf boundary
00123         ! layer (lfbl), into the stoma (stom), dissolution to in mesophyll H2O
00124         ! (dis), and transport in the liquid phase (lphas).  The isotope effect 
00125         ! during C4 photosynthesis is only a function (for now) of transport into
00126         ! the stoma.    note: IECpsC3 is the isotope effect for carbon isotopic  
00127         ! discrimination during photosynthesis of C3 plants.  Similarly for IECpsC4,
00128         ! but for C4 plants. 
00129 
00130 
00131         if(sib%diag%assimn(i) > 0.0) then
00132 
00133             if(c3 == 1.0) then
00134             
00135                 sib%diag%kiecps(i) = ( kieclfbl * sib%prog%pco2ap + &
00136                     (kiecstom-kieclfbl) * sib%diag%pco2s(i) + &
00137                     (kiecdis + kieclphas - kiecstom) * sib%diag%pco2i(i) + &
00138                     (kiecrbsco-kiecdis-kieclphas) * sib%diag%pco2c(i) ) &
00139                     / sib%prog%pco2ap
00140 
00141             else        !C4 plants given constant KIE = 4.4per mil  
00142             
00143                 sib%diag%kiecps(i) = kiecstom
00144                  
00145             endif
00146 
00147             sib%diag%rcassimn(i)   =   rca / ((-sib%diag%kiecps(i) / 1000.) + 1.)
00148             sib%diag%d13cassimn(i) =  ((sib%diag%rcassimn(i) - pdb) / pdb ) * 1000.
00149 
00150         else 
00151 
00152             ! We need values for when Assimn < 0.0, i.e. nighttime.  We revert
00153             ! to the del13C of respiration both for convenience and because the
00154             ! nighttime flux to the atmosphere reflects the 13C/12C ratio of
00155             ! respiration
00156 
00157                 sib%diag%rcassimn(i)   =  ((sib%diag%d13cassimn(i) * pdb) / 1000.0) + pdb
00158                 sib%diag%d13cassimn(i) =  sib%param%d13c_auto(i)
00159 
00160 
00161         endif
00162 
00163 
00164         ! calculates d13C of carbon and fluxes of 13C and 12C assimilated 
00165         ! by phystype i (moles/m2/sec)
00166 
00167 
00168 
00169         sib%diag%c13assimn(i)  = ( sib%diag%rcassimn(i) * sib%diag%assimn(i))/(1. + sib%diag%rcassimn(i))
00170 
00171         sib%diag%c12assimn(i)  =  sib%diag%assimn(i) / (1.+ sib%diag%rcassimn(i))
00172 
00173 
00174 !itb_iso...accumulate del 13C of photosynthesized material: used for calculating
00175 !itb_iso...del 13C of autotrophic respiration
00176 
00177         sib%param%d13c_psn(i) = sib%param%d13c_psn(i) +     &
00178                    sib%diag%assimn(i) * dtt * sib%diag%d13cassimn(i)
00179 
00180         sib%param%psn_accum(i)  = sib%param%psn_accum(i)  +     &
00181                    sib%diag%assimn(i) * dtt
00182 
00183         sib%param%d13c_psn(6) = sib%param%d13c_psn(6) +     &
00184                    sib%diag%assimn(i) * dtt * sib%diag%d13cassimn(i)
00185 
00186         sib%param%psn_accum(6)  = sib%param%psn_accum(6)  +     &
00187                    sib%diag%assimn(i) * dtt
00188 !itb_iso 
00189 
00190 
00191 
00192 
00193     End subroutine cfrax_physloop
00194 
00195 
00196 
00197 
00198     !ITB...this is the 'final' cfrax stuff to be done after the physiology 
00199     !ITB...loops are finished.
00200 
00201     subroutine cfrax_final(sib)
00202 
00203         use kinds
00204         use sibtype
00205 
00206         implicit none
00207         !----------------------------------------------------------------------
00208 
00209         type(sib_t), intent(inout) :: sib
00210 
00211         !----------------------------------------------------------------------  
00212 
00213 
00214         !...LOCAL VARIABLES...
00215         integer(kind=int_kind) :: i
00216 
00217         !...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX
00218 
00219         sib%diag%c13assimn(6) = 0.0
00220         sib%diag%c12assimn(6) = 0.0
00221 
00222         phys_loop: do i=1,5
00223 
00224 !            print*,'physfrac:',i,sib%param%physfrac(i)
00225             if(sib%param%physfrac(i) == 0.0) cycle phys_loop
00226              
00227             sib%diag%c13assimn(6) = sib%diag%c13assimn(6) + &
00228                 sib%diag%c13assimn(i) * sib%param%physfrac(i)
00229 
00230             sib%diag%c12assimn(6) = sib%diag%c12assimn(6) + &
00231                 sib%diag%c12assimn(i) * sib%param%physfrac(i)
00232 
00233         enddo phys_loop
00234 
00235 
00236         sib%diag%rcassimn(6) = sib%diag%c13assimn(6)/   &
00237                                                 sib%diag%c12assimn(6)
00238  
00239 
00240         sib%diag%d13cassimn(6) =  ((sib%diag%rcassimn(6)/  &
00241                                    pdb) - 1.0_dbl_kind)*1000.0_dbl_kind
00242 
00243 
00244         !  Canopy concentrations at time n+1 is calculated using an implicit 
00245         !  scheme.
00246 
00247 
00248 
00249         c13ca = (c13ca + (dtt / sib%param%z2) * (c13resp - &
00250             sib%diag%c13assimn(6) + (c13cm / sib%diag%ra ))) &
00251             / (1. + (dtt / sib%diag%ra ) / sib%param%z2)
00252 
00253         c12ca = (c12ca + (dtt / sib%param%z2) * (c12resp - &
00254             sib%diag%c12assimn(6) + (c12cm / sib%diag%ra ))) &
00255             / (1. + (dtt / sib%diag%ra ) / sib%param%z2)
00256 
00257 
00258 
00259         !  Del13c of canopy is recalculated using concentrations of 13Cca and 
00260         !  12Cca.  The fluxes (moles/m2/sec) of 13C and 12C out of the canopy 
00261         !  (the turbulent flux), and the del13C value (per mil vs PDB) of this 
00262         !  flux are calculated. 
00263 
00264         sib%prog%d13cca = ((c13ca/c12ca - pdb) / pdb) *1000.
00265 
00266 
00267         !  Use the following if you want the net flux from the canopy to 
00268         !  be based on differences in 12C and 13C net fluxes from respiration
00269         !  and photosynthesis
00270 
00271         !           sib%diag%flux13c = sib%diag%resp_grnd * rcresp / (1. + rcresp) -
00272         !     &  (sib%diag%assimn(6) * sib%diag%rcassimn / (1. + sib%diag%rcassimn))
00273 
00274         !           sib%diag%flux12c = sib%diag%resp_grnd / (1. + rcresp) -
00275         !     &  (sib%diag%assimn(6) / (1. + sib%diag%rcassimn))
00276 
00277         !  Use the following if you want the net flux from the canopy to 
00278         !  be based on differences  in concentration gradients between the 
00279         !  canopy and overlying atmosphere.
00280 
00281         sib%diag%flux13c = ( c13ca - c13cm) / sib%diag%ra 
00282         sib%diag%flux12c = ( c12ca - c12cm) / sib%diag%ra
00283 
00284         sib%diag%flux_turb = sib%diag%flux13c + sib%diag%flux12c
00285 
00286     end subroutine cfrax_final
00287 
00288 
00289 
00290 end module cfrax
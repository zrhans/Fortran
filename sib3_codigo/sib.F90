00001 !----------------------------------------------------------------------
00002 
00003 subroutine sib_main ( sib )
00004 
00005     use kinds
00006     use sibtype
00007     use sib_const_module
00008     use physical_parameters
00009 
00010     implicit none
00011 
00012 !---PARAMETERS---------------------------------------------------------
00013 
00014     type(sib_t), intent(inout) :: sib
00015     type(sib_local_vars) :: sib_loc
00016 
00017 
00018 !-------------------------------------------------------------------
00019 
00020 !     REFERENCES: Sato, N., P. J. Sellers, D. A. Randall, E. K. Schneider, 
00021 !          J. Shukla, J. L Kinter III, Y-T, Hou, and Albertazzi (1989) 
00022 !          "Effects of implementing the simple biosphere model in a general
00023 !          circulation model. J. Atmos. Sci., 46, 2767-2782.
00024 
00025 !                 Sellers, P. J., D. A. Randall, C. J. Collatz, J. A. Berry,
00026 !          C. B. Field, D. A. Dazlich, C. Zhang, G. Collelo (1996) A revised 
00027 !          land-surface parameterization (SiB2) for atmospheric GCMs. Part 1:
00028 !          Model formulation.
00029 
00030 
00031 !     SUBROUTINES CALLED: 
00032 
00033 
00034 !     FUNCTIONS CALLED:
00035 
00036 
00037 !----------------------------------------------------------------------
00038 !
00039 !   local variables
00040 !
00041 !----------------------------------------------------------------------
00042 
00043 
00044     real(kind=dbl_kind) :: zzwind    ! adjusted wind measurement height (m) 
00045     real(kind=dbl_kind) :: zztemp    ! adjusted temp measurement height (m)
00046 
00047 
00048 ! CSR consider full column from -nsnow+1 to nsoil here
00049 ! sometimes there is snow and nsl=0 (?!)
00050 
00051     !...water/energy balance variables...
00052     real(kind=dbl_kind), dimension(-nsnow+1:nsoil) :: 
00053         wwwliq_old   ! beginning-of-timetep soil/snow liquid
00054                                     !    (kg/m^2)
00055     real(kind=dbl_kind), dimension(-nsnow+1:nsoil) :: 
00056         wwwice_old   ! end-of-timestep soil/snow ice
00057                                     !    (kg/m^2)
00058                                     ! interception storage (kg/m^2)
00059     integer(kind=int_kind) :: nsl_old  
00060                                     ! beginning-of-timestep # of snow layers
00061 
00062 
00063     real(kind=dbl_kind) :: tsum   ! temp variable
00064     real(kind=dbl_kind) :: cas_q  ! beginning-of-timestep CAS moisture
00065     real(kind=dbl_kind) :: tliq
00066     real(kind=dbl_kind) :: icet,sbeg,send
00067 
00068     real(kind=dbl_kind),dimension(1) :: ppl, ttl,tess
00069                                                ! temp vars for call to eau_sat
00070                                                                                            ! dimension(1) to keep SGI compiler happy
00071 
00072     integer(kind=int_kind) :: i, j, k, n, ksoil, l
00073 
00074 
00075 !----------------------------------------------------------------------
00076 !
00077 !   end local variables
00078 !
00079 !----------------------------------------------------------------------
00080 
00081     !...first guesses for ta and ea
00082 
00083     !...load previous timestep soil temps
00084     !...also load soil ice/water for water balance check
00085     !...at end of timestep
00086 
00087     nsl_old = sib%prog%nsl
00088 
00089     do i = -nsnow+1,sib%prog%nsl
00090        sib%prog%www_liq(i)=0.
00091        sib%prog%www_ice(i)=0.
00092     enddo
00093       
00094 
00095     ! CSR: full snow+soil column
00096     do i = -nsnow+1,nsoil
00097         sib_loc%td_old(i) = sib%prog%td(i)
00098         wwwliq_old(i) = sib%prog%www_liq(i)
00099         wwwice_old(i) = sib%prog%www_ice(i)
00100     enddo
00101 
00102     ! CSR: full snow+soil column
00103     do i = sib%prog%nsl+1, 0
00104         sib_loc%frac_iceold(i) = sib%prog%www_ice(i)     &
00105             /(sib%prog%www_ice(i) + sib%prog%www_liq(i))
00106     enddo
00107 
00108     ! CSR: store old capac/snow
00109     sib%diag%capac_old=sib%prog%capac
00110     sib%diag%snow_veg_old=sib%prog%snow_veg
00111     sib%diag%snow_mass_old=sib%prog%snow_mass
00112 
00113     sib%prog%tha = sib%prog%ta  / sib%prog%bps(1)
00114     sib%prog%ea  = sib%prog%sha * sib%prog%ps / (0.622 + sib%prog%sha)
00115     sib%prog%em  = sib%prog%sh  * sib%prog%ps / (0.622 + sib%prog%sh)
00116 
00117     !...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX
00118     !
00119     !...temporarily hardwiring the carbon isotopic ratios of the mixed layer
00120     !...and respireation into SiBDRIVE
00121     sib%prog%d13cm    = -7.8              ! del13C of mixed layer
00122 !    sib%param%d13cresp   = -28.0             ! del13C of respiration
00123     !
00124     !...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX
00125 
00126 
00127     sib%diag%cuprt = sib%prog%cupr * 0.001
00128     sib%diag%lsprt = sib%prog%lspr * 0.001  ! converting units to m/sec
00129 
00130     sib%diag%roff       = 0.0
00131     sib%diag%roffo      = 0.0
00132     sib%prog%pco2ap_old = sib%prog%pco2ap
00133     sib%prog%cas_old = sib%prog%cas
00134 !    print*, 'sib', sib%prog%pco2ap
00135 
00136     !...calculate albedo/reflectance/transmissivity
00137     call rada2(sib,sib_loc)
00138 
00139     !...distribute incident radiation between canopy and surface
00140     call rnload(sib)
00141 
00142 
00143     sib%param%zpd_adj   = sib%param%z2 - ( sib%param%z2-sib%param%zp_disp )   &
00144                                            * sib%diag%canex
00145 
00146     sib%param%z0        = sib%param%z0d/( sib%param%z2-sib%param%zp_disp )    &
00147                                         * ( sib%param%z2-sib%param%zpd_adj )
00148 
00149 !    print*,'SiB, zpd:',sib%param%zp_disp,sib%param%zpd_adj
00150 !    print*,'SiB, z0:',sib%param%z0d,sib%param%z0
00151 !    print*,'SiB,z2:',sib%param%z2,sib%param%z1
00152 
00153     sib%param%rbc       = sib%param%cc1/sib%diag%canex
00154     sib%param%rdc       = sib%param%cc2*sib%diag%canex
00155 
00156 
00157 !     initialize energy and water budgets
00158 !     Initialize heat capacities, soil properties
00159     call begtem(sib,sib_loc)
00160 
00161 !   after begtem, get beginning-of-timestep CAS water in kg/m^2
00162     cas_q = sib%prog%sha * sib%prog%ros * sib%diag%cas_cap_co2
00163 
00164 
00165 !     CALCULATE RADT USING RADIATION FROM PHYSICS AND CURRENT
00166 !     LOSSES FROM CANOPY AND GROUND!
00167 
00168     call netrad(sib,sib_loc)
00169 
00170 !     GET RESISTANCES FOR SIB, update stomatal resistance 
00171 
00172     call vntlat(sib,sib_loc)
00173 
00174 !   this call for ustar, cu for oceanic value of z0 
00175 !   this is only used for near-coastal gridpoints...
00176 !    zzwind = sib%param%z2 - sib%param%zpd_adj + zwind
00177 !    zztemp = sib%param%z2 - sib%param%zpd_adj + ztemp
00178 
00179 !    call vmfcalzo(sib,zzwind,zztemp)
00180 
00181     !itb...calculate partial derivatives of the various heat fluxes
00182     !itb...with respect to ground/canopy/snow temp, as well as
00183     !itb...some other derivatives.
00184 
00185 
00186     call dellwf(sib,sib_loc)
00187 
00188     
00189     call delef(sib,sib_loc)
00190 
00191 
00192     call delhf(sib,sib_loc)
00193 
00194 !
00195 !   solve matrix of prognostic variables
00196 !
00197 
00198     call sibslv(sib,sib_loc)
00199 
00200 !    update prognostic variables, get total latent and sensible fluxes
00201 
00202     call addinc(sib,sib_loc)
00203 
00204     !itb...call eau_sat instead of vnqsat...
00205     ppl(1) = sib%prog%ps*100.0
00206     ttl(1) = sib%prog%ta
00207 
00208     call ess_eau(1,ppl,ttl,tess)
00209 
00210     sib%diag%eastar = tess(1)/100.0
00211 
00212     !...CAS relative humidity
00213     sib%diag%rha = sib%prog%ea / sib%diag%eastar
00214 
00215     !...update canopy and ground surface water stores; check that fluxes 
00216     !...are correct, no evaporating more water than is available.
00217     call update(sib,sib_loc)
00218 
00219     !...update water storage/interception on canopy and
00220     !...calculate throughfall
00221 
00222 
00223     call hydro_canopy(sib,sib_loc)
00224 
00225     !itb...precip in mm/sec after hydro_canopy: convert to mm/hour
00226     sib%diag%p0 = sib%diag%p0 * 3600.0
00227 
00228     !...update precipitation onto snowcover
00229     call hydro_snow(sib)
00230 
00231     !...update infiltration and soil water
00232     call hydro_soil(sib)
00233 
00234 
00235 !itb...the next three routines need only be called if there is snow
00236 !itb...snow layers are initialized in hydro_canopy.
00237 
00238 !    if(sib%prog%nsl < 0) then              !snow layers present
00239     !...compact snow levels 
00240 
00241        call compact_snow(sib,sib_loc)
00242 
00243     !...combine snow levels where necessary
00244        call combine_snow(sib)
00245 
00246 !       print*,'combine: ',nsl_old,sib%diag%snow_mass_old, wwwice_old(nsl_old+1)
00247 !       print*,'combine: ',sib%prog%nsl,sib%prog%snow_mass,sib%prog%www_ice(sib%prog%nsl+1)
00248 
00249     !...subdivide snow levels where necessary
00250 
00251        call subdivide_snow(sib)
00252 
00253 !    endif                                  !snow layers present
00254 
00255     !     for perpetual conditions, do not update soil moisture
00256 !         if(.not.fixday) then
00257 !            www(1) = wwwtem(1)
00258 !            www(2) = wwwtem(2)
00259 !            www(3) = wwwtem(3)
00260 !         endif
00261 
00262 !         do i = 1,len
00263 !            xgpp(i) = assim(i) * 1.e6
00264 !         enddo
00265 !     Calculate the surface flux of CO2 due to SiB (tracer T18)
00266 
00267 !     The net flux to the atmosphere is given by the release of CO2
00268 !     by soil respiration minus the uptake of CO2 by photosynthesis
00269 
00270 !     ASSIMN is the net assimilation of CO2 by the plants (from SiB2)
00271 !     respFactor*soilScale is the rate of release of CO2 by the soil
00272 
00273 !     soilScale is a diagnostic of the instantaneous rate of 
00274 !        soil respiration (derived by Jim Collatz, similar to TEM)
00275 !     respFactor is the annual total accumulation of carbon in the
00276 !        previous year at each grid cell (annual total ASSIMN) 
00277 !        divided by the annual total of soilScale at the same grid pt.
00278 
00279 !     Surface flux of CO2 used to be merely Assimn-Resp_grnd. With the
00280 !     prognostic CAS, the calculation becomes
00281 !
00282 !     co2flux =  (CO2A - CO2M)/ra
00283 !
00284 !     with a temperature correction thrown in. This calculation is 
00285 !     performed in phosib.
00286 
00287 
00288     !...check that energy and water balance is maintained
00289     call balan(sib,nsl_old,wwwliq_old,wwwice_old,cas_q)
00290 
00291 
00292 end subroutine sib_main
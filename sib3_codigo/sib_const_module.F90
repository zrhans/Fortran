00001 !...module to hold values for constants that are not contained in the
00002 !...BUGS module physical_parameters.F
00003 
00004 module sib_const_module
00005 
00006     use kinds
00007     use physical_parameters
00008 
00009     implicit none
00010     save
00011 
00012     real(kind=dbl_kind) :: dtt       ! model time step (seconds) 
00013     real(kind=dbl_kind) :: dti       ! inverse time step
00014 
00015     !--variables that retain a constant value throughout the simulation
00016     integer(kind=int_kind) ::     
00017         nsib,            !  number of SiB points in datasets
00018         subcount,        !  actual number of SiB point in simulation
00019         snowl,           !  number of (actual) snow layers
00020         ihr,             !  global points in x-direction
00021         jhr,             !  global points in y-direction
00022         nhr,             !  ihr*jhr (total global points)
00023         nper              ! actual number of ndvi composite periods
00024         
00025     integer(kind=long_kind) :: 
00026         endtime,         !  end time of integration -- units can vary
00027         starttime,       !  start time of integration -- units can vary
00028         dtsib,           !  timestep in seconds
00029         dtsibmetin,      !  driver data input interval (seconds)
00030         dtsibres,        !  restart interval (see namel_sibdrv for exp)
00031         dtsibout          !  output interval (see namel_sibdrv for exp)
00032 
00033      integer(kind=int_kind) :: 
00034         dtsibbcin,       !  sib boundary condition input interval
00035         nsecond,         !  simulation time, in seconds
00036         numsib,          !  check of # of sib points--init_sibdrv
00037         nstepsib          !  number of timesteps integrated
00038 
00039     integer(kind=int_kind) :: endyear
00040     integer(kind=int_kind) :: ndtsibpbp
00041 
00042     !itb...to nsib points in init_sibdrv.F
00043     real(kind=real_kind), dimension(:), allocatable    ::    
00044         latsib,        !  SiB point latitude
00045         lonsib,        !  SiB point longitude
00046         latitude,     
00047         longitude,    
00048         lonpbp,       
00049         latpbp
00050 
00051     integer(kind=int_kind), parameter :: nsoil = 10 ! number of soil layers 
00052     integer(kind=int_kind), parameter :: nsnow = 5  ! max number of snow layers
00053     integer(kind=int_kind), parameter :: physmax = 5 ! max number physiology types
00054                                                      ! (only C3 and C4 now, but capability for more)
00055     integer(kind=int_kind), parameter :: npermax = 365  ! max number ndvi composite periods per year
00056 
00057 
00058     !itb...some extra variables
00059     real(kind=real_kind) sin_dec  ! (-) sin solar declination
00060     real(kind=real_kind) cos_dec  ! (-) cosine solar declination
00061     real(kind=real_kind) tau      ! (TBD) time
00062 
00063     integer(kind=int_kind) ::      ! 
00064         startyear = 1,    !  time manager stuff
00065         eqnx   = 80     !  day of vernal equinox
00066 
00067     real(kind=real_kind) ::   
00068         lonearth        ! (rad) Earth lon about Sun from vernal equinox
00069 
00070     integer (kind=int_kind), dimension(:), allocatable   ::    
00071         subset,        !  array of landpoint indices for subgrid
00072         latindex,      !  latitude index array of all landpoints
00073         lonindex,      !  longitude index array of all landpoints
00074         sublat,        !  latitude index array of subset
00075         sublon          !  longitude index array of subset
00076 
00077     !itb...some SCALARS
00078     real(kind=dbl_kind) :: 
00079         c3day,         !  timesteps per day
00080         ztemp,         !  height of temperature measurement (m)
00081         zwind           !  height of wind measurement (m)
00082 
00083     !------------------------------------------------------------------------
00084     real (kind=dbl_kind), parameter  ::     
00085         version = 3.0,                !  code version identifier
00086         snomel  = 3.705185e8,         !  latent heat of fusion of ice (J m^-3) 
00087         cv     = 1952.0,              !  specific heat of water vapor at 
00088                                        !  constant pressure (J deg^-1 kg^-1)
00089         cpice  = 2117.27,             &!  specific heat of ice (J kg^-1 deg^-1)
00090         cpliq  = 4188.0,              &!  spec heat of water (J kg^-1 deg^-1)
00091 
00092 !itb...playing with vegetation heat capacity
00093 
00094 !itb...original.....
00095         clai   = 4.186*1000.0*0.2,    &!  leaf heat capacity  (J m^-2 deg^-1)
00096         cww    = 4.186*1000.0*1000.0, &!  water heat capacity (J m^-3 deg^-1)
00097         asnow  = 16.7,                &!  UNKNOWN
00098         rotper = 24.0,                &!  hours per day
00099         day    = rotper * 3600.0,     &!  seconds per day
00100         vkrmn  = 0.35,                &!  Von Karmann's constant (unitless)
00101         ribc   = 3.05,                &!  critical Richardson Number (unitless)
00102         pr0    = 0.74,                &!  turb Prandtl Number at neutral stblty
00103         tkemin = 0.01,                &!  minimum allowed value for tke
00104         rgfac  = 100.0/gas_const_r,   &!  
00105         cpdgrv = spec_heat_cp/grav,   &! 
00106         po2m   = 20900.0,             &!  mixed layer O2 concentration
00107         perhl  = 102.7,               &!  UNKNOWN     
00108         denh2o = 1000.0,              &!  density of water (kg/m^3)
00109         denice = 917.0,               &!  density of ice (kg/m^3) 
00110         tkair  = 0.023,               &!  thermal conductivity of air (W/m/K)
00111         tkwat  = 0.6,                 &!  thermal conductivity of water (W/m/K)
00112         tkice  = 2.29,                &!  thermal conductivity of ice (W/m/K)
00113         snofac = hltm/(hltm + snomel * 1.E-3), & !  ratio of hltm to hltm+ht 
00114                                        ! of fusion (see Sellers (1986) appendix B)
00115         wimp = 0.05,                  &!  water impermeable if porosity 
00116                                        !  below this value
00117         phmin = -1.e8,                &!  minimum value for soil potential (mm)
00118         !vwcmin = 0.1,                 &!  wilting point volumetric water content
00119         wtfact = 0.3,                 &!  fraction of area with high 
00120                                        !  (HARDWIRE PATCH) water table
00121         ssi = 0.033,                  &!  irreducible water fraction of snow
00122         zlnd = 0.01,                  &!  roughness length for land (m)
00123         eccn   = 0.016715,            &!  eccentricity
00124         daypyr = 365.0,               &!  days per  year
00125         decmax = 23.441,              &!  max declination
00126         cn_fact = 0.5,                &!  Crank-Nicholson factor
00127         cosz_min = -0.1045            !  minimum cosine of zenith angle value
00128                                        !  -0.1045 is 96 deg. which includes
00129                                        !  civil twilight
00130 
00131     real (kind=dbl_kind), parameter  :: 
00132         autofrac=0.5 ! (-) fraction GPP to autotrophic respiration
00133 
00134 
00135 
00136 
00137     !...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX...CFRAX
00138     real(kind=dbl_kind),parameter ::        
00139         pdb = 0.0112372   
00140     ! 13C/12C ratio of Pee Dee 
00141     !   Belemnite (no units)
00142 
00143     !...Carbon isotopic fractionation constants (units = per mil)         
00144     !...KIEC refers to Kinetic Isotope Effect (KIE) for Carbon (C), 
00145     !...and can be converted to alpha notation by alpha = (1 - KIEC/1000). 
00146     !...For a chemical reaction, alpha = Rreactant/Rproduct.  
00147     !...KIEs are sometimes referred to as epsilon factors. 
00148 
00149 
00150     real(kind=dbl_kind),parameter ::  
00151         kieclfbl  = - 2.9    
00152     ! canopy air space to leaf 
00153     !  boundary layer
00154     real(kind=dbl_kind),parameter ::  
00155         kiecstom  = - 4.4    
00156     ! leaf boundary layer to 
00157     !  stomatal cavity
00158     real(kind=dbl_kind),parameter ::  
00159         kieclphas =  -0.7    
00160     ! liquid phase fractionation 
00161     real(kind=dbl_kind),parameter ::  
00162         kiecdis   =  -1.1    
00163     ! dissolution
00164 
00165     real(kind=dbl_kind),parameter ::  
00166         kiecrbsco = -28.2    
00167     ! C3 C-fixation enzyme rubisco
00168 
00169     real(kind=dbl_kind),parameter ::  
00170         tref = 298.16       
00171     ! standard temperature (K)
00172 
00173     real(kind=dbl_kind),parameter ::  
00174         pref = 101325.0     
00175     ! standard pressure (Pa)
00176 
00177 !itb_iso 
00178     integer(kind=long_kind),parameter ::  
00179         d13c_auto_switch = 7
00180     ! number of days to integrate del 13C for autotrophic respiration
00181 !itb_iso 
00182 
00183 
00184 !itb_cos...some constants...
00185     real(kind=dbl_kind),parameter :: tm_length = 86400.0   
00186                 ! time, in seconds, of the averaging period
00187     real(kind=dbl_kind),parameter :: ca_q10 = 0.08747
00188 
00189 
00190 
00191 end module sib_const_module
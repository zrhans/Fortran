00001 !|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
00002         
00003         module physical_parameters
00004 
00005 !-----------------------------------------------------------------
00006 !       this modules specifices physical parameters and the units
00007 !         of those parameters (MKS is standard)
00008 !-----------------------------------------------------------------
00009 
00010         use kinds
00011 
00012         implicit none
00013         save
00014 
00015         real (kind=dbl_kind), parameter :: 
00016                 pi           = 3.14159265358979323846_dbl_kind, 
00017             pidaypy      = 0.0172142_dbl_kind, 
00018                 a            = 6.37122E+06_dbl_kind ,
00019                 grav         = 9.8100_dbl_kind,
00020                 gravi        = 1.0_dbl_kind/grav,
00021             omega        = 2.0_dbl_kind*pi/86400.0_dbl_kind,
00022                 earth_area   = 5.100996990707616E+14_dbl_kind,
00023                 gas_const_R  = 287.000_dbl_kind,
00024                 spec_heat_cp = 1005.000_dbl_kind,
00025                 kappa        = gas_const_R/spec_heat_cp,
00026                 inv_kappa    = 1.0_dbl_kind / kappa,
00027                 p0_sfc       = 1.0E+05_dbl_kind,
00028                 inv_p0_sfc   = 1.0_dbl_kind / p0_sfc,
00029             tice         = 273.15_dbl_kind,
00030             hltm         = 2.25E+06_dbl_kind,
00031             gamfac       = hltm*5417.9827_dbl_kind/spec_heat_cp,
00032             delta        = 0.608_dbl_kind,
00033             stefan       = 5.67e-08_dbl_kind,
00034             rv           = 4.61e+02_dbl_kind
00035 
00036         real (kind=dbl_kind), parameter ::
00037                 c0     = 0.00000_dbl_kind,
00038                 c1     = 1.00000_dbl_kind,
00039                 c2     = 2.00000_dbl_kind,
00040                 c3     = 3.00000_dbl_kind,
00041                 p5     = 0.50000_dbl_kind,
00042                 p25    = 0.25000_dbl_kind,
00043                 pi2    = 2*pi,
00044                 dtr    = pi/180.0_dbl_kind,
00045                 rtd    = 180.0_dbl_kind/pi,
00046                 alpha2 = 0.0
00047 
00048 !-------------------------------------------------------------------
00049 !   VARIABLE DEFINITION
00050 !-------------------------------------------------------------------
00051 !       pi = pi
00052 !       a = earth radius (m)
00053 !       omega = earth angular velocity (1/s)
00054 !       earth_area = surface area of earth (m2)
00055 !       gas_const_R = gas constant for dry air (J/ (kg K))
00056 !       spec_heat_cp = specific heat at constant pressure (J/ (kg K))
00057 !       kappa = gas_const_R/spec_heat_cp
00058 !       p0_sfc = surface pressure (Pa)
00059 !       alpha2 = rotation of axis of rotation from NP/SP
00060 !       tice = freezing temperature of water at 1 atm (K)
00061 !       hltm = latent heat of vaporization (J/kg)
00062 !       gamfac = a moist thermodynamic variable
00063 !       delta = molecular_weight_air/molecular_weight_water - 1
00064 !       inv_kappa = inverse kappa
00065 !       stefan = stefan boltzmann constant (W/m^2/K^4)
00066 !       rv = gas constant for water vapor
00067 !
00068 !       c0 = zero
00069 !       c1 = one
00070 !       c2 = two
00071 !       p5 = 0.5
00072 !       p25 = 0.25
00073 !       pi2 = 2*pi
00074 !       dtr = Degrees To Radians conversion constant
00075 !       rtd = Radians To Degrees conversion constant
00076 !-------------------------------------------------------------------
00077 
00078         end module physical_parameters
00079 
00080 !|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
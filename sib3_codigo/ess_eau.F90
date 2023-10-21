00001 !------------------------------------------------------------------------------
00002 subroutine ess_eau (len, pl, tl, ess)
00003 !------------------------------------------------------------------------------
00004 !eau_sat computes the saturation mixing ratio, vapor pressure, and saturation,
00005 !and their derivatives with respect to temperature over water, ice and mixed-
00006 !phase clouds. the parameterization is that used in the Community Climate Com-
00007 !munity Climate Model at NCAR.
00008 !Laura D. Fowler /slikrock (07-01-01).
00009 
00010 !send comments to laura@atmos.colostate.edu.
00011 
00012 !subroutines called:
00013 !none.
00014 
00015 !argument list variables:
00016 !input arguments:
00017 !----------------
00018 
00019 use kinds
00020       
00021 implicit none
00022       
00023 
00024 integer (kind=int_kind), intent(in) :: len                !length of vector.
00025 
00026 real(kind=dbl_kind), intent(in), dimension(len):: 
00027    pl,               !pressure                                           (Pa).
00028    tl                 !temperature                                         (K).
00029 
00030 !output arguments:
00031 !-----------------
00032 
00033 real (kind=dbl_kind), intent(out), dimension(len) :: 
00034    ess                !saturation vapor pressure                          (Pa).
00035 
00036 !local variables:
00037 
00038 integer (kind=int_kind):: i
00039 
00040 real (kind=dbl_kind):: 
00041    twmin=173.16,     !lowest allowed temperature boundary for water       (K).
00042    twmax=373.16,     !highest allowed temperature boundary for water      (K).     
00043    timin=173.16,     !lowest allowed temperature boundary for ice         (K).
00044    timax=273.16,     !highest allowed temperature boundary for ice        (K).
00045    tnull=273.16       !freezing temperature                                (K).
00046 
00047 real (kind=dbl_kind) :: tstl , t0tl
00048 
00049 real (kind=dbl_kind), dimension(len):: 
00050       esw ,     esi ,
00051       esm ,     tl0 ,            
00052     wghtm 
00053 
00054 !ccm parameterization of saturation vapor pressure over water and ice:
00055 
00056 real (kind=dbl_kind), parameter:: 
00057    ps = 1013.246,                   !reference pressure             (hPa).
00058    ts = 373.16,                     !reference temperature            (K).
00059    t0 = 273.16,                     !freezing temperature             (K)
00060    tbgmin   = 253.15_dbl_kind,      !
00061    tbgmax   = 273.15_dbl_kind        !
00062   
00063 real (kind=dbl_kind):: 
00064        e1 ,   e2 ,     f ,    f1 ,
00065        f2 ,   f3 ,    f4 ,    f5 ,
00066    lphase , term , term1 , term2 ,
00067    term3     
00068 
00069 !------------------------------------------------------------------------------
00070 
00071 !initialization of different arrays:
00072 
00073 tl0    = tl
00074 esw    = 0.0_dbl_kind
00075 esi    = 0.0_dbl_kind
00076 esm    = 0.0_dbl_kind
00077 ess    = 0.0_dbl_kind
00078 
00079 !saturation over water:
00080 
00081 do i = 1, len
00082 
00083    tl0(i)    = max(twmin,tl0(i))
00084    tl0(i)    = min(twmax,tl0(i))
00085    tstl      = ts / tl0(i)
00086    e1        = 11.344*(1.0 - tl0(i)/ts)
00087    e2        = -3.49149*(tstl - 1.0)
00088    f1        = -7.90298*(tstl - 1.0)
00089    f2        = 5.02808*log10(tstl)
00090    f3        = -1.3816*(10.0**e1-1.0)/10000000.0
00091    f4        = 8.1328*(10.0**e2-1.0)/1000.0
00092    f5        = log10(ps)
00093    f         = f1 + f2 + f3 + f4 + f5
00094 
00095    esw(i)    = (10.0**f)*1.e+02
00096    esw(i)    = min(esw(i),pl(i)*0.9)
00097 
00098    ess(i)    = esw(i)
00099 
00100 
00101 !saturation over ice:
00102 
00103    if(tl0(i).lt.timax) then
00104 
00105       tl0(i)    = max(tl0(i),timin)
00106       t0tl      = t0 / tl0(i)
00107       term1     = 2.01889049/(t0tl)
00108       term2     = 3.56654*log(t0tl)
00109       term3     = 20.947031*(t0tl)
00110 
00111       esi(i)    = 575.185606e10*exp(-(term1 + term2 + term3))
00112       esi(i)    = min(esi(i),pl(i)*0.9)
00113 
00114       ess(i)    = esi(i)
00115 
00116    endif
00117 
00118 !interpolated saturation variables:
00119 
00120    if(tl0(i).lt.tbgmax .and. tl0(i).ge.tbgmin) then
00121 
00122       wghtm(i)  = (tl0(i)-tbgmin)/(tbgmax-tbgmin)
00123       esm(i)    = wghtm(i)*esw(i) + (1.-wghtm(i))*esi(i)
00124       esm(i)    = min(esm(i),pl(i)*0.9)
00125 
00126       ess(i)    = esm(i)
00127 
00128    endif
00129 enddo
00130 
00131 end subroutine ess_eau
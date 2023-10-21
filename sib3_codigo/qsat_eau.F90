00001 !------------------------------------------------------------------------------
00002 subroutine qsat_eau (len, pl, tl, qsats)
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
00020 use physical_parameters, only: hltm, rv
00021       
00022 implicit none
00023 
00024 integer (kind=int_kind), intent(in):: 
00025    len                !length of vector.
00026 
00027 real(kind=dbl_kind), intent(in), dimension(len):: 
00028    pl,               !pressure                                           (Pa).
00029    tl                 !temperature                                         (K).
00030 
00031 !output arguments:
00032 !-----------------
00033 
00034 real (kind=dbl_kind), intent(out), dimension(len), optional :: 
00035    qsats              !saturation mixing ratio                         (kg/kg).
00036 
00037 !local variables:
00038 
00039 integer (kind=int_kind):: i
00040 
00041 real (kind=dbl_kind):: 
00042    twmin=173.16,     !lowest allowed temperature boundary for water       (K).
00043    twmax=373.16,     !highest allowed temperature boundary for water      (K).     
00044    timin=173.16,     !lowest allowed temperature boundary for ice         (K).
00045    timax=273.16,     !highest allowed temperature boundary for ice        (K).
00046    tnull=273.16       !freezing temperature                                (K).
00047 
00048 real (kind=dbl_kind) :: tstl , t0tl
00049 
00050 real (kind=dbl_kind), dimension(len):: 
00051       esw ,  esi ,  
00052       esm ,  tl0,   
00053     qsati ,   dqsati ,    qsatm , 
00054     wghtm ,   qsatw
00055 
00056 !ccm parameterization of saturation vapor pressure over water and ice:
00057 
00058 real (kind=dbl_kind), parameter:: 
00059    ps = 1013.246,    !reference pressure                                (hPa).
00060    ts = 373.16,      !reference temperature                               (K).
00061    t0 = 273.16,      !freezing temperature                                (K)
00062    tbgmin   = 253.15_dbl_kind,      !
00063    tbgmax   = 273.15_dbl_kind        !
00064    
00065 real (kind=dbl_kind):: 
00066        e1 ,   e2 ,     f ,    f1 ,
00067        f2 ,   f3 ,    f4 ,    f5 ,
00068    lphase , term3 , term1 , term2  
00069 
00070 !------------------------------------------------------------------------------
00071 
00072 !initialization of different arrays:
00073 
00074 tl0    = tl
00075 esw    = 0.0_dbl_kind
00076 esi    = 0.0_dbl_kind
00077 esm    = 0.0_dbl_kind
00078 qsatw  = 0.0_dbl_kind
00079 qsati  = 0.0_dbl_kind
00080 qsatm  = 0.0_dbl_kind
00081 qsats  = 0.0_dbl_kind
00082 
00083 !saturation over water:
00084 
00085 do i = 1, len
00086 
00087    tl0(i)    = max(twmin,tl0(i))
00088    tl0(i)    = min(twmax,tl0(i))
00089    tstl      = ts / tl0(i)
00090    e1        = 11.344*(1.0 - tl0(i)/ts)
00091    e2        = -3.49149*(tstl - 1.0)
00092    f1        = -7.90298*(tstl - 1.0)
00093    f2        = 5.02808*log10(tstl)
00094    f3        = -1.3816*(10.0**e1-1.0)/10000000.0
00095    f4        = 8.1328*(10.0**e2-1.0)/1000.0
00096    f5        = log10(ps)
00097    f         = f1 + f2 + f3 + f4 + f5
00098 
00099    esw(i)    = (10.0**f)*1.e+02
00100    esw(i)    = min(esw(i),pl(i)*0.9)
00101    qsatw(i)  = 0.622*esw(i)/(pl(i)-esw(i))
00102 
00103    qsats(i)  = qsatw(i)
00104 
00105 !saturation over ice:
00106 
00107    if(tl0(i).lt.timax) then
00108 
00109       tl0(i)    = max(tl0(i),timin)
00110       t0tl      = t0 / tl0(i)
00111       term1     = 2.01889049/(t0tl)
00112       term2     = 3.56654*log(t0tl)
00113       term3     = 20.947031*(t0tl)
00114 
00115       esi(i)    = 575.185606e10*exp(-(term1 + term2 + term3))
00116       esi(i)    = min(esi(i),pl(i)*0.9)
00117       qsati(i)  = 0.622*esi(i) / (pl(i)-esi(i))
00118 
00119       qsats(i)  = qsati(i)
00120 
00121    endif
00122 
00123 !interpolated saturation variables:
00124 
00125    if(tl0(i).lt.tbgmax .and. tl0(i).ge.tbgmin) then
00126 
00127       wghtm(i)  = (tl0(i)-tbgmin)/(tbgmax-tbgmin)
00128       esm(i)    = wghtm(i)*esw(i) + (1.-wghtm(i))*esi(i)
00129       esm(i)    = min(esm(i),pl(i)*0.9)
00130       qsatm(i)  = 0.622*esm(i) / (pl(i) - esm(i))
00131 
00132       qsats(i)  = qsatm(i)
00133 
00134    endif
00135 enddo
00136 
00137 end subroutine qsat_eau
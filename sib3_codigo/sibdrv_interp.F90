00001 !--------------------------------------------------------------
00002 subroutine sibdrv_interp(sib, time)
00003 !--------------------------------------------------------------
00004 ! This subroutine interpolates the sibdrv forcing surface meteorological
00005 ! variables between their read times
00006 !
00007 ! Modifications:
00008 !  Kevin Schaefer changed dayflag to cosz_min when checking if sw_rad in scaled (8/12/04)
00009 !  Kevin Schaefer removed checks on sw_dwn between 1-5 (created light at night) (8/12/04)
00010 !  Kevin Schaefer moved pressure conversion to read_drvr routines (8/16/04)
00011 !  Kevin Schaefer move conversion from dew point to humidity to read_drvr routines (8/17/04)
00012 !  Kevin Schaefer synched facsibdrv with 1 step earlier driver data read (8/18/04)
00013 !  Kevin Schaefer deleted unused variables & commented code (11/15/04)
00014 !--------------------------------------------------------------
00015 
00016 use kinds
00017 use sibtype
00018 use timetype
00019 use sib_const_module, only:  &
00020     rgfac,         &
00021     subcount,      &
00022     cosz_min
00023 use physical_parameters, only:  kappa
00024 !
00025 implicit none
00026 !
00027 ! Input parameters
00028 type(sib_t), dimension(subcount), intent(inout) :: sib
00029 type(time_struct), intent(in) :: time
00030 !
00031 ! local variables
00032 real(kind=dbl_kind):: facsibdrv  ! scaling factor between driver data points
00033 real(kind=dbl_kind):: temp       ! temporary variable for testing
00034 integer(kind=int_kind) :: i      ! index
00035 !
00036 ! initialize CO2 partial pressure in boundary layer
00037     do i = 1,subcount
00038         sib(i)%prog%pco2m = 35.0
00039         sib(i)%prog%psb = 50.0
00040 
00041 !itb_cos
00042         sib(i)%prog%pcosm = 4.5E-5
00043 !itb_cos
00044 
00045 
00046     enddo
00047 !
00048 ! calculate cosine zenith angle
00049     call zenith_angle( time%hour, sib(:)%stat%cosz )
00050 !
00051 ! scale downwelling driver SW radiation
00052     do i = 1,subcount
00053 !
00054 ! scale SW radiation to cosine of zenith angle
00055         if((sib(i)%stat%cosz>cosz_min).and.(sib(i)%stat%coszbar.ne.0.0_dbl_kind))then
00056                 sib(i)%prog%sw_dwn = sib(i)%prog%sw_dwn2 *  max(sib(i)%stat%cosz-cosz_min,0.0_dbl_kind )/sib(i)%stat%coszbar
00057         else
00058             sib(i)%prog%sw_dwn=0.
00059         endif
00060 
00061 !
00062 ! make sure downwelling SW radiation is positive
00063         if(sib(i)%prog%sw_dwn<0.) sib(i)%prog%sw_dwn = 0.
00064     enddo
00065 !
00066 ! split downwelling SW radiation to visible and NIR (direct and diffuse)
00067     call raddrv(subcount,sib(:)%prog%sw_dwn,sib(:)%stat%cosz,sib%prog%radvbc,  &
00068         sib%prog%radvdc,sib%prog%radnbc,sib%prog%radndc)
00069 !
00070 ! calculate driver data interpolation factor
00071     facsibdrv = 1. -float(mod(time%sec_day,time%driver_step)) / float(time%driver_step)
00072 !
00073 ! loop through all land points
00074     do i = 1,subcount
00075 !
00076 ! interpolate driver temperature
00077         sib(i)%prog%tm = facsibdrv*sib(i)%prog%tm1 + (1. - facsibdrv) *  &
00078             sib(i)%prog%tm2
00079 !
00080 ! interpolate driver humidity
00081         sib(i)%prog%sh = facsibdrv*sib(i)%prog%sh1 + (1. - facsibdrv) *  &
00082             sib(i)%prog%sh2
00083 !
00084 ! interpolate driver surface pressure
00085         sib(i)%prog%ps = facsibdrv*sib(i)%prog%ps1 +   &
00086             (1. - facsibdrv) * sib(i)%prog%ps2
00087 !
00088 ! interpolate driver wind speed
00089         sib(i)%prog%spdm = facsibdrv*sib(i)%prog%spdm1 +   &
00090             (1. - facsibdrv) * sib(i)%prog%spdm2
00091         sib(i)%prog%spdm = MAX(sib(i)%prog%spdm,1.0_dbl_kind)
00092 !
00093 ! interpolate driver large scale precipitation
00094         sib(i)%prog%lspr =  sib(i)%prog%lspr1 / time%driver_step
00095 !
00096 ! interpolate driver cumulus or convective precipitation
00097         sib(i)%prog%cupr =  sib(i)%prog%cupr1 / time%driver_step
00098 !
00099 ! interpolate driver downwelling longwave radiation
00100         sib(i)%prog%dlwbot = facsibdrv*sib(i)%prog%dlwbot1 +   &
00101             (1. - facsibdrv) * sib(i)%prog%dlwbot2
00102 !
00103 ! calculate reference level potential temperature and some related parameters
00104         sib(i)%prog%bps(1) = (0.001*sib(i)%prog%ps)**kappa
00105         sib(i)%prog%bps(2) = (0.001*(sib(i)%prog%ps-sib(i)%prog%psb))**kappa
00106         sib(i)%prog%thm = sib(i)%prog%tm / sib(i)%prog%bps(1)
00107 !
00108 ! calculate reference level air density
00109         sib(i)%prog%ros = rgfac * sib(i)%prog%ps / sib(i)%prog%tm
00110 
00111     enddo  ! land point loop
00112 !
00113 ! initialize Canopy Air Space humidity
00114    !ogl...changed to an explicit do loop
00115     if( time%sec_tot == time%init_second ) then
00116         print*, 'sibdrv_interp: init CAS humidity'
00117         do i=1,subcount
00118             sib(i)%prog%sha = sib(i)%prog%sh
00119         enddo
00120     endif
00121 !
00122 end subroutine sibdrv_interp
00123 !
00124 !---------------------------------------------------------------------
00125 subroutine raddrv(nsib,swdown,sunang,radvbc,radvdc,radnbc,radndc)
00126 !---------------------------------------------------------------------
00127 !               radiation radive code to use the downward sw at bottom 
00128 !               and the formulation to estimate radvbc,radvdc, radndc,
00129 !               radndc
00130 !---------------------------------------------------------------------
00131 
00132 use kinds
00133 implicit none
00134 
00135 integer(kind=int_kind) :: nsib, i
00136 real(kind=real_kind) ::  
00137     cloud,          
00138     difrat,         
00139     vnrat
00140 real(kind=dbl_kind) ::  
00141     swdown(nsib),   
00142     sunang(nsib),   
00143     stemp,           
00144     localcosz
00145 real(kind=dbl_kind) ::  
00146     radvbc(nsib),   
00147     radvdc(nsib),   
00148     radnbc(nsib),   
00149     radndc(nsib)
00150 
00151 real(kind=dbl_kind),parameter :: c1 = 580.
00152 real(kind=dbl_kind),parameter :: c2 = 464.
00153 real(kind=dbl_kind),parameter :: c3 = 499.
00154 real(kind=dbl_kind),parameter :: c4 = 963.
00155 real(kind=dbl_kind),parameter :: c5 = 1160.
00156 
00157     do i=1,nsib
00158         localcosz = max( 0.001_dbl_kind, sunang(i) )
00159         stemp = swdown(i)
00160         stemp = MAX(stemp,0.01_dbl_kind)
00161 
00162         cloud = (c5 * localcosz - stemp) / (c4 * localcosz)                   
00163         cloud = max(cloud,0.)                                                
00164         cloud = min(cloud,1.)                                                  
00165 
00166         difrat = 0.0604 / ( sunang(i)-0.0223 + 1.0e-10 ) + 0.0683
00167         if ( difrat < 0. ) difrat = 0.
00168         if ( difrat > 1. ) difrat = 1.
00169         difrat = difrat + ( 1. - difrat ) * cloud
00170 
00171         vnrat = ( c1 - cloud*c2 ) / ( ( c1 - cloud*c3 ) + ( c1 - cloud*c2 ) )
00172 
00173         radvbc(i) = (1.-difrat)*vnrat*stemp
00174         radvdc(i) = difrat*vnrat*stemp
00175         radnbc(i) = (1.-difrat)*(1.-vnrat)*stemp
00176         radndc(i) = difrat*(1.-vnrat)*stemp
00177     enddo
00178 
00179 end subroutine raddrv
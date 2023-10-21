00001 !-------------------------------------------------------------------------------
00002 subroutine init_solar_dec( time )
00003 !-------------------------------------------------------------------------------
00004 ! initializes the declination of the Sun
00005 !
00006 ! FUNCTIONS CALLED:
00007 !  rt_asc
00008 !
00009 ! MODIFICATIONS:
00010 !  Created by Kevin Schaefer (3/4/03)
00011 !  Kevin Schaefer moved time indep calculations to sib_main_mod (3/5/03)
00012 !-----------------------------------------------------------------------
00013 
00014 use kinds
00015 use timetype
00016 use sib_const_module
00017 use physical_parameters
00018 implicit none
00019 
00020 ! parameters
00021 type(time_struct), intent(in) :: time
00022 
00023 ! local variables
00024 real(kind=real_kind) :: t1       ! 1st factor to determine longitude of earth (lonearth)
00025 real(kind=real_kind) :: t2       ! 2nd factor to determine longitude of earth (lonearth)
00026 real(kind=real_kind) :: t3       ! 3rd factor to determine longitude of earth (lonearth)
00027 real(kind=real_kind) :: t4       ! 4th factor to determine longitude of earth (lonearth)
00028 integer(kind=int_kind) :: iday    ! day of year since vernal equinox variable
00029 real(kind=real_kind) :: rt_asc
00030 
00031     ! lon of Earth from equinox at start of simulation
00032     iday = time%doy - eqnx
00033     if ( iday < 0 ) iday = iday + time%days_per_year
00034     lonearth=0.0
00035     if ( iday /= 0 ) then
00036       do while ( iday > 0 )
00037         iday = iday - 1
00038         t1 = rt_asc( lonearth ) * pidaypy
00039         t2 = rt_asc( lonearth+t1*.5 ) * pidaypy
00040         t3 = rt_asc( lonearth+t2*.5 ) * pidaypy
00041         t4 = rt_asc( lonearth+t3 ) * pidaypy
00042         lonearth = lonearth + (t1+2.*(t2+t3)+t4) / 6.
00043       enddo
00044     endif
00045 
00046 end subroutine init_solar_dec
00047 
00048 
00049 !-------------------------------------------------------------------------------
00050 subroutine solar_dec( time )
00051 !-------------------------------------------------------------------------------
00052 ! Calculates the declination of the Sun
00053 !
00054 ! FUNCTIONS CALLED:
00055 !  rt_asc
00056 !
00057 ! MODIFICATIONS:
00058 !  Created by Kevin Schaefer (3/4/03)
00059 !  Kevin Schaefer moved time indep calculations to sib_main_mod (3/5/03)
00060 !-----------------------------------------------------------------------
00061 
00062 use kinds
00063 use timetype
00064 use sib_const_module
00065 use physical_parameters
00066 implicit none
00067 
00068 ! parameters
00069 type(time_struct), intent(in) :: time
00070 
00071 ! local variables
00072 real(kind=real_kind) :: t1  ! 1st factor to determine longitude of earth (lonearth)
00073 real(kind=real_kind) :: t2  ! 2nd factor to determine longitude of earth (lonearth)
00074 real(kind=real_kind) :: t3  ! 3rd factor to determine longitude of earth (lonearth)
00075 real(kind=real_kind) :: t4  ! 4th factor to determine longitude of earth (lonearth)
00076 integer(kind=int_kind) :: iday    ! day of year since vernal equinox variable
00077 real(kind=real_kind) :: rt_asc
00078 
00079     ! reset lon of Earth from equinox
00080     if( time%doy == eqnx ) lonearth = 0.0
00081 
00082     ! Increment Longitude of Earth
00083     t1 = rt_asc( lonearth ) * pidaypy
00084     t2 = rt_asc( lonearth+t1*.5 ) * pidaypy
00085     t3 = rt_asc( lonearth+t2*.5 ) * pidaypy
00086     t4 = rt_asc( lonearth+t3 ) * pidaypy
00087     lonearth = lonearth + (t1+2.*(t2+t3)+t4) / 6.
00088 
00089     ! Calculate the sine and cosine of Solar declination
00090     sin_dec = sin( decmax*(pi/180.) ) * sin( lonearth )
00091     cos_dec = sqrt( 1. - sin_dec * sin_dec )
00092 
00093 
00094 end subroutine solar_dec
00095 
00096 
00097 !-------------------------------------------------------------------------------
00098 function rt_asc( ang )
00099 !-------------------------------------------------------------------------------
00100 ! calculates correction for longitude (right ascension) of the Earth
00101 ! from vernal equinox based on the angle around Sun traversed
00102 ! since beginning of the year
00103 
00104     use kinds
00105     use sib_const_module
00106     use physical_parameters
00107     implicit none
00108 
00109 real(kind=real_kind) :: rt_asc ! right ascension correction
00110 real(kind=real_kind) :: ang    ! angle from beginning of year (radians)
00111 
00112 ! local variables
00113 real(kind=real_kind) :: reccn
00114 real(kind=real_kind) :: perhlr
00115 
00116     ! rt ascension correction based on Earth's orbit
00117     reccn  = 1. / (1. - eccn * eccn ) **1.5
00118     perhlr = perhl * (pi/180.)
00119     rt_asc = reccn * (1. - eccn * cos( ang - perhlr ) ) **2
00120 
00121 end function rt_asc
00122 
00123 
00124 !=======================================================================
00125 subroutine mean_zenith_angle( sib, time )
00126 !=======================================================================      
00127 ! Calculates a mean cosine zenith angle for civil twilight and daylight  
00128 ! hours between driver data points to scale driver radiation data.
00129 ! Daylight is defined as zenith angle<=90. (cosz>=0)
00130 ! Civil twilight is defined as 96<zenith angle<90 (-.1045<cosz<0).
00131 ! Night is defined as zenith angle>=96. (cosz<=-.1045)
00132 !
00133 ! Modifications:
00134 !  Kevin Schaefer created mean_zenith_angle subroutine (5/31/01)
00135 !  Kevin Schaefer changed coszbar calculation to daylight only (5/31/01)
00136 !  Kevin Schaefer included civil twilight bias in mean zenith angle (7/21/01)
00137 !  Kevin Schaefer deleted use sib_main_module; only variable list (3/4/03)
00138 !----------------------------------------------------------------------
00139 
00140 use kinds
00141 use sibtype
00142 use timetype
00143 use sib_const_module
00144 implicit none
00145 
00146 ! parameters
00147 type(sib_t), dimension(subcount), intent(inout) :: sib
00148 type(time_struct), intent(in) :: time
00149 
00150 ! local variables
00151 integer i         ! sib point index
00152 integer n         ! time of day index
00153 integer nsteps    ! number SiB time steps per driver data time step
00154 real loctofday    ! local version of time of day (GMT, in hours)
00155 
00156 
00157     ! Clear out arrays
00158     sib(:)%stat%coszbar = 0.
00159     sib(:)%stat%dayflag = 0.
00160 
00161     ! calculate number of sib time steps per driver data time step
00162     nsteps = time%driver_step / time%dtsib
00163 
00164     ! Integrate cosz over driver data time step (tofday to tofday+dtsibmetin)
00165     loctofday = time%hour
00166     do n = 1, nsteps
00167 
00168 
00169         ! calculate zenith angle at local time of day
00170         call zenith_angle ( loctofday, sib(:)%stat%cosz )
00171 
00172 
00173         ! check for civil twilight (-.1045<loccosz<0.) or daylight (loccosz>0)
00174         ! add loccosz to sum, include bias to account for civil twilight
00175         do i = 1, subcount
00176             if( sib(i)%stat%cosz > cosz_min ) then
00177                 sib(i)%stat%dayflag = sib(i)%stat%dayflag + 1.
00178                 sib(i)%stat%coszbar = sib(i)%stat%coszbar +  &
00179                     sib(i)%stat%cosz - cosz_min
00180             endif
00181         enddo
00182 
00183         ! increment local time of day
00184         loctofday = loctofday + real(time%dtsib)/3600.
00185     enddo
00186 
00187     ! Calculate mean cosz during civil twilight and daylight hours
00188     do i = 1, subcount
00189         if( sib(i)%stat%coszbar > 0. )  &
00190             sib(i)%stat%coszbar = sib(i)%stat%coszbar / nsteps
00191     enddo
00192 
00193 end subroutine mean_zenith_angle
00194 
00195 
00196 !=======================================================================
00197 subroutine zenith_angle ( hour, cosz )
00198 !=======================================================================      
00199 ! calculates the zenith angle for an array of lat/lon points.
00200 ! The cos_hour variable accounts for the difference between GMT and local 
00201 !  time
00202 !
00203 ! Modifications:
00204 !  Kevin Schaefer created zenith_angle subroutine (5/31/01)
00205 !  Kevin Schaefer corrected hour angle calculation (5/31/01)
00206 !  Kevin Schaefer removed minimum cosz limit to include twilight (7/21/01)
00207 !----------------------------------------------------------------------
00208 
00209 use kinds
00210 use sib_const_module
00211 implicit none
00212 
00213 ! parameters
00214 real(kind=real_kind), intent(in) :: hour
00215 real(kind=dbl_kind), dimension(subcount), intent(out) :: cosz
00216 
00217 ! internal variables
00218 real pid180      ! conversion from degrees to radians
00219 real sinlat      ! sine of latitude
00220 real coslat      ! cosine of latitude
00221 real hrang       ! hour angle; longitude of Sun from Greenwhich meridian
00222 real cos_hour    ! cosine delta longitude between Sun & SiB point
00223 integer i        ! sib point index
00224 
00225     ! calculate conversion from degrees to radians
00226     pid180 = 3.14159/180.
00227 
00228     ! Calculate hour angle (longitude of Sun from Greenwhich meridian)
00229     hrang = (12. - hour) * 360. / 24.
00230 
00231     ! Calculate cosine of solar zenith angle for each SiB point
00232     do i = 1,subcount
00233         cos_hour = cos( pid180 * ( hrang - lonsib(subset(i)) ) )
00234         sinlat = sin( pid180 * latsib(subset(i)) )
00235         coslat = cos( pid180 * latsib(subset(i)) )
00236         cosz(i) = coslat * cos_dec * cos_hour + sinlat * sin_dec
00237     enddo
00238 
00239 end subroutine zenith_angle
00240 
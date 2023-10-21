00001 module timetype
00002 
00003 !-------------------------------------------------------------------------------
00004 ! Author:  Owen Leonard
00005 ! Date:    April 17, 2004
00006 ! Purpose:
00007 !   This modules contains a user defined type containing all of the time 
00008 ! variables assigned values in time_check, and updated by time_manager.
00009 !-------------------------------------------------------------------------------
00010 
00011 use kinds
00012 use sib_const_module
00013 implicit none
00014 
00015 public time_struct
00016 
00017 type time_struct
00018     
00019     
00020     ! time constants
00021     integer(kind=long_kind) :: init_year     ! initial year of simulation
00022     integer(kind=long_kind) :: init_month    ! initial month of simulation
00023     integer(kind=long_kind) :: init_day      ! initial day of month of simulation
00024     integer(kind=long_kind) :: init_doy      ! initial day of year of simulation
00025     integer(kind=long_kind) :: init_second  ! initial second of simulation
00026 
00027     integer(kind=long_kind) :: start_year    ! year of restart file
00028     integer(kind=long_kind) :: start_month   ! month of restart file
00029     integer(kind=long_kind) :: start_day     ! day of month of restart file
00030     integer(kind=long_kind) :: start_doy     ! day of year of restart file
00031     integer(kind=long_kind) :: start_second ! second of restart file
00032     
00033     integer(kind=long_kind) :: end_year      ! last year of simulation
00034     integer(kind=long_kind) :: end_month     ! last month of simulation
00035     integer(kind=long_kind) :: end_day       ! last day of month of simulation
00036     integer(kind=long_kind) :: end_doy       ! last day of year of simulation
00037     integer(kind=long_kind) :: end_second   ! last second of simulation
00038     
00039     integer(kind=long_kind) :: total_years   ! total number of years in simulation
00040     integer(kind=long_kind) :: total_months  ! total number of months in simulation
00041     integer(kind=long_kind) :: total_days    ! total number of days in simulation
00042 
00043     integer(kind=long_kind) :: dtsib         ! # seconds in simulation time step
00044     integer(kind=long_kind) :: bc_int_step   ! # seconds between bc interpolation
00045     integer(kind=long_kind) :: drvr_int_step ! # seconds between driver data interpolation
00046     
00047     real(kind=real_kind),   dimension(12) :: mid_month      ! mid-month day of year
00048     integer(kind=long_kind), dimension(12) :: days_per_month ! # days per month
00049     integer(kind=long_kind), dimension(12) :: doy1_month     ! day of year of first 
00050                                                             !   day in month
00051     character(len=10), dimension(12) :: month_names  ! names of the months
00052     
00053     integer(kind=long_kind) :: sec_per_day   ! # seconds in a day 
00054     integer(kind=long_kind) :: days_per_year ! number of days in the current year
00055     
00056     
00057     ! time variables
00058     integer(kind=long_kind) :: year          ! current year in the simulation
00059     integer(kind=int_kind)  :: month         ! current month in the simulation
00060     real(kind=real_kind)    :: hour          ! current hour of day in the simulation
00061     integer(kind=long_kind) :: day           ! current day of the current month 
00062     integer(kind=long_kind) :: doy           ! current day of current year
00063     real(kind=real_kind)    :: real_doy      ! (day) time from first of current year
00064     integer(kind=long_kind) :: sec_day       ! current second in the current day
00065     integer(kind=long_kind) :: sec_year      ! current second in the current year
00066     integer(kind=long_kind) :: sec_tot       ! current second in the whole simulation
00067     
00068     integer(kind=long_kind) :: pyear         ! year of previous month of simulation
00069     integer(kind=long_kind) :: pmonth        ! previous month of simulation
00070     integer(kind=long_kind) :: ppyear        ! year of two months previous of simulation
00071     integer(kind=long_kind) :: ppmonth       ! two months previous of simultion
00072     
00073     integer(kind=long_kind) :: nyear         ! year of next month of simulation
00074     integer(kind=long_kind) :: nmonth        ! next month of simulation
00075     
00076     integer(kind=long_kind) :: driver_times   ! # driver data timesteps per day
00077     integer(kind=int_kind) :: driver_recnum ! record # of data in driver data file
00078     integer(kind=long_kind) :: driver_month  ! month of driver data to read
00079     integer(kind=long_kind) :: driver_year   ! year of driver data to read
00080     integer(kind=int_kind) :: driver_hour   ! hour of driver data to be read
00081     integer(kind=long_kind) :: driver_day    ! day of driver data to be read
00082     
00083     real(kind=dbl_kind) :: start_period     ! start of averaged period
00084     real(kind=dbl_kind) :: end_period       ! end of averaged period
00085     real(kind=dbl_kind) :: period_length    ! length of averaged period
00086     
00087     ! I/O time variables
00088     integer(kind=long_kind) :: driver_step   ! # seconds in driver data timestep
00089     integer(kind=long_kind) :: bc_step       ! # seconds between bc file switching
00090     integer(kind=int_kind) :: bc_recnum      ! record # of data timew dependent parameters
00091 
00092 !itb_modis...
00093 !    real(kind=real_kind), dimension(npermax) :: ndvi_start ! (doy) start times ndvi composite periods
00094 !    real(kind=real_kind), dimension(npermax) :: ndvi_stop  ! (doy) start times ndvi composite periods
00095 
00096     real(kind=real_kind), dimension(npermax) :: modis_start ! (doy) start times modis composite periods
00097     real(kind=real_kind), dimension(npermax) :: modis_stop  ! (doy) start times modis composite periods
00098 !itb_modis
00099 
00100 
00101     integer(kind=long_kind) :: restart_step  ! # seconds in restart file timestep
00102     integer(kind=long_kind) :: qp_step       ! # seconds in qp file timestep
00103     real(kind=dbl_kind)    :: qp_incnt      ! # to multiply qp's by to get average
00104     integer(kind=long_kind) :: qp_count      ! # to divide qp's by to get average
00105     integer(kind=long_kind) :: pbp_step      ! # seconds in pbp file timestep
00106     real(kind=dbl_kind)   :: pbp_incnt     ! # to multiply pbp's by to get average
00107     integer(kind=long_kind) :: pbp_count     ! # to divide pbp's by to get average
00108     integer(kind=long_kind) :: pbp_offset    ! # seconds to offset pbp writing
00109     
00110     
00111     ! Flags
00112     logical(kind=log_kind) :: write_qp      ! write data to qp files ?
00113     logical(kind=log_kind) :: switch_qp     ! switch qp files ?
00114     logical(kind=log_kind) :: write_pbp     ! write data to pbp files ?
00115     logical(kind=log_kind) :: switch_pbp    ! switch pbp files ?
00116     logical(kind=log_kind) :: read_driver   ! read driver data ?
00117     logical(kind=log_kind) :: switch_driver ! switch driver data file ?
00118     logical(kind=log_kind) :: read_bc       ! read boundary condition data ?
00119     logical(kind=log_kind) :: switch_bc     ! switch bc file ?
00120     logical(kind=log_kind) :: write_restart ! write restart file ?
00121     logical(kind=log_kind) :: interp_bc     ! interpolate boundary condition data ?
00122     logical(kind=log_kind) :: interp_driver ! interpolate driver data ?
00123     logical(kind=log_kind) :: new_day       ! new day ?  (solar dec and bc interp)
00124     logical(kind=log_kind) :: calc_respf    ! calculate new respfactor
00125     logical(kind=log_kind) :: write_respf   ! write respfactor file
00126 
00127 end type time_struct
00128 
00129 end module timetype
00001 subroutine time_init( time )
00002 
00003 use timetype
00004 use sib_const_module
00005 use sib_io_module
00006 implicit none
00007 
00008 ! parameters
00009 type(time_struct), intent(out) :: time
00010 
00011 ! local variables
00012 integer :: x
00013 integer i,j,k,l,m,n   ! indeces
00014 
00015     ! set some non-varying values
00016     time%days_per_year = 365
00017     time%sec_per_day = 86400
00018     time%mid_month(:) =      &
00019        (/15.5,45.,74.5,105.,135.5,166.,196.5,227.5,258.,288.5,319.,349.5/)
00020     time%days_per_month(:) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
00021     time%doy1_month(:) = (/1,32,60,91,121,152,182,213,244,274,305,335/)
00022     time%month_names(:) = (/'January   ',  &
00023                             'February  ',  &
00024                             'March     ',  &
00025                             'April     ',  &
00026                             'May       ',  &
00027                             'June      ',  &
00028                             'July      ',  &
00029                             'August    ',  &
00030                             'September ',  &
00031                             'October   ',  &
00032                             'November  ',  &
00033                             'December  '/)
00034 
00035     ! if starttime > 0, then ntinital already in units of seconds
00036     ! if starttime < 0, convert days to seconds 
00037     if ( starttime < 0 ) starttime = (-starttime-1)*time%sec_per_day
00038 
00039     ! if endtime > 0, then endtime already in units of seconds
00040     ! if endtime < 0, convert days to seconds 
00041     if ( endtime < 0 ) endtime = (-endtime)*time%sec_per_day
00042 
00043     ! make sure endtime doesn't occur before starttime
00044     if ( endyear == startyear ) then
00045       if ((starttime >= endtime) )  &
00046           stop 'simulation ends before it starts, check starttime,'  &
00047               //' endtime, startyear, and endyear'
00048 
00049     endif
00050     
00051     ! make sure endyear doesn't occur before startyear
00052     if ( endyear < startyear )  &
00053         stop 'simulation ends before it starts, check startyear and endyear'
00054     
00055     ! make sure number of seconds in simulation is evenly divisible by dtsib
00056     if ( mod( endtime-starttime, dtsib) /= 0 )  &
00057         stop 'dtsib does not divide evenly into the total simulation'
00058 
00059     ! make sure dtsib divides evenly into a day
00060     if (  mod( time%sec_per_day, dtsib ) /= 0 )  &
00061         stop 'dtsib does not divide evenly into a day'
00062         
00063     ! make sure driver data timestep divides evenly into a day
00064     if ( mod( time%sec_per_day, dtsibmetin) /= 0 )  &
00065         stop 'dtsibmetin does not divide evenly into a day'
00066     
00067     ! make sure dtsibout is evenly divisible by dtsib
00068     if ( dtsibout > 0 .and. mod( dtsibout, dtsib ) /= 0 )  &
00069         stop 'dtsib does not divide evenly into dtsibout'
00070     
00071     ! make sure dtsibres is evenly divisible by dtsib
00072     if ( dtsibres > 0 .and. mod( dtsibres, dtsib ) /= 0 )  &
00073         stop 'dtsib does not divide evenly into dtsibres'
00074     
00075     ! make sure ndtsibpbp is positive
00076     if ( ndtsibpbp < 0 ) stop 'ndtsibpbp must be >= 0'
00077     
00078     
00079     ! set initial values
00080     time%init_year = startyear
00081     time%init_doy = starttime / time%sec_per_day + 1
00082     time%init_second = starttime
00083     do x = 1, 12
00084         if ( time%init_doy >= time%doy1_month(x) ) then
00085             time%init_month = x
00086             time%init_day = time%init_doy - time%doy1_month(x) + 1
00087         endif
00088     enddo
00089     
00090     !   else set them to init_* values
00091         time%start_year = time%init_year
00092         time%start_month = time%init_month
00093         time%start_day = time%init_day
00094         time%start_doy = time%init_doy
00095         time%start_second = time%init_second
00096 
00097     
00098     ! set ending values
00099     time%end_year = endyear
00100     ! end_second must be calculated incrementaly to avoid overflow that
00101     !  occurs when there are >= 58 years of simulation
00102     time%end_second = time%days_per_year * time%sec_per_day
00103     time%end_second = time%end_second * (time%end_year - time%start_year)
00104     time%end_second = endtime + time%end_second
00105     time%end_doy = endtime / time%sec_per_day + 1
00106     do x = 1, 12
00107         if ( time%end_doy >= time%doy1_month(x) ) then
00108             time%end_month = x
00109             time%end_day = time%end_doy - time%doy1_month(x) + 1
00110         endif
00111     enddo
00112     
00113     ! set values from namel to variables in structure
00114     time%dtsib = dtsib
00115     time%driver_step = dtsibmetin
00116     time%bc_step = dtsibbcin
00117     time%restart_step = dtsibres
00118     time%qp_step = dtsibout
00119     time%pbp_step = time%dtsib * ndtsibpbp
00120     
00121     ! set current times to get ready for simulation
00122     time%year = time%start_year
00123     time%month = time%start_month
00124     time%doy = time%start_doy
00125     time%day = time%start_day
00126     time%hour = time%sec_day/3600.
00127     time%sec_day = 0 
00128     time%sec_year = time%start_second
00129     time%real_doy =real(time%sec_year)/real(time%sec_per_day)
00130     time%sec_tot = time%start_second
00131 
00132    
00133     ! set previous times for boundary condition data
00134     time%pmonth = time%start_month - 1
00135     if ( time%pmonth == 0 ) time%pmonth = 12
00136     time%pyear = time%year
00137     if ( time%pmonth > time%start_month ) time%pyear = time%year - 1
00138     time%ppmonth = time%pmonth - 1
00139     if ( time%ppmonth == 0 ) time%ppmonth = 12
00140     time%ppyear = time%pyear
00141     if ( time%ppmonth > time%pmonth ) time%ppyear = time%pyear - 1
00142     
00143     ! set next times for boundary condition data
00144     time%nmonth = time%month
00145     time%nyear = time%year
00146 
00147     ! set pbp averaging variable
00148     time%pbp_incnt = 1
00149     time%qp_incnt = 1
00150 
00151     ! set some driver data timing variables
00152     time%driver_times = time%sec_per_day/time%driver_step
00153     time%driver_month = time%month
00154     time%driver_year = time%year
00155     time%driver_recnum = (time%day-1) * time%driver_times + 1
00156     time%driver_hour = int( time%hour )
00157     time%driver_day = time%day
00158     time%start_period = (time%start_year-1) * time%days_per_year +  &
00159         real(time%start_doy-1) + real(time%start_second) / real(time%sec_per_day)
00160     
00161     ! initialize flags
00162     time%write_qp = .false.
00163     time%switch_qp = .true.
00164     time%pbp_offset = 0
00165     time%write_pbp = .false.
00166     time%pbp_count = 0    
00167     time%qp_count = 1
00168     time%switch_pbp = .true.
00169     time%read_driver = .true.
00170 !CSR    time%read_bc = .false.
00171     time%read_bc = .true.
00172     time%switch_bc = .false.
00173     time%write_restart = .false.
00174     time%interp_bc = .true.
00175     time%interp_driver = .true.
00176     time%switch_driver = .true.
00177     time%new_day = .true.
00178     time%calc_respf = .false.
00179     time%write_respf = .false.
00180     
00181 
00182 
00183 
00184 end subroutine time_init
00185 
00186 
00187 !-------------------------------------------------------------------------------
00188 subroutine time_check( time )
00189 !-------------------------------------------------------------------------------
00190 
00191 use timetype
00192 use sib_io_module
00193 implicit none
00194 
00195 ! parameters
00196 type(time_struct), intent(inout) :: time 
00197 
00198 ! local variables
00199     
00200 
00201     time%switch_driver = .false.
00202 
00203 
00204     if (drvr_type == 'single' ) then
00205             time%driver_recnum = 1
00206     else
00207         time%driver_recnum =  time%driver_recnum + 1
00208     endif
00209 
00210 !itb_ncep_single
00211 !    time%driver_recnum =  time%driver_recnum + 1
00212 
00213 
00214 
00215     time%driver_hour = int( time%hour + time%driver_step/3600. )
00216     if ( time%driver_hour > 24 )  &
00217         time%driver_day = mod(time%driver_day,time%days_per_month(time%month))+1
00218     time%driver_hour = mod( time%driver_hour, 24 )
00219     if ( time%driver_recnum >  &
00220             time%days_per_month(time%month) * time%driver_times ) then
00221          
00222         time%driver_recnum = 1
00223         time%driver_month = time%nmonth
00224         time%driver_year = time%nyear   
00225         time%switch_driver = .true.
00226     endif
00227     
00228 
00229 end subroutine time_check
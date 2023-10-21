00001 subroutine time_manager( time, drvr_type, roll_respf )
00002 !----------------------------------------------------------------------
00003 ! calculates all time related variables and sets all operations control flags
00004 !
00005 ! Modifications:
00006 !  Kevin Schaefer shifted read driver back 1 time step (8/17/04)
00007 !  Kevin Schaefer shifted switch driver data back 1 time step (8/19/04)
00008 !  Kevin Schaefer corrected driver month/year calculation (8/19/04)
00009 
00010 use kinds
00011 use timetype
00012 implicit none
00013 
00014 ! parameters
00015 type(time_struct), intent(inout) :: time
00016 character (len=8), intent(in) :: drvr_type 
00017 logical(kind=log_kind), intent(in) :: roll_respf
00018 
00019 ! local variables
00020 integer :: x
00021 integer :: low_bound
00022 integer :: high_bound
00023 
00024 
00025     !---------------------------------------------------------------------------
00026     ! TIME VARIABLES
00027     !---------------------------------------------------------------------------
00028 
00029     ! update time variables
00030     time%sec_tot = time%sec_tot + time%dtsib
00031     time%sec_year = time%sec_year + time%dtsib
00032     time%real_doy =real(time%sec_year)/real(time%sec_per_day)
00033     time%sec_day = time%sec_day+time%dtsib
00034     if(mod(time%sec_year,time%sec_per_day)==time%dtsib .and. &
00035        time%sec_year>time%init_second+time%dtsib)then
00036            time%doy=time%doy+1
00037            time%sec_day =time%dtsib
00038     endif
00039     do x = 1, 12
00040         if ( time%doy >= time%doy1_month(x) .and. time%sec_day > 0) then
00041             time%month = x
00042             time%day = time%doy - time%doy1_month(x) + 1
00043         endif
00044     enddo
00045     time%hour = mod( time%sec_day, time%sec_per_day ) / 3600.
00046     if( time%hour == 0)time%hour =24.0
00047     if (time%sec_year == time%dtsib+(time%sec_per_day*time%days_per_year))  then
00048        time%year = time%year + 1
00049        time%month = 1
00050        time%doy = 1
00051        time%day = 1
00052        time%sec_day = time%dtsib
00053        time%sec_year = time%dtsib
00054        time%real_doy =real(time%sec_year)/real(time%sec_per_day)
00055     endif
00056     time%nmonth = time%month + 1
00057     if ( time%nmonth > 12 ) then
00058        time%nmonth = 1
00059        time%nyear = time%year + 1
00060     endif
00061     !---------------------------------------------------------------------------
00062     ! DRIVER DATA
00063     !---------------------------------------------------------------------------
00064     
00065     ! check to see if it is time to read Driver Data
00066     if ( mod( time%sec_tot, time%driver_step ) == 0  &
00067         .and. time%sec_tot > time%init_second + time%dtsib ) then
00068         time%read_driver = .true.
00069 
00070         if ( drvr_type == 'single' ) then
00071             time%driver_recnum = 1
00072         else
00073             time%driver_recnum =  time%driver_recnum + 1
00074         endif
00075 
00076 !itb_ncep_single
00077 !        time%driver_recnum =  time%driver_recnum + 1
00078 
00079 
00080         time%driver_hour = int( time%hour + time%driver_step/3600. )
00081         if ( time%driver_hour >= 24 ) then
00082             time%driver_day =  &
00083                 mod( time%driver_day, time%days_per_month(time%month) ) + 1
00084         endif
00085         time%driver_hour = mod( time%driver_hour, 24 )
00086     else
00087         time%read_driver = .false.
00088     endif
00089     
00090     ! check to see if it is time to switch Driver Data files
00091     if ( time%day == time%days_per_month(time%month) .and.  &
00092         time%sec_day == time%sec_per_day - time%driver_step ) then
00093         
00094         time%switch_driver = .true.
00095         time%driver_recnum = 1
00096         time%driver_month = time%driver_month+1
00097         !
00098         ! KS check if at end of year
00099         if(time%driver_month>12) then
00100            time%driver_month=1
00101            time%driver_year=time%driver_year+1
00102         endif
00103     else
00104         time%switch_driver = .false.
00105     endif
00106     
00107 
00108     !---------------------------------------------------------------------------
00109     ! time dependent parameter data
00110     !---------------------------------------------------------------------------
00111 
00112     ! check to see if it is time to read parameter data
00113     time%read_bc = .false.
00114 !itb_modis
00115 !    if ( time%real_doy >= time%ndvi_start(time%bc_recnum) ) then
00116 !        time%read_bc = .true.
00117 !        if( time%real_doy >= time%ndvi_start(nper).and.time%bc_recnum==1) time%read_bc = .false.
00118 !    endif
00119     if ( time%real_doy >= time%modis_start(time%bc_recnum) ) then
00120         time%read_bc = .true.
00121         if( time%real_doy >= time%modis_start(nper).and.time%bc_recnum==1) time%read_bc = .false.
00122     endif
00123 !itb_modis
00124 
00125     if(time%read_bc) time%bc_recnum=time%bc_recnum+1
00126     if(time%read_bc) then
00127        print*, 'read_bc: Yes!',time%real_doy,time%bc_recnum
00128        print*, 'read_bc:',time%bc_recnum,nper
00129     endif
00130     ! check to see if it is time to switch parameter data files
00131     time%switch_bc = .false.
00132     if ( time%read_bc .and. time%bc_recnum>nper ) then
00133         time%switch_bc = .true.
00134         time%bc_recnum=1
00135         print*, 'switch_bc',time%real_doy,time%bc_recnum
00136     endif
00137 
00138 
00139     !---------------------------------------------------------------------------
00140     ! RESTART FILES
00141     !---------------------------------------------------------------------------
00142 
00143     ! check to see if it is time to write restart file
00144     if ( time%restart_step > 0 ) then
00145         ! time%restart_step units = seconds
00146         if ( mod( time%sec_year, time%restart_step ) == 0 ) then
00147             time%write_restart = .true.
00148         else
00149             time%write_restart = .false.
00150         endif
00151     else
00152         ! time%restart_step units = months
00153         if (time%sec_year == (time%doy1_month(time%nmonth)-1)*time%sec_per_day .or. &
00154         (time%sec_year == (365)*time%sec_per_day)) then
00155             time%write_restart = .true.
00156         else
00157             time%write_restart = .false.
00158         endif
00159     endif
00160     
00161 
00162     !---------------------------------------------------------------------------
00163     ! QP FILES
00164     !---------------------------------------------------------------------------
00165 
00166     ! check to see if it is time to write data to qp files
00167     time%qp_count = time%qp_count + 1
00168      if ( time%qp_step > 0 ) then
00169          ! time%qp_step units = seconds
00170          if ( mod( time%sec_year, time%qp_step ) == 0 ) then
00171              time%write_qp = .true.
00172              time%qp_incnt = 1. / real(time%qp_count)
00173              time%qp_count = 0
00174              time%end_period = real((time%year-1) * time%days_per_year) +  
00175                  real(time%sec_year)/real(time%sec_per_day)
00176              time%period_length = time%end_period - time%start_period
00177              time%start_period = time%end_period
00178          else
00179              time%write_qp = .false.
00180          endif
00181      else
00182         ! time%qp_step units = months
00183         if ( time%sec_year==(time%doy1_month(time%nmonth)-1)*86400 .or. &
00184             time%sec_year == time%sec_per_day*time%days_per_year) then
00185             
00186             time%write_qp = .true.
00187             time%qp_incnt = 1. /  real(time%qp_count)
00188             time%qp_count = 0
00189             time%end_period = real((time%year-1) * time%days_per_year) +  
00190                 real(time%sec_year+time%dtsib)/real(time%sec_per_day)
00191             time%period_length = time%end_period - time%start_period
00192             time%start_period = time%end_period
00193         else
00194             time%write_qp = .false.
00195         endif
00196      endif
00197     
00198     ! check to see if it is time to switch qp files
00199     if ( time%day == 1 .and. time%sec_day == time%dtsib) then
00200         time%switch_qp = .true.
00201     else
00202         time%switch_qp = .false.
00203     endif
00204 
00205 
00206     !---------------------------------------------------------------------------
00207     ! PBP FILES
00208     !---------------------------------------------------------------------------
00209 
00210     ! check to see if it is time to write data to pbp files
00211     if ( time%pbp_step > 0 ) then
00212 
00213         ! set pbp averaging variable
00214         time%pbp_count = time%pbp_count + 1
00215 
00216         if ( mod( time%sec_tot, time%pbp_step ) == time%pbp_offset ) then
00217             time%write_pbp = .true.
00218             time%pbp_incnt = 1. / real(time%pbp_count)
00219             time%pbp_count = 0
00220         else
00221             time%write_pbp = .false.
00222         endif
00223         
00224         if ( time%day == 1 .and. time%sec_day == time%dtsib) then
00225             time%switch_pbp = .true.
00226         else
00227             time%switch_pbp = .false.
00228         endif
00229     else
00230         ! do not write pbp data at all
00231         time%write_pbp = .false.
00232         time%switch_pbp = .false.
00233     endif
00234     
00235     
00236     !---------------------------------------------------------------------------
00237     ! INTERPOLATIONS
00238     !---------------------------------------------------------------------------
00239     
00240     if ( time%sec_day == time%dtsib ) then
00241         time%new_day = .true.
00242     else
00243         time%new_day = .false.
00244     endif
00245     
00246     
00247     !---------------------------------------------------------------------------
00248     ! RESPFACTOR
00249     !---------------------------------------------------------------------------
00250     if ( roll_respf ) then
00251         ! rolling respfactor
00252         !   calculate last timestep of the month
00253         if ( time%day == time%days_per_month(time%month) .and.   &
00254              time%sec_day + time%dtsib == time%sec_per_day ) then
00255         
00256             time%calc_respf = .true.
00257             time%write_respf = .true.
00258         else
00259             time%calc_respf = .false.
00260             time%write_respf = .false.
00261         endif
00262     else
00263         ! traditional respfactor
00264         !   calculate last timestep of a complete year
00265         if ( time%month == 12 .and.   &
00266              time%doy == 365  .and.   &
00267              time%sec_day + time%dtsib == time%sec_per_day .and.   &
00268              time%sec_year + time%dtsib ==   &
00269                 time%sec_per_day * time%days_per_year ) then
00270              
00271             time%calc_respf = .true.
00272             time%write_respf = .true.
00273         else
00274             time%calc_respf = .false.
00275             time%write_respf = .false.
00276         endif
00277     endif
00278     
00279 
00280 end subroutine time_manager
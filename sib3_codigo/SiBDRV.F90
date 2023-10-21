00001 
00002 
00003 
00004 
00005 
00006 
00007 program sibdrive
00008 
00009 use kinds
00010 use timetype
00011 use sib_const_module
00012 use sib_io_module
00013 use sibtype
00014 implicit none
00015 
00016 ! local variables
00017 type(sib_t), dimension(:), allocatable :: sib
00018 type(time_struct) :: time
00019 integer(kind=long_kind) :: t
00020 integer(kind=int_kind) :: i,j
00021 integer(kind=int_kind) :: rank
00022 integer(kind=int_kind) :: nchunks
00023 integer, external :: iargc
00024 real(kind=dbl_kind) del_store
00025 real(kind=dbl_kind) sum_flux
00026 real(kind=dbl_kind) residual
00027 character*100 filename  ! filename used to read in ndvi data
00028 
00029 !variables for timing
00030  real etime          ! Declare the type of etime()
00031  real elapsed(2)     ! For receiving user and system time
00032  real total          ! For receiving total time
00033 
00034 
00035 !itb------------------------------------------------------------------
00036 !itb...TRANSCOM HARDWIRE..................................................
00037 
00038 !real(kind=dbl_kind), dimension(:), allocatable :: temp1
00039 !real(kind=dbl_kind), dimension(:), allocatable :: temp2
00040 
00041 !character(len=2)  :: cmon
00042 !character(len=4)  :: cyear
00043 !character(len=50) :: fname1
00044 !character(len=50)  :: fname2
00045 
00046 !itb------------------------------------------------------------------
00047 
00048 
00049 
00050 
00051       
00052 ! command line variables
00053 character(len=4) :: one, two
00054 character(len=4) :: dfdfd
00055 
00056     ! read in parallelization values from command line
00057     call getarg( 1, one )
00058     if ( one == '' .or. one == '>' ) then
00059         rank = 1
00060         nchunks = 1
00061     else
00062         call getarg( 2, two )
00063         if ( two == '' .or. two == '>' ) then
00064             stop 'Command line arguments incorrect:  SiBD3 rank nchunks'
00065         else
00066             read( one, * ) rank
00067             read( two, * ) nchunks
00068             if ( rank > nchunks ) stop 'nchunks greater than rank'
00069             if ( rank < 1 .or. nchunks < 1 ) stop 'rank or nchunks < 1'
00070         endif
00071     endif
00072 
00073 
00074 
00075 
00076     ! read in namel_sibdrv
00077     call init_grid( rank, nchunks )
00078     
00079     ! allocate sib structure
00080     allocate( sib(subcount) )
00081 
00082 !itb------------------------------------------------------------------
00083 !itb...TRANSCOM HARDWIRE..................................................
00084 
00085 
00086 
00087 !itb------------------------------------------------------------------
00088 
00089 
00090     
00091     call init_var(sib)
00092     
00093     ! initialize all values and prepare for timestep loop
00094     call init_sibdrv( sib, time )
00095 
00096 
00097 !itb------------------------------------------------------------------
00098 !itb...TRANSCOM HARDWIRE..................................................
00099 !itb...HARDWIRE IN FILE NAME(S) FOR OUTPUT...
00100 !itb...
00101 !itb...OUTPUT VARS ARE ASSIMN AND RESPG
00102 
00103 !    write(cyear,'(i4.4)')time%year
00104 
00105 !    fname1 = './output_hourly/assim_'//cyear//'01.dat'
00106 !    fname2 = './output_hourly/resp_'//cyear//'01.dat'
00107 
00108 !    open(unit=198,file=fname1,form='unformatted',    &
00109 !         convert='LITTLE_ENDIAN',status='unknown')
00110 
00111 !    open(unit=199,file=fname2,form='unformatted',      &
00112 !         convert='LITTLE_ENDIAN',status='unknown')
00113 
00114 !    allocate(temp1(subcount))
00115 !    allocate(temp2(subcount))
00116 
00117 !    temp1(:) = 0.0
00118 !    temp2(:) = 0.0
00119 
00120 !itb------------------------------------------------------------------
00121 
00122 
00123 
00124 
00125     
00126     ! set time variables for initial time step
00127     call time_check( time)
00128       
00129     ! call output_control
00130     call output_control( sib, time, rank )
00131    
00132     ! timestep loop
00133     do t = time%start_second, time%end_second - time%dtsib, time%dtsib
00134      
00135         ! print out date information once a day
00136         if ( time%sec_day == time%dtsib ) then
00137           print*, time%month_names(time%month),time%day,time%year   
00138 
00139         endif
00140         
00141 
00142         ! calculate solar declination
00143         if ( time%new_day ) call solar_dec( time )
00144 
00145         ! read in driver data needed
00146         if ( time%read_driver ) then
00147             if ( drvr_type == 'ecmwf' ) then
00148                 call sibdrv_read_ecmwf( sib, time )
00149             elseif ( drvr_type == 'ncep1' ) then
00150                 call sibdrv_read_ncep1( sib, time )
00151             elseif ( drvr_type == 'ncep2' ) then
00152                 call sibdrv_read_ncep2( sib, time )
00153             elseif ( drvr_type == 'geos4' ) then
00154                 call sibdrv_read_geos4( sib, time )
00155             elseif ( drvr_type == 'single' ) then
00156                 call sibdrv_read_single( sib, time )
00157             elseif ( drvr_type == 'ncp_sngl' ) then
00158                 call sibdrv_read_ncep2_single( sib, time )
00159             
00160 
00161             else
00162                 stop 'Invalid drvr_type specified'
00163             endif
00164             
00165             ! calculate mean cosine zenith angle
00166             call mean_zenith_angle( sib, time )
00167         endif
00168  
00169   if(time%switch_bc .OR. time%read_bc) then
00170        print*,time%sec_day,time%switch_bc,time%read_bc
00171   endif
00172 
00173 
00174         ! switch parameter files (if required)
00175         if ( time%switch_bc ) then
00176           if ( drvr_type == 'single' .OR. drvr_type == 'ncp_sngl') then
00177             write (filename, "(a,i4)") trim(param_path), time%year+1
00178             call open_single_td( sib, time, filename)
00179           else
00180             write (filename, "(a,i4,a3)") trim(param_path), time%year+1, '.nc'
00181 !            print*,'SiBDRV: open_global_td'
00182             call open_global_td( sib, time, filename)
00183           endif
00184           print*, 'switch parameter file to ', trim(filename)
00185         endif
00186 
00187         ! read parameter data (if required)
00188         if ( time%read_bc ) then
00189           if ( drvr_type == 'single'.OR. drvr_type == 'ncp_sngl' ) then
00190 !            call new_bc( sib, time )
00191             call read_single_td_param(sib,time)
00192           else
00193 
00194             call read_global_td_param(sib,time)
00195           endif 
00196         endif
00197 
00198         ! determine if td parameters need to be rotated between the 3 positions
00199         if (t > time%start_second + time%dtsib) call need_to_switch(sib,time)           
00200 
00201         ! interpolate parameter variables once a day      
00202         if ( time%new_day ) call bc_interp( sib, time )
00203 !itb        if ( time%new_day ) call bc_interp_old( sib, time )
00204 
00205         !interpolate driver data each time step
00206         call sibdrv_interp( sib, time )
00207 
00208         ! call sib_main()
00209         dtt = time%dtsib
00210         dti = 1./dtt
00211         tau = time%sec_year
00212         !$OMP DO
00213 
00214         do i = 1, subcount
00215 !print*,'call sib:',sib(i)%param%zlt,sib(i)%param%aparc
00216 !itb_pheno...patch: a very small number of pixels have LAI=0. this crashes things...
00217            if (sib(i)%param%zlt == 0.0 .OR. sib(i)%param%aparc == 0.0) then
00218               sib(i)%param%zlt    = 0.1
00219               sib(i)%param%vcover = 0.1
00220               sib(i)%param%aparc  = 0.1
00221               sib(i)%param%green  = 0.1
00222               sib(i)%param%z0d    = 0.1
00223            endif
00224 
00225             call sib_main( sib(i) )
00226 
00227 
00228 !itb------------------------------------------------------------------
00229 !itb...TRANSCOM HARDWIRE..................................................
00230 
00231 !            temp1(i) = temp1(i) + sib(i)%diag%assim(6) * 1.0e6
00232 !            temp2(i) = temp2(i) + sib(i)%diag%resp_tot * 1.0e6
00233 
00234 !            print*,'i=',i,' t1=',temp1(i),' t2=',temp2(i)
00235 
00236 !itb------------------------------------------------------------------
00237 
00238 
00239 
00240 
00241         enddo
00242         !$OMP END DO
00243 
00244         ! call respfactor_control
00245         call respfactor_control( sib, time, rank )
00246 
00247         ! call time_manager()
00248         call time_manager( time, drvr_type, roll_respf )
00249         sib(:)%stat%julday = time%doy 
00250 
00251 
00252 !itb------------------------------------------------------------------
00253 !itb...TRANSCOM HARDWIRE..................................................
00254 
00255 
00256 !itb        ! call output_control
00257         call output_control( sib, time, rank )
00258 
00259 !            if(time%write_qp) then
00260 !              temp1(:) = temp1(:) / 6.0
00261 !              temp2(:) = temp2(:) / 6.0
00262 
00263 !              write(198) temp1
00264 !              write(199) temp2
00265 
00266 !              temp1(:) = 0.0
00267 !              temp2(:) = 0.0
00268 !            endif
00269 
00270 !itb------------------------------------------------------------------
00271 
00272 
00273 
00274 !itb_iso...check for time to calculate the del 13C of autotrophic respiration. The
00275 !itb_iso...integration period is set in sib_const_module.
00276 
00277         if ( time%write_restart ) then      ! switch at restart time period (monthly)
00278 !        if ( mod(d13c_auto_switch,time%total_days) == 0) then     ! switch at user-defined period
00279 
00280           do i= 1,subcount
00281             phys_loop_d13c : do j=1,5
00282               if ( sib(i)%param%physfrac(j) == 0.0 ) then
00283                  sib(i)%param%d13c_auto(j) = 0.0
00284                  cycle phys_loop_d13c
00285               else
00286                   sib(i)%param%d13c_auto(j) = sib(i)%param%d13c_psn(j) / sib(i)%param%psn_accum(j)
00287               endif
00288             enddo phys_loop_d13c
00289 
00290 !itb_iso...reset
00291             do j = 1,physmax
00292               sib(i)%param%d13c_psn(j)  = 0.0_dbl_kind
00293               sib(i)%param%psn_accum(j) = 0.0_dbl_kind
00294             enddo
00295 
00296           enddo
00297         endif
00298 !itb_iso 
00299 
00300 
00301         ! Write restart
00302         if ( time%write_restart ) then
00303 
00304             call rtape_sib( sib, time, rank )
00305 
00306 
00307 !itb------------------------------------------------------------------
00308 !itb...TRANSCOM HARDWIRE..................................................
00309 
00310 !    print*,'closing file:',fname1
00311 !    print*,'closing file:',fname2
00312 
00313 
00314 !    close(198)
00315 !    close(199)
00316 
00317 !    write(cmon,'(i2.2)')time%month+1
00318 
00319 !    fname1 = './output_hourly/assim_'//cyear//cmon//'.dat'
00320 !    fname2 = './output_hourly/resp_'//cyear//cmon//'.dat'
00321 
00322 !    print*,'opening file:',fname1
00323 !    print*,'opening file:',fname2
00324 
00325 !    open(unit=198,file=fname1,form='unformatted',    &
00326 !         convert='LITTLE_ENDIAN',status='unknown')
00327 !    open(unit=199,file=fname2,form='unformatted',      &
00328 !         convert='LITTLE_ENDIAN',status='unknown')
00329 
00330 
00331 !itb------------------------------------------------------------------
00332 
00333 
00334         endif
00335 
00336     ! cas CO2 conservation
00337         del_store=sib(1)%prog%cas-sib(1)%prog%cas_old
00338         sum_flux=sib(1)%diag%resp_grnd*dtt
00339         sum_flux=sum_flux-sib(1)%diag%assimn(6)*dtt
00340         sum_flux=sum_flux-sib(1)%diag%cflux*dtt
00341         residual=del_store-sum_flux-sib(1)%prog%expand
00342 
00343     enddo
00344 
00345 
00346     ! make sure all files have been closed
00347     call file_closer
00348     !
00349     ! print message
00350     print*, 'end simulation'
00351       total = etime(elapsed)
00352     print *, 'End: total=', total, ' user=', elapsed(1),' system=', elapsed(2)
00353 
00354 end program sibdrive
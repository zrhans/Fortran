00001 
00002 !
00003 !-------------------------------------------------------------------------------
00004 subroutine open_single_td( sib, time, filename)
00005 !-------------------------------------------------------------------------------
00006 ! opens single point ascii file of time dependent parameters
00007 ! and reads in times for ndvi composite periods
00008 !
00009 ! Modifications:
00010 !   Kevin Schaefer created routine (8/1/05)
00011 !-------------------------------------------------------------------------------
00012 !
00013 use kinds
00014 use sibtype
00015 use timetype
00016 use sib_io_module
00017 use sib_const_module
00018 use sib_bc_module
00019 !
00020 implicit none
00021 !
00022 ! input/output variables
00023 type(sib_t), dimension(subcount), intent(inout) :: sib
00024 type(time_struct) :: time
00025 !
00026 ! local variables
00027 integer(kind=int_kind) :: ntest1    ! compare nsib value of file to simulation
00028 integer(kind=int_kind) :: i,j,k,l,m,n  ! indeces
00029 character*100 filename  ! filename used to read in ndvi data
00030 !
00031 ! close time dependent parameter file
00032     close(32)
00033 !
00034 ! open time dependent parameter file
00035     open(32, file=trim(filename), form='formatted', status='old') 
00036 !
00037 ! read number of sib points  
00038     read(32,*) ntest1
00039     if(ntest1/=nsib)  stop ' param file nsib no match with model nsib'
00040 !
00041 ! read number composite periods
00042     read(32,*) nper
00043     if(nper>npermax)  stop ' too many composite periods in param file'
00044 !
00045 ! read composite period start and stop times
00046     read(32,*)
00047     do i = 1,nper
00048 
00049 !itb_modis
00050 !      read(32,*) time%ndvi_start(i), time%ndvi_stop(i), j
00051       read(32,*) time%modis_start(i), time%modis_stop(i), j
00052     enddo
00053 !
00054 ! read header line before time dep parameters
00055     read(32,*)
00056 !
00057 end subroutine open_single_td
00058 !
00059 !-------------------------------------------------------------------------------
00060 subroutine read_single_td_param(sib, time)
00061 !-------------------------------------------------------------------------------
00062 ! reads in time dependent parameters (ndvi, etc.) for single point from ascii file
00063 !
00064 ! Modifications:
00065 !   Kevin Schaefer created routine (8/1/05)
00066 !-------------------------------------------------------------------------------
00067 !
00068 use kinds
00069 use sibtype
00070 use timetype
00071 use sib_io_module
00072 use sib_const_module
00073 use sib_bc_module
00074 !
00075 implicit none
00076 !
00077 ! input/output variables
00078 type(sib_t), dimension(subcount), intent(inout) :: sib
00079 type(time_struct), intent(in) :: time
00080 !
00081 ! read in time dependent parameters (assume file at time%bc_recnum)
00082     read(32,*)
00083 !itb_modis
00084 !    read(32,*) sib%param%ndvi3
00085 !    read(32,*) sib%param%NDVI_time3
00086     read(32,*) sib%param%mlai3
00087     read(32,*) sib%param%mfpar3
00088     read(32,*) sib%param%modis_time3
00089 !itb_modis
00090     read(32,*) sib%param%d13cresp3
00091     read(32,*) sib%param%physfrac3(1)
00092     read(32,*) sib%param%physfrac3(2)
00093     read(32,*) sib%param%physfrac3(3)
00094     read(32,*) sib%param%physfrac3(4)
00095     read(32,*) sib%param%physfrac3(5)
00096     
00097 !itb_modis
00098 !    sib%param%ndvi_period3 = time%bc_recnum
00099     sib%param%modis_period3 = time%bc_recnum
00100 !itb_modis
00101 
00102 !
00103 end subroutine read_single_td_param
00104 !
00105 !-------------------------------------------------------------------------------
00106 subroutine open_global_td( sib, time, filename )
00107 !-------------------------------------------------------------------------------
00108 ! opens netcdf file containing time dependent paramter data (ndvi, etc.)
00109 ! and reads in times for ndvi composite periods
00110 !
00111 ! Modifications:
00112 !  Kevin Schaefer created routine from new_bc (8/4/05)
00113 !-------------------------------------------------------------------------------
00114 !
00115 use sibtype
00116 use timetype
00117 use sib_io_module
00118 use sib_const_module
00119 use sib_bc_module
00120 !
00121 #ifdef PGF
00122 use netcdf
00123 use typeSizes
00124 #endif
00125 !
00126 implicit none
00127 !
00128 ! input and output variables
00129 type(sib_t), dimension(subcount), intent(inout) :: sib
00130 type(time_struct) :: time
00131 character*100 filename  ! filename used to read in ndvi data
00132 
00133 ! local variables
00134 integer(kind=int_kind) :: ntest1   ! compare nsib value of file to simulation
00135 integer(kind=int_kind) :: dimid    ! netcdf dimension id number
00136 integer(kind=int_kind) :: var_id   ! netcdf variable ID number
00137 integer(kind=int_kind) :: status   ! status of netcdf call
00138 character(len=10) :: name          ! not sure
00139 !
00140 ! close currently open file
00141     if(param_id/=0) then
00142       status = nf90_close ( param_id )
00143       if (status /= nf90_noerr) call handle_err (status)
00144     endif
00145 !
00146 ! Open the parameter file
00147 
00148 print*,'open_global_td: filename=',trim(filename)
00149 
00150     status = nf90_open ( trim(filename), nf90_nowrite, param_id )
00151     if (status /= nf90_noerr) call handle_err(status)
00152 !
00153 ! check number of points
00154     status = nf90_inq_dimid ( param_id, 'nsib', dimid )
00155     if (status /= nf90_noerr) call handle_err(status)
00156     status = nf90_inquire_dimension ( param_id, dimid, name, ntest1 )
00157     if (status /= nf90_noerr) call handle_err(status)
00158     if(ntest1 /= nsib) stop ' open: file sib_bc no match with model for nsib'
00159 !
00160 ! read in global attributes that are passed on to output files
00161 !itb_modis...this is still called 'ndvi_source' in the file I have...
00162     status = nf90_get_att( param_id, nf90_global, 'ndvi_source', ndvi_source )
00163     status = nf90_get_att( param_id, nf90_global, 'c4_source', c4_source )
00164     status = nf90_get_att( param_id, nf90_global, 'd13cresp_source', d13cresp_source )
00165 
00166 
00167 
00168 !itb_modis...
00169 
00170 
00171 !
00172 ! get ndvi id number
00173 !    status = nf90_inq_varid ( param_id, 'ndvi', ndvi_id )
00174 !    if (status /= nf90_noerr) call handle_err(status)
00175 !print*,'open_global_td: netcdf ids=',param_id,ndvi_id
00176 
00177 
00178 !
00179 ! get ndvi time id number
00180 !    status = nf90_inq_varid ( param_id, 'ndvi_time', ndvi_time_id )
00181 !    if (status /= nf90_noerr) call handle_err(status)
00182 
00183 !itb_modis; get modis id info
00184     status = nf90_inq_varid ( param_id, 'lai', mlai_id )
00185     if (status /= nf90_noerr) call handle_err(status)
00186     status = nf90_inq_varid ( param_id, 'fpar', mfpar_id )
00187     if (status /= nf90_noerr) call handle_err(status)
00188 
00189 ! get modis time id number
00190     status = nf90_inq_varid ( param_id, 'modis_time', modis_time_id )
00191     if (status /= nf90_noerr) call handle_err(status)
00192 
00193 !print*,'open_global_td: netcdf ids=',param_id,mlai_id,mfpar_id
00194 
00195 !itb_modis
00196 
00197 
00198 
00199 
00200 
00201 !
00202 ! get d13cresp id number
00203     status = nf90_inq_varid ( param_id, 'd13cresp', d13_id )
00204     if (status /= nf90_noerr) call handle_err(status)
00205 !
00206 ! get physfrac id number
00207     status = nf90_inq_varid ( param_id, 'physfrac', phys_id )
00208     if (status /= nf90_noerr) call handle_err(status)
00209 !
00210 ! get number of composite periods
00211     status = nf90_inq_varid ( param_id, 'mapsyear', var_id )
00212     if (status /= nf90_noerr) call handle_err(status)
00213     status = nf90_get_var ( param_id, var_id, nper )
00214     if (status /= nf90_noerr) call handle_err (status)
00215 
00216 !itb_modis
00217 !
00218 ! get composite period start times
00219 !    status = nf90_inq_varid ( param_id, 'ndvi_start', var_id )
00220 !    if (status /= nf90_noerr) call handle_err(status)
00221 !    status = nf90_get_var ( param_id, var_id, time%ndvi_start(1:nper) )
00222 !    if (status /= nf90_noerr) call handle_err (status)
00223 !
00224 ! get composite period stop times
00225 !    status = nf90_inq_varid ( param_id, 'ndvi_stop', var_id )
00226 !    if (status /= nf90_noerr) call handle_err(status)
00227 !    status = nf90_get_var ( param_id, var_id, time%ndvi_stop(1:nper) )
00228 !    if (status /= nf90_noerr) call handle_err (status)
00229 
00230 
00231 ! get composite period start times
00232     status = nf90_inq_varid ( param_id, 'modis_start', var_id )
00233     if (status /= nf90_noerr) call handle_err(status)
00234     status = nf90_get_var ( param_id, var_id, time%modis_start(1:nper) )
00235     if (status /= nf90_noerr) call handle_err (status)
00236 !
00237 ! get composite period stop times
00238     status = nf90_inq_varid ( param_id, 'modis_stop', var_id )
00239     if (status /= nf90_noerr) call handle_err(status)
00240     status = nf90_get_var ( param_id, var_id, time%modis_stop(1:nper) )
00241     if (status /= nf90_noerr) call handle_err (status)
00242 
00243 !itb_modis
00244 
00245 !
00246 end subroutine open_global_td
00247 !
00248 !-------------------------------------------------------------------------------
00249 subroutine read_global_td_param(sib, time)
00250 !-------------------------------------------------------------------------------
00251 ! Reads time dependent parameter data (ndvi, etc.) from netcdf file
00252 !
00253 ! Modifications:
00254 !  Kevin Schaefer created routine from read_ndvi (8/4/05)
00255 !-------------------------------------------------------------------------------
00256 
00257 use sibtype
00258 use timetype
00259 use sib_io_module
00260 use sib_const_module
00261 use sib_bc_module
00262 
00263 #ifdef PGF
00264 use netcdf
00265 use typeSizes
00266 #endif
00267 
00268 Implicit none
00269 
00270 ! input/output variables
00271 type(sib_t), dimension(subcount), intent(inout) :: sib
00272 type(time_struct), intent(in) :: time
00273 !
00274 ! local variables
00275 integer(kind=int_kind) :: i,j
00276 integer(kind=int_kind) :: ntest1     ! value of nsib read from sib_bc file
00277 integer(kind=int_kind) :: status     ! netcdf status variable
00278 integer(kind=int_kind) :: begin (2)  ! indeces where to start reading from file
00279 integer(kind=int_kind) :: finish (2) ! indeces where to finish reading from file
00280 
00281 !itb_modis
00282 !real(kind=real_kind), dimension(nsib) :: ndvi      ! temp ndvi variable
00283 !real(kind=real_kind), dimension(nsib) :: ndvi_time ! temp ndvi time variable
00284 
00285 real(kind=real_kind), dimension(nsib) :: mlai       ! temp lai variable
00286 real(kind=real_kind), dimension(nsib) :: mfpar       ! temp fpar variable
00287 real(kind=real_kind), dimension(nsib) :: modis_time ! temp MODIS time variable
00288 
00289 !itb_modis
00290 
00291 real(kind=real_kind), dimension(nsib,physmax)  :: frac ! temp physfrac variabla
00292 real(kind=real_kind), dimension(nsib)  :: d13  ! temp d12cresp variable
00293 character(len=10) :: name
00294 !
00295 ! set start and stop points of data to read
00296     begin (1)  = time%bc_recnum 
00297     begin (2)  = 1 
00298     finish (1) = 1 
00299     finish (2) = nsib 
00300 
00301 !itb_modis
00302 !
00303 ! get ndvi data
00304 !print*,'read_ndvi:',param_id,ndvi_id,begin,finish
00305 !    status = nf90_get_var ( param_id, ndvi_id, ndvi, begin, finish )
00306 !    if (status /= nf90_noerr) call handle_err (status,'read_ndvi',1)
00307 !
00308 ! get ndvi times
00309 !    status = nf90_get_var ( param_id, ndvi_time_id, ndvi_time, begin, finish )
00310 !    if (status /= nf90_noerr) call handle_err (status,'read_ndvi',2)
00311 
00312 ! get MODIS data
00313 !print*,'read_modis:',param_id,mlai_id,mfpar_id,begin,finish
00314     status = nf90_get_var ( param_id, mlai_id, mlai, begin, finish )
00315     if (status /= nf90_noerr) call handle_err (status,'read_modis',1)
00316 
00317     status = nf90_get_var ( param_id, mfpar_id, mfpar, begin, finish )
00318     if (status /= nf90_noerr) call handle_err (status,'read_modis',2)
00319 !
00320 ! get modis times
00321     status = nf90_get_var ( param_id, modis_time_id, modis_time, begin, finish )
00322     if (status /= nf90_noerr) call handle_err (status,'read_modis',3)
00323 
00324 
00325 !itb_modis
00326 
00327 
00328 
00329 
00330 !
00331 ! get d13cresp data
00332     status = nf90_get_var ( param_id, d13_id, d13 )
00333     if (status /= nf90_noerr) call handle_err (status,'read_ndvi',3)
00334 !
00335 ! get physfrac data
00336     status = nf90_get_var ( param_id, phys_id, frac )
00337     if (status /= nf90_noerr) call handle_err (status,'read_ndvi',4)
00338 !
00339 ! copy only points in subdomain
00340     do i = 1, subcount
00341 
00342 !itb_modis
00343 !        sib(i)%param%ndvi3 = ndvi(subset(i))
00344 !        sib(i)%param%ndvi_time3 = ndvi_time(subset(i))
00345 !        sib(i)%param%ndvi_period3 = time%bc_recnum
00346 
00347         sib(i)%param%mlai3 = mlai(subset(i))
00348         sib(i)%param%mfpar3 = mfpar(subset(i))
00349 !print*,'READ_NDVI:',sib(i)%param%mlai3,sib(i)%param%mfpar3
00350         sib(i)%param%modis_time3 = modis_time(subset(i))
00351         sib(i)%param%modis_period3 = time%bc_recnum
00352 !itb_modis
00353 
00354         
00355         sib(i)%param%d13cresp = d13(subset(i))
00356         do j = 1, physmax
00357             sib(i)%param%physfrac3(j) = frac(subset(i),j)
00358         enddo
00359     enddo
00360 !
00361 end subroutine read_global_td_param
00362 !
00363 !-------------------------------------------------------------------------------
00364 subroutine calculate_td_param (sib, lat)
00365 !-------------------------------------------------------------------------------
00366 ! Calls mapper and calculates the time dependent parameters
00367 !
00368 ! MODIFICATIONS:
00369 !  Owen Leonard created routine (8/10/2001)
00370 !  Kevin Schaefer changed to single point (8/1/05)
00371 !-------------------------------------------------------------------------------
00372 !
00373 use sibtype
00374 use sib_const_module
00375 use sib_bc_module
00376 use kinds
00377 !
00378 implicit none
00379 !
00380 ! input/output variables
00381 type(sib_t), intent(inout) :: sib
00382     real(kind=real_kind) lat ! (deg) latitude of point
00383 !
00384 ! local variables
00385 integer(kind=int_kind) :: i,j,k,l,m,n   ! indeces
00386 type time_dep_var
00387     real(kind=real_kind) :: fpar       ! canopy absorbed fraction of par
00388     real(kind=real_kind) :: lai        ! leaf-area index
00389     real(kind=real_kind) :: green      ! canopy greeness fraction of lai
00390     real(kind=real_kind) :: zo         ! canopy roughness coeff 
00391     real(kind=real_kind) :: zp_disp    ! zero plane displacement
00392     real(kind=real_kind) :: rbc        ! rb coefficient (c1)
00393     real(kind=real_kind) :: rdc        ! rc coefficient (c2)
00394     real(kind=real_kind) :: gmudmu     ! time-mean leaf projection
00395 end type time_dep_var
00396 type(time_dep_var) :: timevar
00397 real(kind=real_kind) :: temptran (2,2)
00398 real(kind=real_kind) :: tempref (2,2)
00399 type(aero_var) :: tempaerovar(50,50)
00400 !
00401 ! convert biome number to integer
00402     i = int(sib%param%biome)
00403 !
00404 ! double to single precision conversion
00405     temptran = sib%param%tran(:,:)
00406     tempref = sib%param%ref(:,:)
00407     tempaerovar = aerovar(:,:,i)
00408 !
00409 ! calculate new MODIS derived parameters
00410     call mapper(          &
00411       lat,&
00412       sib%param%modis_time1,           &      
00413       sib%param%modis_time2,           &
00414       sib%param%mlai1,      &
00415       sib%param%mlai2,       &
00416       sib%param%mfpar1,      &
00417       sib%param%mfpar2,       &
00418       sib%param%vcover, &
00419       sib%param%chil,   &
00420       temptran,         &
00421       tempref,          &
00422       morphtab(i),      &
00423       tempaerovar,      &
00424       laigrid,          &
00425       fvcovergrid,      &
00426       timevar)
00427 !
00428 ! move ndvi parameters to sib variable tree
00429     sib%param%aparc2 = timevar%fpar
00430     sib%param%zlt2 = timevar%lai
00431     sib%param%green2 = timevar%green
00432     sib%param%z0d2 = timevar%zo
00433     sib%param%zp_disp2 = timevar%zp_disp
00434     sib%param%rbc2 = timevar%rbc
00435     sib%param%rdc2 = timevar%rdc
00436     sib%param%gmudmu2 = timevar%gmudmu 
00437 
00438 end subroutine calculate_td_param
00439 !
00440 !------------------------------------------------------------------------------
00441 subroutine need_to_switch (sib,time)
00442 !------------------------------------------------------------------------------
00443 ! determines if time dependent parameters need to be switched ( 3-->2, 2-->1 )
00444 !
00445 ! MODIFICATIONS:
00446 !  Andrew Philpott created routine (9/28/05)
00447 !------------------------------------------------------------------------------
00448 !
00449 use sibtype
00450 use timetype
00451 use sib_const_module
00452 use sib_bc_module
00453 use kinds
00454 !
00455 implicit none
00456 !
00457 ! parameters
00458 type(sib_t), dimension(subcount), intent(inout) :: sib
00459 type(time_struct), intent(in) :: time
00460 !
00461 ! local variables
00462 integer i      ! index
00463 real(kind=real_kind) :: ndtime   ! (day) local version of ndvi2 time
00464 integer(kind=int_kind) :: ndp    ! (#) local version of ndvi2 period
00465 real(kind=real_kind) :: conv  ! (day) local real copy of time%sec_per_day
00466 !
00467 conv = real(time%sec_per_day)
00468 do i=1,subcount
00469 !
00470         ! copy local versions of ndvi2 time and ndvi2 period
00471 !itb_modis
00472 !        ndtime = sib(i)%param%ndvi_time2
00473         ndtime = sib(i)%param%modis_time2
00474 
00475 !        ndp = sib(i)%param%ndvi_period2
00476         ndp = sib(i)%param%modis_period2
00477 !
00478         ! two cases: Case A) -- ndvi2 time is at the end of ndvi2 period
00479         !            in this case we want to switch one timestep prior to 
00480         !            then because we want to copy the old period 3 values to 
00481         !            period 2 before overwriting them in the read routines
00482         !  Note -- checking that two real numbers are equal requires rather 
00483         !       checking that they are very nearly equal (avoid roundoff diffs)
00484 
00485 
00486 !itb_modis; I'm just replacing 'ndvi' with 'modis' here, hoping the 
00487 !           conversion is seamless...
00488 
00489 !itb        if ( ndtime >= time%ndvi_stop(ndp) - 0.1*time%dtsib/conv .and. &
00490 !itb               ndtime <= time%ndvi_stop(ndp) + 0.1*time%dtsib/conv ) then
00491 !itb            if ( time%real_doy >= ndtime - 1.1*time%dtsib/conv .and. &
00492 !itb                   time%real_doy <= ndtime - 0.9*time%dtsib/conv .and. &
00493 !itb                   ndp<nper   ) then
00494 !itb              call switch_td_param (sib(i))
00495 !itb              call calculate_td_param (sib(i), latsib(subset(i)))
00496 !       print*,'A1 switch',sib%param%ndvi_time1,sib%param%ndvi_time2,sib%param%ndvi_time3
00497 !itb            else if ( time%real_doy >= 365. - 1.1*time%dtsib/conv .and. &
00498 !itb                      time%real_doy <= 365. - 0.9*time%dtsib/conv .and. &
00499 !itb                      ndp==nper   ) then
00500 !itb              call switch_td_param (sib(i))
00501 !itb              call calculate_td_param (sib(i), latsib(subset(i)))
00502 !       print*,'A2 switch',sib%param%ndvi_time1,sib%param%ndvi_time2,sib%param%ndvi_time3                     
00503 !itb            end if
00504         ! Case B) -- ndvi2 time is not at the end of ndvi2 period
00505         !            switch td params at the beginning of the first day 
00506         !            that falls after ndvi2 time                
00507 !itb        else if (time%new_day) then
00508 !itb          if (time%real_doy>=ndtime .and. &
00509 !itb              (ndp>1.or.time%real_doy<time%ndvi_start(nper))) then
00510 !itb            call switch_td_param (sib(i))
00511 !itb            call calculate_td_param (sib(i), latsib(subset(i)))
00512 !       print*,'B1 switch',sib%param%ndvi_time1,sib%param%ndvi_time2,sib%param%ndvi_time3
00513 !itb          end if
00514 !itb    end if
00515 
00516 !print*,'need_to_switch:',ndtime,ndp
00517         if ( ndtime >= time%modis_stop(ndp) - 0.1*time%dtsib/conv .and. &
00518                ndtime <= time%modis_stop(ndp) + 0.1*time%dtsib/conv ) then
00519             if ( time%real_doy >= ndtime - 1.1*time%dtsib/conv .and. &
00520                    time%real_doy <= ndtime - 0.9*time%dtsib/conv .and. &
00521                    ndp<nper   ) then
00522               call switch_td_param (sib(i))
00523               call calculate_td_param (sib(i), latsib(subset(i)))
00524 !       print*,'A1 switch',sib%param%modis_time1,sib%param%modis_time2,sib%param%modis_time3
00525             else if ( time%real_doy >= 365. - 1.1*time%dtsib/conv .and. &
00526                       time%real_doy <= 365. - 0.9*time%dtsib/conv .and. &
00527                       ndp==nper   ) then
00528               call switch_td_param (sib(i))
00529               call calculate_td_param (sib(i), latsib(subset(i)))
00530 !       print*,'A2 switch',sib%param%modis_time1,sib%param%modis_time2,sib%param%modis_time3                  
00531             end if
00532         ! Case B) -- ndvi2 time is not at the end of ndvi2 period
00533         !            switch td params at the beginning of the first day 
00534         !            that falls after ndvi2 time                
00535         else if (time%new_day) then
00536           if (time%real_doy>=ndtime .and. &
00537               (ndp>1.or.time%real_doy<time%modis_start(nper))) then
00538             call switch_td_param (sib(i))
00539             call calculate_td_param (sib(i), latsib(subset(i)))
00540 !       print*,'B1 switch',sib%param%modis_time1,sib%param%modis_time2,sib%param%modis_time3
00541           end if
00542         end if
00543 !
00544 end do
00545 !
00546 end subroutine need_to_switch                                   
00547                                     
00548 !-------------------------------------------------------------------------------
00549 subroutine switch_td_param (sib)
00550 !-------------------------------------------------------------------------------
00551 ! switches time dependent parameters
00552 !
00553 ! MODIFICATIONS:
00554 !  Kevin Schaefer created routine (8/1/05)
00555 !-------------------------------------------------------------------------------
00556 !
00557 use sibtype
00558 use sib_const_module
00559 use sib_bc_module
00560 use kinds
00561 !
00562 implicit none
00563 !
00564 ! input/output variables
00565 type(sib_t), intent(inout) :: sib
00566 !
00567 ! local variables
00568 integer(kind=int_kind) :: i,j,k,l,m,n   ! indeces
00569 !
00570 ! switch time dependent parameters
00571 !itb_modis
00572 
00573 !        sib%param%ndvi1        = sib%param%ndvi2
00574 !        sib%param%ndvi_time1   = sib%param%ndvi_time2
00575 
00576         sib%param%mlai1        = sib%param%mlai2
00577         sib%param%mfpar1        = sib%param%mfpar2
00578         sib%param%modis_time1   = sib%param%modis_time2
00579 
00580 !        sib%param%ndvi_period1 = sib%param%ndvi_period2        
00581         sib%param%modis_period1 = sib%param%modis_period2
00582 !itb_modis
00583         
00584         sib%param%aparc1       = sib%param%aparc2
00585         sib%param%zlt1         = sib%param%zlt2
00586         sib%param%green1       = sib%param%green2
00587         sib%param%z0d1         = sib%param%z0d2
00588         sib%param%zp_disp1     = sib%param%zp_disp2
00589         sib%param%rbc1         = sib%param%rbc2
00590         sib%param%rdc1         = sib%param%rdc2
00591         sib%param%gmudmu1      = sib%param%gmudmu2
00592         sib%param%d13cresp1    = sib%param%d13cresp2
00593         do i = 1, physmax
00594             sib%param%physfrac1(i) = sib%param%physfrac2(i)
00595         enddo
00596 !
00597 !itb_modis...
00598 ! move in new ndvi data
00599 !        sib%param%ndvi2       = sib%param%ndvi3
00600 !        sib%param%ndvi_time2   = sib%param%ndvi_time3
00601 !        sib%param%ndvi_period2 = sib%param%ndvi_period3        
00602         sib%param%mlai2       = sib%param%mlai3
00603         sib%param%mfpar2       = sib%param%mfpar3
00604         sib%param%modis_time2   = sib%param%modis_time3
00605         sib%param%modis_period2 = sib%param%modis_period3
00606 !itb_modis
00607 
00608         
00609         sib%param%d13cresp2    = sib%param%d13cresp3
00610         do i = 1, physmax
00611             sib%param%physfrac2(i) = sib%param%physfrac3(i)
00612         enddo 
00613 
00614 !print*,'switch:',sib%param%ndvi_time1,sib%param%ndvi_time2,sib%param%ndvi_time3
00615 
00616 end subroutine switch_td_param
00617 !
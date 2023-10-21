00001 
00002 !---------------------------------------------------------------------
00003 subroutine init_sibdrv( sib, time )
00004 !---------------------------------------------------------------------
00005 
00006 use sibtype
00007 use timetype
00008 use sib_const_module
00009 use sib_io_module
00010 implicit none
00011 
00012 !---------------------------------------------------------------------
00013 !itb...init_sibdrv reads most initialization information. takes 
00014 !itb...place of several BUGS routines, most notably init_global.
00015 !
00016 !     REFERENCES:
00017 !
00018 ! Modifications:
00019 !  - added dtsibbcin, dtsibmetin for possible different intervals
00020 !    of reading in the sibbc and sibdrv met data dd, jk 980209
00021 !  Kevin Schaefer moved read IC and respfactor to after driver data (8/12/04)
00022 !  Kevin Schaefer added calls to read NCEP1 driver data (8/13/04)
00023 !  Kevin Schaefer deleted init of tot_an and tot_ss done in init_var (5/6/05)
00024 !
00025 !     SUBROUTINES CALLED:
00026 !          none
00027 !     FUNCTIONS CALLED:
00028 !          none
00029 !
00030 !     INCLUDED COMMONS:
00031 !
00032 !     ARGUMENT LIST VARIABLES
00033 !---------------------------------------------------------------------
00034 
00035 ! parameters
00036 type(sib_t), dimension(subcount), intent(inout) :: sib
00037 type(time_struct), intent(inout) :: time
00038 
00039 ! local variables
00040 integer(kind=int_kind) :: i
00041 
00042     print *, 'INIT_SIBDRV:'
00043 
00044     !itb---------------------------------------------------------------------
00045     !itb...initialize some seasonal diagnostic values...
00046 
00047     do i = 1, subcount
00048         sib(i)%diag%snow_end(1) = 365.0
00049         sib(i)%diag%snow_end(2) = 365.0
00050         sib(i)%diag%snow_end(3) = 365.0
00051         sib(i)%stat%pt_num=i
00052     enddo
00053 
00054     ! parse sib_qpopts and sib_pbpopts to see which variables are output
00055     call read_qp_pbp_opts
00056     
00057     ! initialize time variables
00058     print *, '\t initialize time variables'
00059     call time_init( time )
00060     sib(:)%stat%julday = time%doy
00061 !
00062 ! initialize parameters
00063     call init_parameters( sib, time )
00064 
00065     ! read in initial driver data
00066     print *, '\t reading in initial time-step driver data'
00067     if ( drvr_type == 'ecmwf' ) then
00068         call sibdrv_read_ecmwf( sib, time )
00069     elseif ( drvr_type == 'ncep1' ) then
00070         call sibdrv_read_ncep1( sib, time )
00071     elseif ( drvr_type == 'ncep2' ) then
00072         call sibdrv_read_ncep2( sib, time )
00073     elseif ( drvr_type == 'geos4' ) then
00074         call sibdrv_read_geos4( sib, time )
00075     elseif ( drvr_type == 'single' ) then
00076         call sibdrv_read_single( sib, time )
00077     elseif ( drvr_type == 'ncp_sngl' ) then
00078         call sibdrv_read_ncep2_single( sib, time )
00079 
00080 
00081     else
00082         stop 'Invalid drvr_type specified'
00083     endif
00084 !
00085 ! read in initial conditions
00086     print *, '\t reading in initial conditions '
00087     call read_ic(sib,time)
00088 
00089 ! read in respfactor file
00090     if(drvr_type=='single'.OR. drvr_type == 'ncp_sngl') then
00091       call read_single_respfactor(sib,time)
00092     else
00093       call read_global_respfactor(sib,time)
00094     endif
00095 
00096 ! calculate initial solar declination
00097     call init_solar_dec( time )
00098 
00099 
00100 
00101 end subroutine init_sibdrv
00102 !
00103 !---------------------------------------------------------------------
00104 subroutine init_parameters( sib, time )
00105 !---------------------------------------------------------------------
00106 ! reads and assigns all time invariant parameters
00107 ! reads in appropriate time dependent variables to start program
00108 ! Assumes ndvi composite periods apply to all time dependent parameters
00109 !
00110 ! Modifications:
00111 !  Kevin Schaefer created routine (7/29/05)
00112 !---------------------------------------------------------------------
00113 !
00114 use sibtype
00115 use timetype
00116 use sib_const_module
00117 use sib_io_module
00118 !
00119 implicit none
00120 !
00121 ! input
00122 type(sib_t), dimension(subcount), intent(inout) :: sib
00123 type(time_struct), intent(inout) :: time
00124 
00125 ! local variables
00126 integer(kind=int_kind) :: i, j,k,l,m,n   ! indeces
00127 integer(kind=int_kind) temp_recnum  ! temporary parameter record number
00128 real(kind=real_kind) temp_doy       ! (day) temporary time from first of current year
00129 character*100 filename  ! filename used to read in ndvi data
00130 !
00131 ! print message to screen
00132     print *, '\t Initialize parameters'
00133 !
00134 ! read in time-invariant parameters
00135        call read_ti(sib)
00136 !
00137 ! open current time dependent parameter file
00138 print*,'driver type:',drvr_type
00139 
00140        if ( drvr_type == 'single' .OR. drvr_type == 'ncp_sngl') then
00141           write (filename, "(a,i4)") trim(param_path), time%year
00142           call open_single_td( sib, time, filename)
00143        else
00144           write (filename, "(a,i4,a3)") trim(param_path), time%year, '.nc'
00145           print*,'init_sibdrv: open_global_td:1'
00146           call open_global_td( sib, time, filename)
00147        endif
00148 
00149 
00150        !
00151        ! determine current ndvi composite period (time%bc_recnum)
00152        time%bc_recnum=0
00153        do i = 1,nper
00154 !itb_modis
00155 !          if(time%real_doy>=time%ndvi_start(i)) then
00156 !             if(time%real_doy<time%ndvi_stop(i)) time%bc_recnum=i
00157 !             if(i==nper.and.time%real_doy<time%ndvi_stop(i)+365.) time%bc_recnum=i
00158 
00159           if(time%real_doy>=time%modis_start(i)) then
00160              if(time%real_doy<time%modis_stop(i)) time%bc_recnum=i
00161              if(i==nper.and.time%real_doy<time%modis_stop(i)+365.) time%bc_recnum=i
00162 !itb_modis
00163           endif
00164        enddo
00165        if (time%bc_recnum==0) stop ' param file does not cover this time'
00166        !
00167        ! save current parameter record number
00168 
00169        temp_recnum=time%bc_recnum
00170 
00171        !
00172        ! go back two composite periods
00173 
00174        time%bc_recnum=temp_recnum-2
00175 
00176        !
00177        ! switch to previous year's time dep parameter file if required
00178 
00179        if(temp_recnum<=2) then
00180           if ( drvr_type == 'single' .OR. drvr_type == 'ncp_sngl' ) then
00181              write (filename, "(a,i4)") trim(param_path), time%year-1
00182              call open_single_td( sib, time, filename)
00183              time%bc_recnum=nper+temp_recnum-2
00184           else
00185              write (filename, "(a,i4,a3)") trim(param_path), time%year-1, '.nc'
00186           print*,'init_sibdrv: open_global_td:2'
00187              call open_global_td( sib, time, filename)
00188              time%bc_recnum=nper+temp_recnum-2
00189           endif
00190        endif
00191 
00192 
00193        !
00194        ! read ndvi (sib%param%ndvi3) and other time dependent parameters
00195        if ( drvr_type == 'single' .OR. drvr_type == 'ncp_sngl') then
00196           do k = 1,time%bc_recnum
00197              call read_single_td_param(sib, time)
00198           enddo
00199        else
00200           call read_global_td_param(sib,time)
00201        endif
00202        !
00203        ! switch the parameters (put ndvi2 into ndvi1 and ndvi3 into ndvi2)
00204        do i=1,subcount
00205           call switch_td_param (sib(i))
00206        enddo
00207 
00208 
00209        !
00210        ! go forward one composite period to the one just prior to present period
00211 
00212        time%bc_recnum = time%bc_recnum + 1
00213        if (time%bc_recnum > nper) time%bc_recnum = 1   
00214   
00215        !
00216        ! switch back to current time dep parameter file if required
00217 
00218        if(time%bc_recnum==1) then
00219           if ( drvr_type == 'single' .OR. drvr_type == 'ncp_sngl') then
00220              write (filename, "(a,i4)") trim(param_path), time%year
00221              call open_single_td( sib, time, filename)
00222           else
00223              write (filename, "(a,i4,a3)") trim(param_path), time%year, '.nc'
00224           print*,'init_sibdrv: open_global_td:3'
00225              call open_global_td( sib, time, filename)
00226           endif
00227        endif
00228 
00229 
00230        !
00231        ! read ndvi and other time dependent parameters
00232 
00233        if ( drvr_type == 'single' .OR. drvr_type == 'ncp_sngl') then
00234           call read_single_td_param(sib, time)
00235        else
00236           call read_global_td_param(sib,time)
00237        endif
00238 
00239 
00240        !
00241        ! switch the parameters (put ndvi2 into ndvi1 and ndvi3 into ndvi2)
00242        ! calculate ndvi derived parameters 
00243 
00244        do i=1,subcount
00245           call switch_td_param (sib(i))
00246           call calculate_td_param (sib(i), latsib(subset(i)))
00247        enddo
00248 
00249        !
00250        ! go to current composite period, read ndvi and calculate parameters
00251        time%bc_recnum=temp_recnum
00252        !
00253        ! switch back to current time dep parameter file if required
00254 
00255        if(time%bc_recnum==1) then
00256           if ( drvr_type == 'single' .OR. drvr_type == 'ncp_sngl') then
00257              write (filename, "(a,i4)") trim(param_path), time%year
00258              call open_single_td( sib, time, filename)
00259           else
00260              write (filename, "(a,i4,a3)") trim(param_path), time%year, '.nc'
00261           print*,'init_sibdrv: open_global_td:4'
00262              call open_global_td( sib, time, filename)
00263           endif
00264        endif
00265 
00266 
00267        !
00268        ! read ndvi and other time dependent parameters
00269 
00270        if ( drvr_type == 'single' .OR. drvr_type == 'ncp_sngl') then
00271           call read_single_td_param(sib, time)
00272        else
00273           call read_global_td_param(sib,time)
00274        endif
00275 
00276 
00277        !
00278        ! switch parameters (put ndvi2 into ndvi1 and ndvi3 into ndvi2)
00279        ! calculate ndvi derived parameters 
00280        do i=1,subcount
00281           call switch_td_param (sib(i))
00282           call calculate_td_param (sib(i), latsib(subset(i)))
00283        enddo
00284        !
00285        ! Now read ndvi from the next period beyond the present one
00286 
00287        time%bc_recnum = time%bc_recnum + 1
00288        if (time%bc_recnum > nper) time%bc_recnum = 1
00289        
00290        ! switch to next time dep parameter file if required
00291 
00292        if(time%bc_recnum==1) then
00293           if ( drvr_type == 'single' .OR. drvr_type == 'ncp_sngl') then
00294              write (filename, "(a,i4)") trim(param_path), time%year+1
00295              call open_single_td( sib, time, filename)
00296           else
00297              write (filename, "(a,i4,a3)") trim(param_path), time%year+1, '.nc'
00298           print*,'init_sibdrv: open_global_td:5'
00299              call open_global_td( sib, time, filename)
00300           endif
00301        endif
00302 
00303 
00304        !
00305        ! read ndvi and other time dependent parameters
00306 
00307        if ( drvr_type == 'single' .OR. drvr_type == 'ncp_sngl') then
00308           call read_single_td_param(sib, time)
00309        else
00310           call read_global_td_param(sib,time)
00311        endif
00312 
00313        !
00314        ! switch params (3-->2, 2-->1) one more time if ndvi time 2 is at the beginning
00315        !                               of its compositing period
00316        do i=1,subcount
00317 
00318 !itb_modis; ugly! has to be converted to MODIS vars...
00319 !          if (        sib(i)%param%ndvi_time2 >=              &
00320 !               time%ndvi_start(sib(i)%param%ndvi_period2)- &
00321 !               0.1*time%dtsib/real(time%sec_per_day) .and. &
00322 !               sib(i)%param%ndvi_time2 <=              &
00323 !               time%ndvi_start(sib(i)%param%ndvi_period2)+ &
00324 !               0.1*time%dtsib/real(time%sec_per_day) ) then
00325 
00326           if (        sib(i)%param%modis_time2 >=              &
00327                time%modis_start(sib(i)%param%modis_period2)- &
00328                0.1*time%dtsib/real(time%sec_per_day) .and. 
00329                sib(i)%param%modis_time2 <=              
00330                time%modis_start(sib(i)%param%modis_period2)+ 
00331                0.1*time%dtsib/real(time%sec_per_day) ) then
00332 !itb_modis
00333 
00334              call switch_td_param (sib(i))
00335              call calculate_td_param (sib(i), latsib(subset(i)))
00336           end if
00337        end do
00338 
00339 
00340 
00341 
00342 
00343 !itb...set read_bc to false
00344   time%read_bc = .false.
00345 
00346 
00347 !
00348 ! calculate soil properties
00349     call soil_properties( sib )
00350 print*, 'bc_recnum=',time%bc_recnum
00351 !
00352 end subroutine init_parameters
00353 !
00354 !===============================================================================
00355 subroutine read_qp_pbp_opts
00356 !===============================================================================
00357 
00358 use sib_io_module
00359 use sib_const_module
00360 implicit none
00361 
00362 integer(kind=int_kind) :: i,n
00363 logical(kind=log_kind) :: doqptem
00364 integer(kind=int_kind) :: ipbp, ldummy, ndummy
00365 character (len=16) :: nametem
00366 character (len=80) :: listtem
00367 integer(kind=int_kind), dimension(:), allocatable :: imulttem, imulttem2
00368 
00369 
00370     !---------------------------------------------------------------------------
00371     ! read sib_qpopts and count number of variables to be output
00372     !---------------------------------------------------------------------------
00373     open(unit=2,file=qp_path,form='formatted') !jk
00374     nqpsib = 0
00375     nqp3sib = 0
00376     do 
00377         read(2,*, end=922)doqptem,ldummy,nametem,ndummy,listtem
00378         if(ldummy.eq.1) then
00379             nqp3sib = nqp3sib + 1
00380         else if (ldummy.eq.0) then
00381             nqpsib = nqpsib + 1
00382         endif
00383     enddo
00384 
00385     922  continue
00386 
00387     rewind 2
00388     allocate (doqp3sib(nqp3sib))
00389     allocate (nameqp3sib(nqp3sib))
00390     allocate (listqp3sib(nqp3sib))
00391     allocate (numqp3sib(nqp3sib))
00392     allocate (doqpsib(nqpsib))
00393     allocate (nameqpsib(nqpsib))
00394     allocate (listqpsib(nqpsib))
00395     allocate (numqpsib(nqpsib))
00396     iiqp3sib = 0
00397     iiqpsib = 0
00398     do i = 1,nqp3sib+nqpsib
00399         read(2,*)doqptem,ldummy,nametem,ndummy,listtem
00400         if(ldummy.eq.1) then
00401             iiqp3sib = iiqp3sib + 1
00402             doqp3sib(iiqp3sib) = doqptem
00403             nameqp3sib(iiqp3sib) = nametem
00404             listqp3sib(iiqp3sib) = listtem
00405             numqp3sib(iiqp3sib) = ndummy
00406         else if (ldummy.eq.0) then
00407             iiqpsib = iiqpsib + 1
00408             doqpsib(iiqpsib) = doqptem
00409             nameqpsib(iiqpsib) = nametem
00410             listqpsib(iiqpsib) = listtem
00411             numqpsib(iiqpsib) = ndummy
00412         endif
00413     enddo 
00414     close(2)
00415     allocate (indxqp3sib(nqp3sib))
00416     allocate (indxqpsib(nqpsib))
00417 
00418     iiqpsib = 0
00419     do n = 1,nqpsib
00420         if(doqpsib(n)) then
00421             iiqpsib = iiqpsib + 1
00422             indxqpsib(n) = iiqpsib
00423         endif
00424     enddo
00425     iiqp3sib = 0
00426     do n = 1,nqp3sib
00427         if(doqp3sib(n)) then
00428             iiqp3sib = iiqp3sib + 1
00429             indxqp3sib(n) = iiqp3sib
00430         endif
00431     enddo
00432     do n = 1,nqpsib
00433         if(.not.doqpsib(n)) then
00434             indxqpsib(n) = iiqpsib + 1
00435         endif
00436     enddo
00437     do n = 1,nqp3sib
00438         if(.not.doqp3sib(n)) then
00439             indxqp3sib(n) = iiqp3sib + 1
00440         endif
00441     enddo
00442 
00443 
00444     !      initialize diagnostics         
00445     allocate (qpsib(subcount,iiqpsib+1))   
00446     allocate( qp2varid(nqpsib) )
00447     allocate( qp3varid(nqp3sib) )
00448     allocate (qp3sib(subcount,nsoil,iiqp3sib+1))   
00449     qp3sib(:,:,:) = 0.0
00450     qpsib(:,:) = 0.0
00451     print*,'\t diagnostics initialized'
00452 
00453 
00454 
00455     !---------------------------------------------------------------------------
00456     ! read sib_pbpopts and count number of variables to be output
00457     !---------------------------------------------------------------------------
00458     open(unit=2,file=pbp_path,form='formatted')   !jk
00459     npbpsib = 0
00460     npbp2sib = 0
00461     
00462     ! count number of variables listed for pbp and pbp2 data in sib_pbpopts
00463     do 
00464         read(2,*, end=932)doqptem,ldummy,nametem,ndummy,listtem
00465         if(ldummy.eq.1) then
00466             npbp2sib = npbp2sib + 1
00467         else if (ldummy.eq.0) then
00468             npbpsib = npbpsib + 1
00469         endif
00470     enddo 
00471     932  continue
00472     rewind 2
00473 
00474     allocate (dopbp2sib(npbp2sib))
00475     allocate (namepbp2sib(npbp2sib))
00476     allocate (listpbp2sib(npbp2sib))
00477     allocate (numpbp2sib(npbp2sib))
00478     allocate (dopbpsib(npbpsib))
00479     allocate (namepbpsib(npbpsib))
00480     allocate (listpbpsib(npbpsib))
00481     allocate (numpbpsib(npbpsib))
00482 
00483     ! count number of variables that are set to be saved to pbp files
00484     iipbp2sib = 0
00485     iipbpsib = 0
00486     do i = 1,npbp2sib+npbpsib
00487         read(2,*)doqptem,ldummy,nametem,ndummy,listtem
00488         if(ldummy.eq.1) then
00489             iipbp2sib = iipbp2sib + 1
00490             dopbp2sib(iipbp2sib) = doqptem
00491             namepbp2sib(iipbp2sib) = nametem
00492             listpbp2sib(iipbp2sib) = listtem
00493             numpbp2sib(iipbp2sib) = ndummy
00494         else if (ldummy.eq.0) then
00495             iipbpsib = iipbpsib + 1
00496             dopbpsib(iipbpsib) = doqptem
00497             namepbpsib(iipbpsib) = nametem
00498             listpbpsib(iipbpsib) = listtem
00499             numpbpsib(iipbpsib) = ndummy
00500         endif
00501     enddo 
00502     close(2)
00503     
00504     allocate (indxpbp2sib(npbp2sib))
00505     allocate (indxpbpsib(npbpsib))
00506 
00507     iipbpsib = 0
00508     do n = 1,npbpsib
00509         if(dopbpsib(n)) then
00510             iipbpsib = iipbpsib + 1
00511             indxpbpsib(n) = iipbpsib
00512         endif
00513     enddo
00514     iipbp2sib = 0
00515     do n = 1,npbp2sib
00516         if(dopbp2sib(n)) then
00517             iipbp2sib = iipbp2sib + 1
00518             indxpbp2sib(n) = iipbp2sib
00519         endif
00520     enddo
00521     do n = 1,npbpsib
00522         if(.not.dopbpsib(n)) then
00523             indxpbpsib(n) = iipbpsib + 1
00524         endif
00525     enddo
00526     do n = 1,npbp2sib
00527         if(.not.dopbp2sib(n)) then
00528             indxpbp2sib(n) = iipbp2sib + 1
00529         endif
00530     enddo
00531 
00532     allocate( pbpsib(iipbpsib+1,ijtlensib) )
00533     allocate( pbpvarid(npbpsib) )
00534     allocate( pbp2sib(nsoil,iipbp2sib+1,ijtlensib) )
00535     allocate( pbp2varid(npbp2sib) )
00536     pbpsib(:,:) = 0.0
00537     pbp2sib(:,:,:) = 0.0
00538 
00539 end subroutine read_qp_pbp_opts
00540 !
00541 !===============================================================================
00542 subroutine read_ic(sib,time)
00543 !===============================================================================
00544 !  Author:  Ian Baker
00545 !  Modified by:  Owen Leonard
00546 !  Date :  March 30, 2004
00547 !  Purpose:
00548 !    This subroutine reads in the initial conditions file and pulls out
00549 !  only those points in the subdomain
00550 !
00551 ! Modifications:
00552 !  Kevin Schaefer moved soil layer calculations to soil_properties (10/27/04)
00553 !===============================================================================
00554 
00555 #ifdef PGF
00556 use netcdf 
00557 use typeSizes
00558 #endif
00559 use kinds
00560 use sibtype
00561 use timetype
00562 use sib_const_module
00563 use sib_io_module
00564 
00565 ! parameters
00566 type(sib_t), dimension(subcount), intent(inout) :: sib
00567 type(time_struct), intent(inout) :: time
00568 ! netcdf variables
00569 integer(kind=int_kind) :: status
00570 integer(kind=int_kind) :: ncid
00571 integer(kind=int_kind) :: varid
00572 
00573 ! local variables
00574 integer(kind=int_kind) :: i,j,k
00575 integer(kind=int_kind) :: nsibt
00576 integer(kind=int_kind) :: nsoilt
00577 integer(kind=int_kind) :: nsnowt
00578 real(kind=int_kind) :: versiont
00579 integer(kind=int_kind) :: subcountt
00580 integer(kind=int_kind), dimension(2) :: start
00581 integer(kind=int_kind), dimension(2) :: finish
00582 real(kind=dbl_kind), dimension(nsib) :: ta
00583 real(kind=dbl_kind), dimension(nsib) :: tc
00584 integer(kind=int_kind), dimension(nsib) :: nsl
00585 real(kind=dbl_kind), dimension(nsib) :: pco2ap
00586 !itb_ocs
00587 real(kind=dbl_kind), dimension(nsib) :: pcosap
00588 real(kind=dbl_kind), dimension(nsib) :: d13cca
00589 real(kind=dbl_kind), dimension(nsib) :: snow_veg
00590 real(kind=dbl_kind), dimension(nsib) :: snow_age
00591 real(kind=dbl_kind), dimension(nsib) :: snow_depth
00592 real(kind=dbl_kind), dimension(nsib) :: snow_mass
00593 real(kind=dbl_kind), dimension(nsib) :: tke
00594 real(kind=dbl_kind), dimension(nsib) :: sha
00595 real(kind=dbl_kind), dimension(nsib) :: capac1
00596 real(kind=dbl_kind), dimension(nsib) :: capac2
00597 real(kind=dbl_kind), dimension(nsib) :: coszbar
00598 real(kind=dbl_kind), dimension(nsib) :: dayflag
00599 real(kind=dbl_kind), dimension(12,nsib) :: tot_an
00600 real(kind=dbl_kind), dimension(12,nsib) :: tot_gpp
00601 real(kind=dbl_kind), dimension(12,nsib) :: tot_rc
00602 real(kind=dbl_kind), dimension(12,nsib) :: tot_fpar
00603 real(kind=dbl_kind), dimension(12,nsib,nsoil) :: tot_ss
00604 real(kind=dbl_kind), dimension(nsib,6) :: rst
00605 !itb_iso 
00606 real(kind=dbl_kind), dimension(nsib,6) :: d13c_auto
00607 !itb_iso 
00608 real(kind=dbl_kind), dimension(nsib,-nsnow+1:nsoil) :: deept
00609 real(kind=dbl_kind), dimension(nsib,-nsnow+1:nsoil) :: www_liq
00610 real(kind=dbl_kind), dimension(nsib,-nsnow+1:nsoil) :: www_ice
00611 real(kind=dbl_kind), dimension(nsib,nsnow) :: nz_snow
00612 real(kind=dbl_kind), dimension(nsib,nsnow) :: lz_snow
00613 real(kind=dbl_kind), dimension(nsib,nsnow) :: dz_snow
00614 
00615 
00616 integer(kind=int_kind),dimension(11) :: map_totals
00617 integer(kind=int_kind)               :: jday
00618 
00619 DATA map_totals/31,59,90,120,151,181,212,243,273,304,334/
00620 
00621     print*,'ic_path=',trim(ic_path)
00622 
00623     ! read in initial conditions (restart file)
00624     status = nf90_open( trim(ic_path), nf90_nowrite, ncid )
00625     if( status /= nf90_noerr ) call handle_err(status)
00626 
00627     print *,'\t opened ic file', trim(ic_path)
00628 
00629     !itb...read some scalars
00630     status = nf90_inq_varid( ncid, 'nsib', varid )
00631     status = nf90_get_var( ncid, varid, nsibt )
00632     print *, '\t nsib=',nsib, ' total nsib=',nsibt
00633     if(nsib /= nsibt) stop'INITIAL CONDITIONS: NSIB INCORRECT'
00634 
00635     status = nf90_inq_varid( ncid, 'nsoil', varid )
00636     status = nf90_get_var( ncid, varid, nsoilt )
00637     if(nsoil /= nsoilt) stop'INITIAL CONDITIONS: NSOIL INCORRECT'
00638 
00639     status = nf90_inq_varid( ncid, 'nsnow', varid )
00640     status = nf90_get_var( ncid, varid, nsnowt )
00641     if(nsnow /= nsnowt) stop'INITIAL CONDITIONS: NSNOW INCORRECT'
00642 
00643 !    status = nf90_inq_varid( ncid, 'subcount', varid )
00644 !    status = nf90_get_var( ncid, varid, subcountt )
00645 !    if(subcount /= subcountt) stop'INITIAL CONDITIONS: SUBCOUNT INCORRECT'
00646 
00647     status = nf90_inq_varid( ncid, 'version', varid )
00648     status = nf90_get_var( ncid, varid, versiont )
00649 
00650     status = nf90_inq_varid( ncid, 'nsecond', varid )
00651     status = nf90_get_var( ncid, varid, nsecond )
00652 print*,nsecond,time%sec_year
00653     if(nsecond /= time%sec_year) stop 'NSECONDS DOES NOT MATCH STARTTIME'
00654 
00655     !itb...read nsib vectors
00656 
00657     status = nf90_inq_varid( ncid, 'ta', varid )
00658     status = nf90_get_var( ncid, varid, ta )
00659 
00660     status = nf90_inq_varid( ncid, 'tc', varid )
00661     status = nf90_get_var( ncid, varid, tc )
00662 
00663     status = nf90_inq_varid( ncid, 'nsl', varid )
00664     status = nf90_get_var( ncid, varid, nsl )
00665 
00666     status = nf90_inq_varid( ncid, 'pco2a', varid )
00667     status = nf90_get_var( ncid, varid, pco2ap )
00668 
00669 !itb_ocs
00670     status = nf90_inq_varid( ncid, 'pcosa', varid )
00671     status = nf90_get_var( ncid, varid, pcosap )
00672 
00673     status = nf90_inq_varid( ncid, 'd13cca', varid )
00674     status = nf90_get_var( ncid, varid, d13cca )
00675 
00676     status = nf90_inq_varid( ncid, 'snow_veg', varid )
00677     status = nf90_get_var( ncid, varid, snow_veg )
00678 
00679     status = nf90_inq_varid( ncid, 'snow_age', varid )
00680     status = nf90_get_var( ncid, varid, snow_age )
00681 
00682     status = nf90_inq_varid( ncid, 'snow_depth', varid )
00683     status = nf90_get_var( ncid, varid, snow_depth )
00684 
00685     status = nf90_inq_varid( ncid, 'snow_mass', varid )
00686     status = nf90_get_var( ncid, varid, snow_mass )
00687 
00688     status = nf90_inq_varid( ncid, 'tke', varid )
00689     status = nf90_get_var( ncid, varid, tke )
00690 
00691     status = nf90_inq_varid( ncid, 'sha', varid )
00692     status = nf90_get_var( ncid, varid, sha )
00693 
00694     !itb...read some 2-d vars
00695     status = nf90_inq_varid( ncid, 'td', varid )
00696     status = nf90_get_var( ncid, varid, deept )
00697 
00698     status = nf90_inq_varid( ncid, 'www_liq', varid )
00699     status = nf90_get_var( ncid, varid, www_liq )
00700 
00701     status = nf90_inq_varid( ncid, 'www_ice', varid )
00702     status = nf90_get_var( ncid, varid, www_ice )
00703 
00704     !itb...now the rest...
00705     status = nf90_inq_varid( ncid, 'capac1', varid )
00706     status = nf90_get_var( ncid, varid, capac1 )
00707 
00708     status = nf90_inq_varid( ncid, 'capac2', varid )
00709     status = nf90_get_var( ncid, varid, capac2 )
00710 
00711     status = nf90_inq_varid( ncid, 'coszbar', varid )
00712     status = nf90_get_var( ncid, varid, coszbar )
00713 
00714     status = nf90_inq_varid( ncid, 'dayflag', varid )
00715     status = nf90_get_var( ncid, varid, dayflag )
00716 
00717     status = nf90_inq_varid( ncid, 'rst', varid )
00718     status = nf90_get_var( ncid, varid, rst )
00719 
00720 !itb_iso 
00721     status = nf90_inq_varid( ncid, 'd13c_auto', varid )
00722     status = nf90_get_var( ncid, varid, d13c_auto )
00723 !itb_iso 
00724 
00725     print*,'\t\t read in slabs...'
00726 
00727     !itb...don't know how to read slabs directly into the structure yet...
00728     start(1) = 1
00729     start(2) = 1
00730     finish(1) = nsib
00731     finish(2) = nsnow
00732     status = nf90_inq_varid( ncid, 'dzsnow', varid )
00733     status = nf90_get_var( ncid, varid, dz_snow, start, finish )
00734 
00735     status = nf90_inq_varid( ncid, 'lzsnow', varid )
00736     status = nf90_get_var( ncid, varid, lz_snow, start, finish )
00737 
00738     status = nf90_inq_varid( ncid, 'nzsnow', varid )
00739     status = nf90_get_var( ncid, varid, nz_snow, start, finish )
00740 
00741     ! read in tot_an and tot_ss for rolling respfactor
00742     status = nf90_inq_varid( ncid, 'tot_an', varid )
00743     status = nf90_get_var( ncid, varid, tot_an )
00744     status = nf90_inq_varid( ncid, 'tot_ss', varid )
00745     status = nf90_get_var( ncid, varid, tot_ss )
00746 
00747     status = nf90_inq_varid( ncid, 'tot_gpp', varid )
00748     if(status/=nf90_noerr) then
00749        print*, 'Error: problem reading tot_gpp from ic'
00750        print*, 'tot_gpp set to zero'
00751        tot_gpp=0._dbl_kind
00752     else
00753        status = nf90_get_var( ncid, varid, tot_gpp )
00754     endif
00755 
00756     status = nf90_inq_varid( ncid, 'tot_rc', varid )
00757     if(status/=nf90_noerr) then
00758        print*, 'Error: problem reading tot_rc from ic'
00759        print*, 'tot_rc set to zero'
00760        tot_rc=0._dbl_kind
00761     else
00762        status = nf90_get_var( ncid, varid, tot_rc )
00763     endif
00764 
00765     status = nf90_inq_varid( ncid, 'tot_fpar', varid )
00766     if(status/=nf90_noerr) then
00767        print*, 'Error: problem reading tot_fpar from ic'
00768        print*, 'tot_fpar set to zero'
00769        tot_fpar=0._dbl_kind
00770     else
00771        status = nf90_get_var( ncid, varid, tot_fpar )
00772     endif
00773 
00774     !itb...close the file
00775     status = nf90_close( ncid )
00776 
00777     print *,'\t\t load data into the structure'
00778 
00779     !itb...need to load these data into sibtype arrays
00780     do i = 1,subcount
00781         sib(i)%prog%ta = ta(subset(i))
00782         sib(i)%prog%tc = tc(subset(i))
00783         sib(i)%prog%nsl = nsl(subset(i))
00784         sib(i)%prog%pco2ap = pco2ap(subset(i))
00785 !itb_ocs
00786         sib(i)%prog%pcosap = pcosap(subset(i))
00787         sib(i)%prog%d13cca = d13cca(subset(i))
00788         sib(i)%prog%snow_veg = snow_veg(subset(i))
00789         sib(i)%prog%snow_age = snow_age(subset(i))
00790         sib(i)%prog%snow_depth = snow_depth(subset(i))
00791         sib(i)%prog%snow_mass = snow_mass(subset(i))
00792         sib(i)%prog%tke = max( tkemin, tke(subset(i)) )
00793         sib(i)%prog%sha = sha(subset(i))
00794         sib(i)%stat%coszbar = coszbar(subset(i))
00795         sib(i)%stat%dayflag = dayflag(subset(i))
00796         
00797         sib(i)%prog%capac(1) = capac1(subset(i))
00798         sib(i)%prog%capac(2) = capac2(subset(i))
00799         
00800         do k = 1, 12
00801             sib(i)%param%tot_an(k) = tot_an(k,subset(i))
00802             sib(i)%param%tot_gpp(k) = tot_gpp(k,subset(i))
00803             sib(i)%param%tot_rc(k) = tot_rc(k,subset(i))
00804             sib(i)%param%tot_fpar(k) = tot_fpar(k,subset(i))
00805             do j = 1, nsoil
00806                 sib(i)%param%tot_ss(k,j) = tot_ss(k,subset(i),j)
00807             enddo
00808         enddo
00809 
00810         do j = 1, 6
00811             sib(i)%prog%rst(j) = rst(subset(i),j)
00812 
00813 !itb_iso 
00814             sib(i)%param%d13c_auto(j) = d13c_auto(subset(i),j)
00815 !print*,i,j,subset(i),sib(i)%param%d13c_auto(j)
00816 !itb_iso 
00817 
00818         enddo
00819 
00820         do j = 1,nsnow
00821             k = j - 5
00822             sib(i)%prog%dz(k)      = dz_snow(subset(i),j)
00823             sib(i)%prog%node_z(k)  = nz_snow(subset(i),j)
00824             sib(i)%prog%layer_z(k-1) = lz_snow(subset(i),j)
00825         enddo
00826 
00827         do j=-nsnow+1,nsoil
00828             sib(i)%prog%td(j)      = deept(subset(i),j)
00829             sib(i)%prog%www_liq(j) = www_liq(subset(i),j)
00830             sib(i)%prog%www_ice(j) = www_ice(subset(i),j)
00831         enddo
00832 
00833     enddo   !subcount loop
00834 
00835     print *, '\t\t read in sib initial conditions'
00836 
00837 !itb...need to manipulate tot_an and tot_ss for restart/initial conditions...
00838     if(time%sec_year /= 0) then
00839        jday = nsecond/86400
00840        month_loop: do j = 1, 11
00841          if(jday == map_totals(j)) then
00842            do i=1,subcount
00843              sib(i)%param%tot_ss(j+1:13,:) = 0.0_dbl_kind
00844              sib(i)%param%tot_an(j+1:13)   = 0.0_dbl_kind
00845              sib(i)%param%tot_gpp(j+1:13)   = 0.0_dbl_kind
00846              sib(i)%param%tot_rc(j+1:13)   = 0.0_dbl_kind
00847              sib(i)%param%tot_fpar(j+1:13)   = 0.0_dbl_kind
00848            enddo
00849            exit month_loop
00850          endif
00851        enddo month_loop
00852     else 
00853      do i=1,subcount
00854        sib(i)%param%tot_ss(:,:) = 0.0_dbl_kind
00855        sib(i)%param%tot_an(:)   = 0.0_dbl_kind
00856        sib(i)%param%tot_gpp(:)   = 0.0_dbl_kind
00857        sib(i)%param%tot_rc(:)   = 0.0_dbl_kind
00858        sib(i)%param%tot_fpar(:)   = 0.0_dbl_kind
00859 
00860      enddo
00861     endif
00862 
00863 end subroutine read_ic
00864 
00865 !===============================================================================
00866 subroutine read_global_respfactor(sib,time)
00867 !===============================================================================
00868 ! reads global respfactor from netcdf file
00869 !
00870 ! Modifications:
00871 !  Kevin Schaefer fill respfactor any error (status>0 to status/=0) (11/11/04)
00872 !  Kevin Schaefer added autotrophic respfactor (5/9/05)
00873 !  Kevin Schaefer moved read single to separate routine (7/22/05)
00874 !  Ian Baker added d13c of heterotrophic respiration (6/12/09)
00875 !-------------------------------------------------------------------------------
00876 !
00877 #ifdef PGF
00878 use netcdf 
00879 use typeSizes
00880 #endif
00881 use kinds
00882 use sibtype
00883 use timetype
00884 use sib_const_module
00885 use sib_io_module
00886 !
00887 implicit none
00888 !
00889 ! input/output variables
00890 type(sib_t), dimension(subcount), intent(inout) :: sib
00891 type(time_struct), intent(in) :: time       ! time data
00892 !
00893 ! local variables
00894 integer(kind=int_kind) :: i,j, k, l,n,m      ! indeces
00895 integer(kind=int_kind) :: status      ! netcdf error number
00896 integer(kind=int_kind) :: ncid        ! netcdf file id number
00897 integer did_subcount ! dimension id - subcount
00898 integer varid        ! variable ID - subset index array
00899 integer loc_subcount ! local subcount from respfactor file
00900 integer(kind=int_kind), allocatable :: loc_subset(:) ! local subset index values from respfile
00901 logical good  ! flag indicating good data
00902 real(kind=dbl_kind), allocatable :: loc_data(:,:) ! temp var to read in data
00903 real(kind=dbl_kind), dimension(nsib,nsoil) :: het_respfac ! local heterotrophic resp factor
00904 real(kind=dbl_kind), dimension(nsib) :: auto_respfac  ! local autotrophic resp factor
00905 real(kind=dbl_kind), dimension(nsib) :: d13c_het  ! local d13C of heterotrophic resp
00906 !
00907 ! print message to screen
00908     print*, '\t read global respfactor: ', trim(co2_path)
00909 !
00910 ! clear out local respfactor arrays
00911     het_respfac=1.e36
00912     auto_respfac=1.e36
00913     d13c_het = 1.e36
00914 !
00915 ! open respfactor file
00916     status = nf90_open( trim(co2_path), nf90_nowrite, ncid )
00917     if(status/=nf90_noerr) call handle_err(status,'rf_read',1)
00918 !
00919 ! get dimension of index subcount
00920     status = nf90_inq_dimid(ncid, 'landpoints', did_subcount)
00921     if(status/=nf90_noerr) call handle_err(status,'rf_read',2)
00922     status = nf90_inquire_dimension( ncid, did_subcount, len=loc_subcount )
00923     if(status/=nf90_noerr) call handle_err(status,'rf_read',3)
00924 !
00925 ! allocate local subset index array and local data variable
00926     allocate(loc_subset(loc_subcount))
00927     allocate(loc_data(loc_subcount,nsoil))
00928 print*,'init_sibdrv: allocate:',loc_subcount,nsoil
00929 !
00930 ! get subset index array
00931     status = nf90_inq_varid( ncid, 'sibindex', varid )
00932     if(status/=nf90_noerr) call handle_err(status,'rf_read',4)
00933     status = nf90_get_var( ncid, varid, loc_subset )
00934     if(status/=nf90_noerr) call handle_err(status,'rf_read',5)
00935 !
00936 
00937 !
00938 ! get heterotrophic respfactor
00939     status = nf90_inq_varid( ncid, 'het_respfac', varid )
00940     if(status/=nf90_noerr) call handle_err(status,'rf_read',8)
00941     status = nf90_get_var( ncid, varid, loc_data )
00942     if(status/=nf90_noerr) call handle_err(status,'rf_read',9)
00943     do i=1,loc_subcount
00944        do j=1,nsoil
00945           het_respfac(loc_subset(i),j) = loc_data(i,j)
00946        enddo
00947     enddo
00948 
00949 ! get autotrophic respfactor
00950     status = nf90_inq_varid( ncid, 'auto_respfac', varid )
00951     if(status/=nf90_noerr) call handle_err(status,'rf_read',6)
00952     status = nf90_get_var( ncid, varid, loc_data(:,1) )
00953     if(status/=nf90_noerr) call handle_err(status,'rf_read',7)
00954     do i=1,loc_subcount
00955        auto_respfac(loc_subset(i)) = loc_data(i,1)
00956     enddo
00957 
00958 !
00959 ! get del 13C of heterotrophic respiration
00960     status = nf90_inq_varid( ncid, 'del13c_resp_het', varid )
00961     if(status/=nf90_noerr) call handle_err(status,'rf_read',66)
00962     if ( status /= 0 ) then
00963       print *, 'ERROR: del13C of heterotrophic respiration missing'
00964       print *, trim(co2_path)
00965       stop
00966     endif
00967     status = nf90_get_var( ncid, varid, loc_data(:,1) )
00968     if(status/=nf90_noerr) call handle_err(status,'rf_read',67)
00969     do i=1,loc_subcount
00970        d13c_het(loc_subset(i)) = loc_data(i,1)
00971     enddo
00972 !
00973 ! close respfactor file
00974     status = nf90_close( ncid )
00975     if(status/=nf90_noerr) call handle_err(status,'rf_read',10)
00976 !
00977 !  deallocate local variables
00978     deallocate(loc_subset)
00979     deallocate(loc_data)
00980 !
00981 ! copy respfactors into the variable tree
00982     do i=1,subcount
00983         do j=1,nsoil
00984             sib(i)%param%het_respfac(j) = het_respfac(subset(i),j)
00985         enddo
00986         sib(i)%param%auto_respfac = auto_respfac(subset(i))
00987         sib(i)%param%d13c_het     = d13c_het(subset(i))
00988     enddo
00989 !
00990 ! check for bad respfactors
00991     n=0
00992     m=0
00993     do i=1,subcount
00994       do j=1,nsoil
00995          good=.true.
00996          if(sib(i)%param%het_respfac(j)<=0.) good=.false.
00997          if(sib(i)%param%het_respfac(j)==1.e36) good=.false.
00998 !      print*, j,sib(i)%param%het_respfac(j)
00999          if(.not.good) then
01000             n=n+1
01001             sib(i)%param%het_respfac(j) = 1.0e-11_dbl_kind
01002          endif
01003       enddo
01004       good=.true.
01005       if(sib(i)%param%auto_respfac<=0.) good=.false.
01006       if(sib(i)%param%auto_respfac==1.e36) good=.false.
01007 !     print*, sib(i)%param%auto_respfac
01008       if(.not.good) then
01009          m=m+1
01010          sib(i)%param%auto_respfac = 1.0e-11_dbl_kind
01011       endif
01012     enddo
01013     if(n>0) print*, n, ' bad het respfac values replaced with 1.0e-11'
01014     if(m>0) print*, m, ' bad auto respfac values replaced with 1.0e-11'
01015 !
01016 end subroutine read_global_respfactor
01017 !
01018 !===============================================================================
01019 subroutine read_single_respfactor(sib,time)
01020 !===============================================================================
01021 ! reads respfactor for single point from external ascii file
01022 !
01023 ! Modifications:
01024 !  Kevin Schaefer created routine (7/22/05)
01025 !-------------------------------------------------------------------------------
01026 !
01027 use kinds
01028 use sibtype
01029 use timetype
01030 use sib_const_module
01031 use sib_io_module
01032 !
01033 implicit none
01034 !
01035 ! input/output variables
01036 type(sib_t), dimension(subcount), intent(inout) :: sib
01037 type(time_struct), intent(in) :: time       ! time data
01038 !
01039 ! local variables
01040 integer(kind=int_kind) :: i,j,k,l,n,m  ! indeces
01041 integer(kind=int_kind) :: status       ! error number
01042 logical good                           ! flag indicating good data
01043 !
01044 ! print message to screen
01045     print*, '\t read single respfactor: ', trim(co2_path)
01046 !
01047 ! Open SiB-CO2 respiration factor 
01048     open(unit=3, file=co2_path, form='formatted', status='old', iostat=status)
01049 !
01050 ! no file check
01051     if ( status /= 0 ) then
01052       print *, 'ERROR: respFactor file missing'
01053       print *, trim(co2_path)
01054       stop
01055     endif
01056 !
01057 ! read heterotrophic respfactor 
01058     do i = 1,nsoil
01059       read(3,*, iostat=status) sib(1)%param%het_respfac(i)
01060     enddo
01061 !
01062 ! no data check
01063     if ( status /= 0 ) then
01064       print *, 'ERROR: heterotrophic respFactor missing'
01065       print *, trim(co2_path)
01066       stop
01067     endif
01068 !
01069 ! read autotrophic respfactor
01070     read(3,*, iostat=status) sib(1)%param%auto_respfac
01071 !
01072 ! no data check
01073 !    if ( status /= 0 ) then
01074 !      print *, 'ERROR: autotrophic respFactor missing'
01075 !      print *, trim(co2_path)
01076 !      stop
01077 !    endif
01078 !    sib(1)%param%auto_respfac = 0.1e-5
01079 
01080 !
01081 !read del13C of heterotrophic respiration
01082    read(3,*, iostat=status) sib(1)%param%d13c_het
01083 
01084 print*,'d13c_het=',sib(1)%param%d13c_het
01085 !
01086 ! close respfactor file
01087     close(unit=3)
01088 !
01089 ! check for bad heterotrophic respfactors
01090     do j=1,nsoil
01091       good=.true.
01092       if(sib(1)%param%het_respfac(j)<=0.) good=.false.
01093       if(sib(1)%param%het_respfac(j)==1.e36) good=.false.
01094       if(.not.good) then
01095         print*, ' bad het respfac', sib(1)%param%het_respfac(j),' replaced with 1.0e-11 layer',j
01096         sib(1)%param%het_respfac(j) = 1.0e-11_dbl_kind
01097       endif
01098     enddo
01099 !
01100 ! check for bad autotrophic respfactor
01101     good=.true.
01102     if(sib(1)%param%auto_respfac<=0.) good=.false.
01103     if(sib(1)%param%auto_respfac==1.e36) good=.false.
01104     if(.not.good) then
01105       print*,' bad auto respfac',sib(1)%param%auto_respfac,' replaced with 1.0e-11'
01106       sib(1)%param%auto_respfac = 1.0e-11_dbl_kind
01107     endif
01108 !
01109 end subroutine read_single_respfactor
01110 !
01111 !===============================================================================
01112 subroutine soil_properties(sib)
01113 !===============================================================================
01114 ! calculates various soil parameters that do not change with time
01115 !
01116 ! Modifications:
01117 !  Kevin Schaefer moved soil layer calculations here from read_ic (10/27/04)
01118 !===============================================================================
01119 !
01120 use kinds
01121 use sibtype
01122 use sib_const_module
01123 implicit none
01124 
01125 ! parameters
01126 type(sib_t), dimension(subcount), intent(inout) :: sib
01127 
01128 ! local variables
01129 integer(kind=int_kind) :: i, j      ! (-) indeces
01130 real(kind=real_kind) :: tkm        ! (W/m K) mineral conductivity
01131 real(kind=real_kind) :: bd         ! (kg/m^3) bulk density of dry soil material
01132 real(kind=real_kind) :: kroot(12)  ! (?) root density extinction coeficient
01133 real(kind=real_kind) :: rootd(12)  ! (m) maximum rooting depth
01134 real(kind=real_kind) :: rtot       ! total root amount (-)
01135 real(kind=real_kind) :: totalroot  ! (?) total root density in soil column
01136 real(kind=real_kind) :: ztop       ! (-) normalized depth of soil layer top
01137 real(kind=real_kind) :: zbot       ! (-) normalized depth of soil layer bottom
01138 
01139 real(kind=real_kind) :: pot_fc     ! water potential at field capacity (J/kg)
01140 real(kind=real_kind) :: pot_wp     ! water potential at wilt point (J/kg)
01141 
01142 
01143 !
01144 ! assign values of root density profiles
01145 DATA KROOT/3.9,3.9,2.0,5.5,5.5,2.0,5.5,2.0,2.0,5.5,2.0,5.5/
01146 
01147 DATA rootd/30.0,5.0,5.0,5.0,5.0,3.5,3.5,3.5,5.0,1.0,10.0,3.5/
01148 
01149 
01150     do i = 1,subcount
01151         !Bio------------------------------------------------------------------
01152         !Bio   miscellaneous soil properties
01153         !Bio-------------------------------------------------------------------
01154 
01155 
01156 !itb...fixing field capacity and wilting point, based on %sand/%clay basis
01157 !itb...the stress performance is directly tied to FC and WP values. We are
01158 !itb...playing with the 'operating range' that gives us the best model 
01159 !itb...performance. 
01160 
01161         pot_fc = -15.0   ! field capacity (J/kg)
01162 
01163         pot_wp = -1500.0 ! wilt point (J/kg)
01164 
01165         sib(i)%param%fieldcap = sib(i)%param%poros*             &
01166                    ((pot_fc/9.8)/sib(i)%param%phsat) ** (-1.0 / sib(i)%param%bee)
01167 
01168         sib(i)%param%vwcmin = sib(i)%param%poros *               &
01169                  ((pot_wp/9.8)/sib(i)%param%phsat) ** (-1.0 / sib(i)%param%bee)
01170 
01171 
01172         
01173         tkm = ( 8.80*sib(i)%param%sandfrac + 2.92*sib(i)%param%clayfrac ) /    &
01174             ( sib(i)%param%sandfrac + sib(i)%param%clayfrac )
01175 
01176         bd = (1.0 - sib(i)%param%poros) * 2.7E3
01177 
01178         do j=1,nsoil
01179 
01180             sib(i)%param%tkmg(j)    = tkm**(1.0 - sib(i)%param%poros)
01181 
01182             sib(i)%param%tksatu(j)  = sib(i)%param%tkmg(j) * 0.57**sib(i)%param%poros 
01183 
01184             sib(i)%param%tkdry(j)   = (0.135*bd + 64.7) / (2.7E3 - 0.947*bd)
01185 
01186             sib(i)%param%csolid(j)  = (2.128*sib(i)%param%sandfrac       &
01187                 + 2.385*sib(i)%param%clayfrac)/      &
01188                 (sib(i)%param%sandfrac + sib(i)%param%clayfrac)*1.0E6
01189         enddo
01190 
01191         !Bio-------------------------------------------------------------------
01192         !Bio  compute soil layer values
01193         !Bio-------------------------------------------------------------------
01194 
01195         !itb...CLM uses a 'scalez' (0.025) factor to determine soil layer depths. 
01196         !itb...for now i'm going to use it as well...
01197         do j=1,nsoil
01198 
01199 
01200 !            sib(i)%prog%node_z(j) = 0.025*(exp(0.5*(j-0.5))-1.0)
01201 !itb...we change scalez to 0.73 to obtain a 10m deep soil
01202             sib(i)%prog%node_z(j) = 0.073*(exp(0.5*(j-0.5))-1.0)
01203 
01204         enddo
01205 
01206         sib(i)%prog%dz(1) = 0.5*(sib(i)%prog%node_z(1)+sib(i)%prog%node_z(2))
01207 
01208         do j=2,nsoil-1
01209             sib(i)%prog%dz(j) = 0.5*(sib(i)%prog%node_z(j+1)-  &
01210                 sib(i)%prog%node_z(j-1))
01211         enddo
01212 
01213         sib(i)%prog%dz(nsoil) = sib(i)%prog%node_z(nsoil) -   &
01214             sib(i)%prog%node_z(nsoil-1)
01215         sib(i)%prog%layer_z(0) = 0.0
01216 
01217         do j=1,nsoil-1
01218             sib(i)%prog%layer_z(j) = 0.5*(sib(i)%prog%node_z(j) +   &
01219                 sib(i)%prog%node_z(j+1))
01220         enddo
01221         sib(i)%prog%layer_z(nsoil) = sib(i)%prog%node_z(nsoil) +    &
01222             0.5*sib(i)%prog%dz(nsoil)
01223 
01224 
01225 !itb...seems like I'm always wanting info about soil layers...
01226 !        do j=1,nsoil
01227 !          print'(i5,3f15.5)',j,sib(i)%prog%layer_z(j),sib(i)%prog%node_z(j), &
01228 !                sib(i)%prog%dz(j)
01229 !        enddo
01230 !        stop
01231 
01232         !Bio-------------------------------------------------------------
01233         !Bio   compute root fractions for each layer
01234         !Bio-------------------------------------------------------------
01235 !
01236 ! total roots
01237         totalroot = (1.0 - exp(-kroot(int(sib(i)%param%biome))*     &
01238           sib(i)%prog%layer_z(nsoil))) / kroot(int(sib(i)%param%biome))
01239 !
01240 ! root fraction per soil layer
01241         ztop = 0.0
01242         do j=1,nsoil
01243             zbot = ztop + sib(i)%prog%dz(j)
01244             sib(i)%param%rootf(j) = (exp(-kroot(int(sib(i)%param%biome))*ztop) &
01245                 - exp(-kroot(int(sib(i)%param%biome))*zbot))/ &
01246                 (kroot(int(sib(i)%param%biome)) * totalroot)
01247 
01248 !          print*, j,sib(i)%param%rootf(j)
01249 
01250             ztop = zbot
01251 
01252         enddo
01253 
01254 
01255         !itb...quick patch to cover some underflow problems...
01256         if(sib(i)%param%vcover < sib(i)%param%zlt/10.0)  then
01257             sib(i)%param%vcover = sib(i)%param%vcover * 10.0
01258         endif
01259 
01260 
01261 
01262 !itb...modify for rooting depth...
01263 
01264        if (rootd(int(sib(i)%param%biome)) < sib(i)%prog%layer_z(nsoil)) then
01265 
01266          rtot = 0.0
01267 
01268          do j=1,nsoil
01269 
01270            if(sib(i)%prog%node_z(j) > rootd(int(sib(i)%param%biome))) then
01271 
01272              sib(i)%param%rootf(j) = 0.0_dbl_kind
01273 
01274            else
01275 
01276              rtot = rtot + sib(i)%param%rootf(j)
01277 
01278            endif
01279          enddo
01280 
01281          do j=1,nsoil
01282           
01283            sib(i)%param%rootf(j) = sib(i)%param%rootf(j) / rtot
01284 
01285          enddo
01286 
01287        endif
01288 
01289 !itb...end rooting depth calculation
01290 
01291 
01292 
01293 
01294     enddo  ! subcount loop
01295 
01296 
01297 end subroutine soil_properties
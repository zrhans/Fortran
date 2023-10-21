00001 subroutine sibdrv_read_ncep2( sib, time )
00002 
00003 !****--------------------------------------------------------------------
00004 !    This subroutines reads the forcing surface meteorological data 
00005 !    for the next time step.
00006 !    If required, it closes the current month's data file and opens the 
00007 !    next month's data file.
00008 
00009 !    precip, snowfall and radiation data are aggregated data over 6 hours
00010 !    They have been stored such that the data at time x represent the
00011 !    aggregation over the preceeding 6 hours.
00012 !    Thus data at 
00013 !     0 hours are the sum of the values 18-24 hours of the previous day
00014 !     6 hours are the sum of the values  0- 6 hours of the current day
00015 !    12 hours are the sum of the values  6-12 hours of the current day
00016 !    18 hours are the sum of the values 12-18 hours of the current day
00017 !
00018 !    Consequently new data are read for all variables at the same time.
00019 !    For all point data variables, e.g. temp., the data are for
00020 !    now + 6 hours. For precip and radiation data the new data
00021 !    are the aggregation over the next six hours.
00022 !
00023 ! Modifications:
00024 !  Kevin Schaefer moved conversion pascals to millibars from sibdrv_interp to here (8/16/04)
00025 !  Kevin Schaefer changed from tdew1/2 to sh1/2 because thats whats there (8/17/04)
00026 !****--------------------------------------------------------------------
00027 
00028 
00029 use sib_const_module, only:   &
00030     nsib,               &
00031     latsib,             &
00032     lonsib,             &
00033     subset,             &
00034     subcount
00035 
00036 use sib_io_module, only:   &
00037     dr_format,           &
00038     driver_id
00039 
00040 use physical_parameters, only:              &
00041     kapa => kappa,   &
00042     pi
00043 
00044 use kinds
00045 #ifdef PGF
00046 use netcdf
00047 use typeSizes
00048 #endif
00049 use sibtype
00050 use timetype
00051 
00052 type(sib_t), dimension(subcount), intent(inout) :: sib
00053 type(time_struct), intent(in) :: time
00054 
00055 real(kind=dbl_kind) :: pid180 
00056 real(kind=dbl_kind) :: cosz(nsib)
00057 
00058 integer(kind=int_kind) :: i, iyear, imon, iday, idoy, ihour, imin
00059 
00060 integer(kind=int_kind) ::  n
00061 
00062 character*80 filename
00063 character*7 gchar
00064 integer(kind=int_kind) :: nctimeid,ncyid,ncmid,nctdid,ncdoyid,nchid
00065 integer(kind=int_kind), dimension(2) :: mstart,mcount
00066 
00067 integer(kind=int_kind) :: nct2mid ! Total Cloud Cover
00068 integer(kind=int_kind) :: nctccid ! Total Cloud Cover
00069 integer(kind=int_kind) :: ncswdid ! Surface solar radiation downwards           
00070 integer(kind=int_kind) :: ncldwid ! Surface thermal radiation downwards
00071 integer(kind=int_kind) :: ncuwdid ! U-wind at 10 m
00072 integer(kind=int_kind) :: ncvwdid ! V-wind at 10 m
00073 integer(kind=int_kind) :: ncshid  ! humidity at 2 m
00074 integer(kind=int_kind) :: ncsfpid ! Log Surface Pressure
00075 integer(kind=int_kind) :: nclspid ! Large Scale Precipitation
00076 integer(kind=int_kind) :: nccvpid ! Convective Precipitation
00077 integer(kind=int_kind) :: ncsflid ! Snow Fall
00078 
00079 real(kind=real_kind), dimension(nsib) :: t2m ! Total Cloud Cover
00080 real(kind=real_kind), dimension(nsib) :: tcc ! Total Cloud Cover
00081 real(kind=real_kind), dimension(nsib) :: swd ! Surface solar radiation downwards           
00082 real(kind=real_kind), dimension(nsib) :: ldw ! Surface thermal radiation downwards
00083 real(kind=real_kind), dimension(nsib) :: sh  ! humidity at 2 m
00084 real(kind=real_kind), dimension(nsib) :: sfp ! Log Surface Pressure
00085 real(kind=real_kind), dimension(nsib) :: lsp ! Large Scale Precipitation
00086 real(kind=real_kind), dimension(nsib) :: cvp ! Convective Precipitation
00087 real(kind=real_kind), dimension(nsib) :: sfl ! Snow Fall
00088 
00089 integer(kind=int_kind) :: status
00090 real(kind=real_kind) :: xtime,xyear,xmonth,xdoy,xday,xhour
00091 real(kind=real_kind), dimension(nsib) :: xx,uwd,vwd
00092 
00093 character(len=13) :: subname
00094 data subname/'sibdrv_read '/
00095 
00096 
00097    !*** Storing previous time steps data
00098     do i=1,subcount
00099         sib(i)%prog%ps1       = sib(i)%prog%ps2
00100         sib(i)%prog%tm1       = sib(i)%prog%tm2
00101         sib(i)%prog%tcc1      = sib(i)%prog%tcc2
00102         sib(i)%prog%sh1       = sib(i)%prog%sh2
00103         sib(i)%prog%spdm1     = sib(i)%prog%spdm2
00104         sib(i)%prog%lspr1     = sib(i)%prog%lspr2
00105         sib(i)%prog%cupr1     = sib(i)%prog%cupr2
00106         sib(i)%prog%dlwbot1   = sib(i)%prog%dlwbot2
00107         sib(i)%prog%sw_dwn1 = sib(i)%prog%sw_dwn2
00108     enddo
00109 
00110     ! switch files if needed
00111     if ( time%switch_driver ) then
00112         status = nf90_close( driver_id )
00113 
00114         write( filename, dr_format ) time%driver_year, time%driver_month
00115         status = nf90_open( trim(filename), nf90_nowrite, driver_id )
00116         if(status/=nf90_noerr) call handle_err(status,'read_ncep2',1)
00117     endif
00118 
00119     ! Read new driver data
00120     !print*, subname,tau,nextsecond,nextday,nextdoy,       &
00121     !    nmonth,nyear,nextmonth,nextyear
00122 
00123     ! check time values in driver data file
00124 !    status = nf90_inq_varid( driver_id,   'time', nctimeid )
00125 !    if ( status /= nf90_noerr ) call handle_err( status )
00126     status = nf90_inq_varid( driver_id,   'year', ncyid )
00127     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',2)
00128     status = nf90_inq_varid( driver_id,   'month',ncmid )
00129     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',3)
00130     status = nf90_inq_varid( driver_id,   'doy',  ncdoyid )
00131     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',4)
00132     status = nf90_inq_varid( driver_id,   'day',  nctdid )
00133     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',5)
00134     status = nf90_inq_varid( driver_id,   'hour', nchid )
00135     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',6)
00136 
00137     ! read time
00138     mstart(1) = time%driver_recnum
00139 !    status = nf90_get_var( driver_id, nctimeid, xtime, mstart(1:1) )
00140 !    if ( status /= nf90_noerr ) call handle_err( status )
00141     status = nf90_get_var( driver_id, ncyid,    xyear, mstart(1:1) )
00142     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',7)
00143     status = nf90_get_var( driver_id, ncmid,   xmonth, mstart(1:1) )
00144     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',8)
00145     status = nf90_get_var( driver_id, ncdoyid,   xdoy, mstart(1:1) )
00146     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',9)
00147     status = nf90_get_var( driver_id, nctdid,    xday, mstart(1:1) )
00148     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',10)
00149     status = nf90_get_var( driver_id, nchid,    xhour, mstart(1:1) )
00150     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',11)
00151 
00152     ihour=xhour
00153     iday =xday
00154     idoy =xdoy
00155     imon =xmonth
00156     iyear=xyear
00157     imin=0
00158 
00159 !    print*,subname,'Time level in file: ',ihour,iday,idoy,imon,iyear
00160 
00161     !* Get veriable id's
00162     status = nf90_inq_varid( driver_id, 't2m', nct2mid ) ! Temperature at 2 m
00163     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',12)
00164     status = nf90_inq_varid( driver_id, 'tcc', nctccid ) ! Total Cloud Cover
00165     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',13)
00166     status = nf90_inq_varid( driver_id, 'swd', ncswdid ) ! Surface solar rad downwards
00167     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',14)
00168     status = nf90_inq_varid( driver_id, 'lwd', ncldwid ) ! Surface thermal rad down
00169     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',15)
00170     status = nf90_inq_varid( driver_id, 'uwd', ncuwdid ) ! U-wind at 10 m
00171     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',16)
00172     status = nf90_inq_varid( driver_id, 'vwd', ncvwdid ) ! V-wind at 10 m
00173     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',17)
00174     status = nf90_inq_varid( driver_id, 'shum', ncshid ) ! humidity at 2 m
00175     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',18)
00176     status = nf90_inq_varid( driver_id, 'sfp', ncsfpid ) ! Log Surface Pressure
00177     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',19)
00178     status = nf90_inq_varid( driver_id, 'lsp', nclspid ) ! Large Scale Precipitation
00179     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',20)
00180     status = nf90_inq_varid( driver_id, 'cvp', nccvpid ) ! Convective Precipitation
00181     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',21)
00182     status = nf90_inq_varid( driver_id, 'sfl', ncsflid ) ! Snow Fall
00183     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',22)
00184 
00185     !* get data
00186     mstart=(/1,time%driver_recnum/); mcount=(/nsib,1/)
00187     status = nf90_get_var( driver_id, nct2mid, t2m,    & !Temperature at 2 m
00188          mstart,  mcount )
00189     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',23)
00190     status = nf90_get_var( driver_id, nctccid, tcc,    & !Total Cloud Cover
00191          mstart,  mcount )
00192     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',24)
00193     status = nf90_get_var( driver_id, ncswdid, swd,    & !Surface solar rad downwards
00194          mstart,  mcount )
00195     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',25)
00196     status = nf90_get_var( driver_id, ncldwid, ldw,    & !Surface thermal rad downwards
00197          mstart,  mcount )
00198     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',26)
00199     status = nf90_get_var( driver_id, ncuwdid, uwd,    & ! U-wind at 10 m
00200          mstart,  mcount )
00201     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',27)
00202     status = nf90_get_var( driver_id, ncvwdid, vwd,    & ! V-wind at 10 m
00203          mstart,  mcount )
00204     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',28)
00205     status = nf90_get_var( driver_id, ncshid, sh,      & ! humidity at 2 m
00206          mstart,  mcount )
00207     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',29)
00208     status = nf90_get_var( driver_id, ncsfpid, sfp,    & ! Surface Pressure
00209          mstart,  mcount )
00210     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',30)
00211     status = nf90_get_var( driver_id, nclspid, lsp,    & ! Large Scale Precipitation
00212          mstart,  mcount )
00213     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',31)
00214     status = nf90_get_var( driver_id, nccvpid, cvp,    & ! Convective Precipitation
00215          mstart,  mcount )
00216     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',32)
00217     status = nf90_get_var(driver_id, ncsflid, xx,    & ! Snow Fall
00218          mstart,  mcount )
00219     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',33)
00220 
00221 
00222     do i=1,subcount
00223         ! pull out landpoints in subdomain
00224         sib(i)%prog%tm2 = t2m(subset(i))
00225         sib(i)%prog%tcc2 = tcc(subset(i))
00226         sib(i)%prog%sw_dwn2 = swd(subset(i))
00227         sib(i)%prog%dlwbot2 = ldw(subset(i))
00228         sib(i)%prog%sh2 = sh(subset(i))
00229         sib(i)%prog%ps2 = sfp(subset(i))
00230         sib(i)%prog%lspr2 = lsp(subset(i))
00231         sib(i)%prog%cupr2 = cvp(subset(i))
00232     
00233         ! scale radiation to w/m2
00234         !sib(i)%prog%sw_dwn2 = sib(i)%prog%sw_dwn2/time%driver_step
00235         !sib(i)%prog%dlwbot2 = sib(i)%prog%dlwbot2/time%driver_step
00236         if ( sib(i)%prog%sw_dwn2 < 0 ) sib(i)%prog%sw_dwn2 = 0.0
00237         if ( sib(i)%prog%dlwbot2 < 0 ) sib(i)%prog%dlwbot2 = 0.0
00238         
00239         ! convert total cloud cover to fraction
00240         sib(i)%prog%tcc2 = sib(i)%prog%tcc2 * 0.01
00241 
00242         ! 10 m wind
00243         sib(i)%prog%spdm2=SQRT(uwd(subset(i))*uwd(subset(i))+vwd(subset(i))*vwd(subset(i)))
00244 
00245         ! add snowfall to large scale precip and let SiB decide about snow.
00246         sib(i)%prog%lspr2 = sib(i)%prog%lspr2+xx(subset(i))
00247         ! convert to mm
00248         sib(i)%prog%lspr2 = (sib(i)%prog%lspr2-sib(i)%prog%cupr2)*time%driver_step
00249         sib(i)%prog%cupr2 = sib(i)%prog%cupr2*time%driver_step
00250         ! make sure precip > 0
00251         if ( sib(i)%prog%lspr2 < 0.0 ) sib(i)%prog%lspr2 = 0.0
00252         if ( sib(i)%prog%cupr2 < 0.0 ) sib(i)%prog%cupr2 = 0.0
00253 
00254         ! Conversion Pa -> hPa (pascals to millibars)
00255         sib(i)%prog%ps2 = sib(i)%prog%ps2 * 0.01
00256     enddo
00257 
00258 !    print*,subname,'New driver data read ',ihour,iday,imon,iyear
00259 !    print*,'------------------------------------------------------'
00260 !    print*,'Extrema of new input data'
00261 !    print*, minval(sib%prog%tm2      ),maxval(sib%prog%tm2  ),' Temperature'
00262 !    print*, minval(sib%prog%tcc2     ),maxval(sib%prog%tcc2),' Total cloudiness'
00263 !    print*, minval(sib%prog%sh2    ),maxval(sib%prog%sh2 ),' dew point'
00264 !    print*, minval(sib%prog%spdm2    ),maxval(sib%prog%spdm2),' Surface wind'
00265 !    print*, minval(sib%prog%ps2      ),maxval(sib%prog%ps2 ),' Pressure'
00266 !    print*, minval(sib%prog%dlwbot2  ),maxval(sib%prog%dlwbot2),  &
00267 !        ' Long wave down'
00268 !    print*, minval(sib%prog%lspr2    ),maxval(sib%prog%lspr2),' Large sc precip'
00269 !    print*, minval(sib%prog%cupr2    ),maxval(sib%prog%cupr2),' Convective '
00270 !    print*, minval(sib%prog%sw_dwn2),maxval(sib%prog%sw_dwn2),  &
00271 !        ' Short wave down'
00272 !    print*,'-----------------------------------------------------'
00273 
00274 
00275 end subroutine sibdrv_read_ncep2
00276 !
00277 !-------------------------------------------------------
00278 subroutine sibdrv_read_ncep1(sib, time)
00279 !-------------------------------------------------------
00280 ! This subroutines reads the forcing surface meteorological
00281 ! data from the NCEP1 2x2 reanalysis for the next driver
00282 ! data time step.  If required, it closes the current
00283 ! month's data file and opens the next month's data file.
00284 !
00285 ! precip, snowfall and radiation data are aggregated data over 6 hours
00286 ! They have been stored such that the data at time x represent the
00287 ! aggregation over the preceeding 6 hours.  Thus data at 
00288 !     0 hours are the sum of the values 18-24 hours of the previous day
00289 !     6 hours are the sum of the values  0- 6 hours of the current day
00290 !    12 hours are the sum of the values  6-12 hours of the current day
00291 !    18 hours are the sum of the values 12-18 hours of the current day
00292 !
00293 ! Consequently new data are read for all variables at the same time.
00294 ! For all point data variables, e.g. temp., the data are for
00295 ! now + 6 hours. For precip and radiation data the new data
00296 ! are the aggregation over the next six hours.
00297 !
00298 ! Modifications:
00299 !  Kevin Schaefer created subroutine from sibdrv_read_ncep1(8/12/04)
00300 !  Kevin Schaefer added check on zero humidity (8/16/04)
00301 !-------------------------------------------------------
00302 !
00303 use sib_const_module, only:   &
00304     nsib,               &
00305     subset,             &
00306     subcount
00307 use sib_io_module, only:   &
00308     dr_format,           &
00309     driver_id
00310 use kinds
00311 #ifdef PGF
00312 use netcdf
00313 use typeSizes
00314 #endif
00315 use sibtype
00316 use timetype
00317 
00318 !
00319 ! define local variables
00320 type(sib_t), dimension(subcount), intent(inout) :: sib  ! main sib variable tree
00321 type(time_struct), intent(in) :: time         ! sibdrive time variables
00322 integer(kind=int_kind) ::  i,j,k,l,m,n       ! generic indeces
00323 character*80 filename                                   ! netcdf driver data file name
00324 integer(kind=int_kind) :: varid              ! netcdf variable id number
00325 integer(kind=int_kind), dimension(2) :: mstart ! starting index location for driver data
00326 integer(kind=int_kind), dimension(2) :: mcount ! number of driver data points to read
00327 real(kind=real_kind), dimension(nsib) :: var  ! generic driver data variable
00328 real(kind=real_kind), dimension(nsib) :: uwd  ! u (zonal) wind component
00329 real(kind=real_kind), dimension(nsib) :: vwd  ! v (meridional) wind component
00330 integer(kind=int_kind) :: status             ! netcdf operation status variable
00331 character(len=13) :: subname                            ! subroutine name
00332 !
00333 ! set subroutine name
00334 data subname/'read_ncep1'/
00335 !
00336 ! print message
00337 !print*, 'read new NCEP1 Driver data', time%hour, time%driver_recnum
00338 !
00339 !switch previous and next driver data
00340     do i=1,subcount
00341         sib(i)%prog%ps1     = sib(i)%prog%ps2
00342         sib(i)%prog%tm1     = sib(i)%prog%tm2
00343         sib(i)%prog%sh1     = sib(i)%prog%sh2
00344         sib(i)%prog%spdm1   = sib(i)%prog%spdm2
00345         sib(i)%prog%lspr1   = sib(i)%prog%lspr2
00346         sib(i)%prog%cupr1   = sib(i)%prog%cupr2
00347         sib(i)%prog%dlwbot1 = sib(i)%prog%dlwbot2
00348         sib(i)%prog%sw_dwn1 = sib(i)%prog%sw_dwn2
00349     enddo
00350 !
00351 ! switch files if needed
00352     if (time%switch_driver) then
00353 !
00354 ! close old file
00355         status=nf90_close(driver_id)
00356 !
00357 ! new file name
00358         write(filename, dr_format) time%driver_year, time%driver_month
00359         print*, '\tswitch drvr to ', trim(filename)
00360 !
00361 ! open new file
00362         status=nf90_open(trim(filename), nf90_nowrite, driver_id)
00363         if(status/=nf90_noerr) call handle_err(status,'read_ncep1',1)
00364     endif
00365 !
00366 ! set starting point for reading driver data file
00367     mstart=(/1,time%driver_recnum/); mcount=(/nsib,1/)
00368 !
00369 !-------------------------------------------------------
00370 ! Temperature at 2 m
00371     status=nf90_inq_varid(driver_id, 'tmp', varid)
00372     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',2)
00373     status=nf90_get_var(driver_id, varid, var, mstart, mcount)
00374     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',3)
00375 !
00376 ! subgrid the driver data
00377     do i=1,subcount
00378         sib(i)%prog%tm2 = var(subset(i))
00379     enddo
00380 !
00381 !-------------------------------------------------------
00382 ! Surface solar radiation downwards
00383     status=nf90_inq_varid(driver_id, 'dswrf', varid)
00384     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',4)
00385     status=nf90_get_var(driver_id, varid, var, mstart, mcount)
00386     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',5)
00387 !
00388 ! subgrid the driver data
00389     do i=1,subcount
00390         sib(i)%prog%sw_dwn2 = var(subset(i))
00391     enddo
00392 !
00393 !-------------------------------------------------------
00394 ! Surface thermal (infrared) radiation down
00395     status=nf90_inq_varid(driver_id, 'dlwrf', varid)
00396     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',6)
00397     status=nf90_get_var(driver_id, varid, var, mstart, mcount)
00398     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',7)
00399 !
00400 ! subgrid the driver data
00401     do i=1,subcount
00402         sib(i)%prog%dlwbot2 = var(subset(i))
00403     enddo
00404 !
00405 !-------------------------------------------------------
00406 ! total wind speed
00407 !
00408 ! U-wind at 10 m
00409     status=nf90_inq_varid(driver_id, 'ugrd', varid)
00410     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',8)
00411     status=nf90_get_var(driver_id, varid, uwd, mstart, mcount)
00412     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',9)
00413 !
00414 ! V-wind at 10 m
00415     status=nf90_inq_varid(driver_id, 'vgrd', varid)
00416     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',10)
00417     status=nf90_get_var(driver_id, varid, vwd, mstart, mcount)
00418     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',11)
00419 !
00420 ! subgrid the driver data
00421 ! combine winds into total wind
00422     do i=1,subcount
00423         sib(i)%prog%spdm2 = uwd(subset(i))*uwd(subset(i))+ vwd(subset(i))*vwd(subset(i))
00424         sib(i)%prog%spdm2 = sqrt(sib(i)%prog%spdm2)
00425     enddo
00426 !
00427 !-------------------------------------------------------
00428 ! specific humidity at 2 m
00429     status=nf90_inq_varid(driver_id, 'spfh', varid)
00430     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',12)
00431     status=nf90_get_var(driver_id, varid, var, mstart, mcount)
00432     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',13)
00433 !
00434 ! subgrid the driver data
00435     do i=1,subcount
00436         sib(i)%prog%sh2 = var(subset(i))
00437         if(sib(i)%prog%sh2==0.) sib(i)%prog%sh2=1.e-4
00438     enddo
00439 !
00440 !-------------------------------------------------------
00441 ! Surface Pressure
00442     status=nf90_inq_varid(driver_id, 'pres', varid)
00443     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',14)
00444     status=nf90_get_var(driver_id, varid, var, mstart, mcount)
00445     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',15)
00446 !
00447 ! subgrid the driver data
00448 ! convert pressure from pascals to millibars
00449     do i=1,subcount
00450         sib(i)%prog%ps2 = var(subset(i))*.01
00451     enddo
00452 !
00453 !-------------------------------------------------------
00454 ! Large Scale Precipitation
00455     status=nf90_inq_varid(driver_id, 'prate', varid)
00456     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',16)
00457     status=nf90_get_var(driver_id, varid, var, mstart, mcount)
00458     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',17)
00459 !
00460 ! subgrid the driver data
00461 ! convert precipitation from kg m-2 s-2 to milimeters
00462     do i=1,subcount
00463         sib(i)%prog%lspr2 = var(subset(i))*time%driver_step
00464     enddo
00465 !
00466 !-------------------------------------------------------
00467 ! Convective Precipitation
00468     status=nf90_inq_varid(driver_id, 'cprat', varid)
00469     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',18)
00470     status=nf90_get_var(driver_id, varid, var, mstart, mcount)
00471     if(status/=nf90_noerr) call handle_err(status,'read_ncep1',19)
00472 !
00473 ! subgrid the driver data
00474 ! convert precipitation from kg m-2 s-2 to milimeters
00475     do i=1,subcount
00476         sib(i)%prog%cupr2 = var(subset(i))*time%driver_step
00477     enddo
00478 !
00479 !-------------------------------------------------------
00480 ! some checks on the driver data
00481     do i=1,subcount
00482 !
00483 ! check for positive radiation
00484         if ( sib(i)%prog%sw_dwn2 < 0 ) sib(i)%prog%sw_dwn2 = 0.0
00485         if ( sib(i)%prog%dlwbot2 < 0 ) sib(i)%prog%dlwbot2 = 0.0
00486 !
00487 ! check for positive precipitation
00488         if ( sib(i)%prog%lspr2 < 0.0 ) sib(i)%prog%lspr2 = 0.0
00489         if ( sib(i)%prog%cupr2 < 0.0 ) sib(i)%prog%cupr2 = 0.0
00490     enddo
00491 !
00492 end subroutine sibdrv_read_ncep1
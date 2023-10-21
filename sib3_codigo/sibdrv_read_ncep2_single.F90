00001 subroutine sibdrv_read_ncep2_single( sib, time )
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
00052 implicit none
00053 
00054 
00055 type(sib_t), dimension(subcount), intent(inout) :: sib
00056 type(time_struct), intent(in) :: time
00057 
00058 
00059 
00060 
00061 integer(kind=int_kind) :: i, iyear, imon, iday, idoy, ihour, imin
00062 
00063 integer(kind=int_kind) ::  n
00064 
00065 character*80 filename
00066 character*7 gchar
00067 integer(kind=int_kind) :: nctimeid,ncyid,ncmid,nctdid,ncdoyid,nchid
00068 integer(kind=int_kind), dimension(2) :: mstart,mcount
00069 
00070 integer(kind=int_kind) :: nct2mid ! Total Cloud Cover
00071 integer(kind=int_kind) :: nctccid ! Total Cloud Cover
00072 integer(kind=int_kind) :: ncswdid ! Surface solar radiation downwards           
00073 integer(kind=int_kind) :: ncldwid ! Surface thermal radiation downwards
00074 integer(kind=int_kind) :: ncuwdid ! U-wind at 10 m
00075 integer(kind=int_kind) :: ncvwdid ! V-wind at 10 m
00076 integer(kind=int_kind) :: ncshid  ! humidity at 2 m
00077 integer(kind=int_kind) :: ncsfpid ! Log Surface Pressure
00078 integer(kind=int_kind) :: nclspid ! Large Scale Precipitation
00079 integer(kind=int_kind) :: nccvpid ! Convective Precipitation
00080 integer(kind=int_kind) :: ncsflid ! Snow Fall
00081 
00082 !itb_ncep_single
00083 integer(kind=int_kind),parameter :: nsib_global=14637
00084 integer(kind=int_kind) :: ipoint
00085 !itb_ncep_single
00086 
00087 real(kind=real_kind), dimension(nsib_global) :: t2m ! Total Cloud Cover
00088 real(kind=real_kind), dimension(nsib_global) :: tcc ! Total Cloud Cover
00089 real(kind=real_kind), dimension(nsib_global) :: swd ! Surface solar radiation downwards           
00090 real(kind=real_kind), dimension(nsib_global) :: ldw ! Surface thermal radiation downwards
00091 real(kind=real_kind), dimension(nsib_global) :: sh  ! humidity at 2 m
00092 real(kind=real_kind), dimension(nsib_global) :: sfp ! Log Surface Pressure
00093 real(kind=real_kind), dimension(nsib_global) :: lsp ! Large Scale Precipitation
00094 real(kind=real_kind), dimension(nsib_global) :: cvp ! Convective Precipitation
00095 real(kind=real_kind), dimension(nsib_global) :: sfl ! Snow Fall
00096 
00097 integer(kind=int_kind) :: status
00098 real(kind=real_kind) :: xtime,xyear,xmonth,xdoy,xday,xhour
00099 real(kind=real_kind), dimension(nsib_global) :: xx,uwd,vwd
00100 
00101 
00102 
00103 character(len=23) :: subname
00104 data subname/'sibdrv_read_single_ncep '/
00105 
00106 
00107 
00108 
00109 
00110 
00111 
00112 
00113 
00114    !*** Storing previous time steps data
00115     do i=1,subcount
00116         sib(i)%prog%ps1       = sib(i)%prog%ps2
00117         sib(i)%prog%tm1       = sib(i)%prog%tm2
00118         sib(i)%prog%tcc1      = sib(i)%prog%tcc2
00119         sib(i)%prog%sh1       = sib(i)%prog%sh2
00120         sib(i)%prog%spdm1     = sib(i)%prog%spdm2
00121         sib(i)%prog%lspr1     = sib(i)%prog%lspr2
00122         sib(i)%prog%cupr1     = sib(i)%prog%cupr2
00123         sib(i)%prog%dlwbot1   = sib(i)%prog%dlwbot2
00124         sib(i)%prog%sw_dwn1 = sib(i)%prog%sw_dwn2
00125 
00126 !itb_ncep_single...
00127         ipoint = sib(i)%param%pt_1x1
00128 !        print*,'ipoint=',ipoint
00129 
00130     enddo
00131 
00132     ! switch files if needed
00133     if ( time%switch_driver ) then
00134         status = nf90_close( driver_id )
00135 
00136         write( filename, dr_format ) time%driver_year, time%driver_month
00137 
00138 
00139         status = nf90_open( trim(filename), nf90_nowrite, driver_id )
00140         if(status/=nf90_noerr) call handle_err(status,'read_ncep2',1)
00141     endif
00142 
00143     ! Read new driver data
00144 
00145 
00146     ! check time values in driver data file
00147 !    status = nf90_inq_varid( driver_id,   'time', nctimeid )
00148 !    if ( status /= nf90_noerr ) call handle_err( status )
00149     status = nf90_inq_varid( driver_id,   'year', ncyid )
00150     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',2)
00151     status = nf90_inq_varid( driver_id,   'month',ncmid )
00152     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',3)
00153     status = nf90_inq_varid( driver_id,   'doy',  ncdoyid )
00154     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',4)
00155     status = nf90_inq_varid( driver_id,   'day',  nctdid )
00156     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',5)
00157     status = nf90_inq_varid( driver_id,   'hour', nchid )
00158     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',6)
00159 
00160     ! read time
00161     mstart(1) = time%driver_recnum
00162 !    status = nf90_get_var( driver_id, nctimeid, xtime, mstart(1:1) )
00163 !    if ( status /= nf90_noerr ) call handle_err( status )
00164     status = nf90_get_var( driver_id, ncyid,    xyear, mstart(1:1) )
00165     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',7)
00166     status = nf90_get_var( driver_id, ncmid,   xmonth, mstart(1:1) )
00167     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',8)
00168     status = nf90_get_var( driver_id, ncdoyid,   xdoy, mstart(1:1) )
00169     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',9)
00170     status = nf90_get_var( driver_id, nctdid,    xday, mstart(1:1) )
00171     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',10)
00172     status = nf90_get_var( driver_id, nchid,    xhour, mstart(1:1) )
00173     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',11)
00174 
00175     ihour=xhour
00176     iday =xday
00177     idoy =xdoy
00178     imon =xmonth
00179     iyear=xyear
00180     imin=0
00181 
00182 !    print*,subname,'Time level in file: ',ihour,iday,idoy,imon,iyear
00183 !    print*,'driver_recnum=',time%driver_recnum
00184 !    print*,'--------'
00185 
00186     !* Get variable id's
00187     status = nf90_inq_varid( driver_id, 't2m', nct2mid ) ! Temperature at 2 m
00188     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',12)
00189     status = nf90_inq_varid( driver_id, 'tcc', nctccid ) ! Total Cloud Cover
00190     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',13)
00191     status = nf90_inq_varid( driver_id, 'swd', ncswdid ) ! Surface solar rad downwards
00192     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',14)
00193     status = nf90_inq_varid( driver_id, 'lwd', ncldwid ) ! Surface thermal rad down
00194     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',15)
00195     status = nf90_inq_varid( driver_id, 'uwd', ncuwdid ) ! U-wind at 10 m
00196     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',16)
00197     status = nf90_inq_varid( driver_id, 'vwd', ncvwdid ) ! V-wind at 10 m
00198     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',17)
00199     status = nf90_inq_varid( driver_id, 'shum', ncshid ) ! humidity at 2 m
00200     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',18)
00201     status = nf90_inq_varid( driver_id, 'sfp', ncsfpid ) ! Log Surface Pressure
00202     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',19)
00203     status = nf90_inq_varid( driver_id, 'lsp', nclspid ) ! Large Scale Precipitation
00204     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',20)
00205     status = nf90_inq_varid( driver_id, 'cvp', nccvpid ) ! Convective Precipitation
00206     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',21)
00207     status = nf90_inq_varid( driver_id, 'sfl', ncsflid ) ! Snow Fall
00208     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',22)
00209 
00210 
00211     !* get data
00212     mstart=(/1,time%driver_recnum/); mcount=(/nsib_global,1/)
00213     status = nf90_get_var( driver_id, nct2mid, t2m,    & !Temperature at 2 m
00214          mstart,  mcount )
00215     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',23)
00216     status = nf90_get_var( driver_id, nctccid, tcc,    & !Total Cloud Cover
00217          mstart,  mcount )
00218     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',24)
00219     status = nf90_get_var( driver_id, ncswdid, swd,    & !Surface solar rad downwards
00220          mstart,  mcount )
00221     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',25)
00222     status = nf90_get_var( driver_id, ncldwid, ldw,    & !Surface thermal rad downwards
00223          mstart,  mcount )
00224     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',26)
00225     status = nf90_get_var( driver_id, ncuwdid, uwd,    & ! U-wind at 10 m
00226          mstart,  mcount )
00227     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',27)
00228     status = nf90_get_var( driver_id, ncvwdid, vwd,    & ! V-wind at 10 m
00229          mstart,  mcount )
00230     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',28)
00231     status = nf90_get_var( driver_id, ncshid, sh,      & ! humidity at 2 m
00232          mstart,  mcount )
00233     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',29)
00234     status = nf90_get_var( driver_id, ncsfpid, sfp,    & ! Surface Pressure
00235          mstart,  mcount )
00236     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',30)
00237     status = nf90_get_var( driver_id, nclspid, lsp,    & ! Large Scale Precipitation
00238          mstart,  mcount )
00239     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',31)
00240     status = nf90_get_var( driver_id, nccvpid, cvp,    & ! Convective Precipitation
00241          mstart,  mcount )
00242     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',32)
00243     status = nf90_get_var(driver_id, ncsflid, xx,    & ! Snow Fall
00244          mstart,  mcount )
00245     if(status/=nf90_noerr) call handle_err(status,'read_ncep2',33)
00246 
00247 
00248 
00249 
00250 !itb...I have this hardwired for now: user must figure out the point
00251 !itb...he/she is seeking (not hard to do) and then directly assign it
00252 
00253 
00254         ! pull out landpoints in subdomain
00255         sib(1)%prog%tm2 = t2m(ipoint)
00256         sib(1)%prog%tcc2 = tcc(ipoint)
00257         sib(1)%prog%sw_dwn2 = swd(ipoint)
00258         sib(1)%prog%dlwbot2 = ldw(ipoint)
00259         sib(1)%prog%sh2 = sh(ipoint)
00260         sib(1)%prog%ps2 = sfp(ipoint)
00261         sib(1)%prog%lspr2 = lsp(ipoint)
00262         sib(1)%prog%cupr2 = cvp(ipoint)
00263     
00264         ! scale radiation to w/m2
00265         !sib(1)%prog%sw_dwn2 = sib(1)%prog%sw_dwn2/time%driver_step
00266         !sib(1)%prog%dlwbot2 = sib(1)%prog%dlwbot2/time%driver_step
00267         if ( sib(1)%prog%sw_dwn2 < 0 ) sib(1)%prog%sw_dwn2 = 0.0
00268         if ( sib(1)%prog%dlwbot2 < 0 ) sib(1)%prog%dlwbot2 = 0.0
00269         
00270         ! convert total cloud cover to fraction
00271         sib(1)%prog%tcc2 = sib(1)%prog%tcc2 * 0.01
00272 
00273         ! 10 m wind
00274         sib(1)%prog%spdm2=SQRT(uwd(ipoint)*uwd(ipoint)+vwd(ipoint)*vwd(ipoint))
00275 
00276         ! add snowfall to large scale precip and let SiB decide about snow.
00277         sib(1)%prog%lspr2 = sib(1)%prog%lspr2+xx(ipoint)
00278         ! convert to mm
00279         sib(1)%prog%lspr2 = (sib(1)%prog%lspr2-sib(1)%prog%cupr2)*time%driver_step
00280         sib(1)%prog%cupr2 = sib(1)%prog%cupr2*time%driver_step
00281         ! make sure precip > 0
00282         if ( sib(1)%prog%lspr2 < 0.0 ) sib(1)%prog%lspr2 = 0.0
00283         if ( sib(1)%prog%cupr2 < 0.0 ) sib(1)%prog%cupr2 = 0.0
00284 
00285         ! Conversion Pa -> hPa (pascals to millibars)
00286         sib(1)%prog%ps2 = sib(1)%prog%ps2 * 0.01
00287 
00288 !    print*,subname,' New driver data read ',ihour,iday,imon,iyear
00289 !    print*,'------------------------------------------------------'
00290 !    print*,'Extrema of new input data'
00291 !    print*, minval(sib%prog%tm2      ),maxval(sib%prog%tm2  ),' Temperature'
00292 !    print*, minval(sib%prog%tcc2     ),maxval(sib%prog%tcc2),' Total cloudiness'
00293 !    print*, minval(sib%prog%sh2    ),maxval(sib%prog%sh2 ),' dew point'
00294 !    print*, minval(sib%prog%spdm2    ),maxval(sib%prog%spdm2),' Surface wind'
00295 !    print*, minval(sib%prog%ps2      ),maxval(sib%prog%ps2 ),' Pressure'
00296 !    print*, minval(sib%prog%dlwbot2  ),maxval(sib%prog%dlwbot2),  &
00297 !        ' Long wave down'
00298 !    print*, minval(sib%prog%lspr2    ),maxval(sib%prog%lspr2),' Large sc precip'
00299 !    print*, minval(sib%prog%cupr2    ),maxval(sib%prog%cupr2),' Convective '
00300 !    print*, minval(sib%prog%sw_dwn2),maxval(sib%prog%sw_dwn2),  &
00301 !        ' Short wave down'
00302 !    print*,'-----------------------------------------------------'
00303 
00304 
00305 end subroutine sibdrv_read_ncep2_single
00306 
00001 subroutine sibdrv_read_ecmwf( sib, time )
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
00024 !  Kevin Schaefer moved conversion from pascals to millibars from sibdrv_interp to here (8/16/04)
00025 !  Kevin Schaefer moved conversion from dewpt to humidity here from sibdrv_interp (8/17/04)
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
00043 #ifdef PGF
00044 use netcdf
00045 use typeSizes
00046 #endif
00047 use kinds
00048 use sibtype
00049 use timetype
00050 
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
00073 integer(kind=int_kind) :: ncdptid ! Dewpoint at 2 m
00074 integer(kind=int_kind) :: ncsfpid ! Log Surface Pressure
00075 integer(kind=int_kind) :: nclspid ! Large Scale Precipitation
00076 integer(kind=int_kind) :: nccvpid ! Convective Precipitation
00077 integer(kind=int_kind) :: ncsflid ! Snow Fall
00078 
00079 real(kind=real_kind), dimension(nsib) :: t2m ! Total Cloud Cover
00080 real(kind=real_kind), dimension(nsib) :: tcc ! Total Cloud Cover
00081 real(kind=real_kind), dimension(nsib) :: swd ! Surface solar radiation downwards           
00082 real(kind=real_kind), dimension(nsib) :: ldw ! Surface thermal radiation downwards
00083 real(kind=real_kind), dimension(nsib) :: dpt ! Dewpoint at 2 m
00084 real(kind=dbl_kind), dimension(subcount) :: temp_dpt ! dew point
00085 real(kind=real_kind), dimension(nsib) :: sfp ! Log Surface Pressure
00086 real(kind=real_kind), dimension(nsib) :: lsp ! Large Scale Precipitation
00087 real(kind=real_kind), dimension(nsib) :: cvp ! Convective Precipitation
00088 real(kind=real_kind), dimension(nsib) :: sfl ! Snow Fall
00089 
00090 
00091 integer(kind=int_kind) :: status
00092 real(kind=real_kind) :: xtime,xyear,xmonth,xdoy,xday,xhour
00093 real(kind=real_kind), dimension(nsib) :: uwd,vwd
00094 real(kind=real_kind), dimension(nsib) :: xx
00095 
00096 character(len=13) :: subname
00097 data subname/'sibdrv_read '/
00098 
00099 
00100     !*** Storing previous time steps data
00101     do i=1,subcount
00102         sib(i)%prog%ps1       = sib(i)%prog%ps2
00103         sib(i)%prog%tm1       = sib(i)%prog%tm2
00104         sib(i)%prog%tcc1      = sib(i)%prog%tcc2
00105         sib(i)%prog%sh1     = sib(i)%prog%sh2
00106         sib(i)%prog%spdm1     = sib(i)%prog%spdm2
00107         sib(i)%prog%lspr1     = sib(i)%prog%lspr2
00108         sib(i)%prog%cupr1     = sib(i)%prog%cupr2
00109         sib(i)%prog%dlwbot1   = sib(i)%prog%dlwbot2
00110         sib(i)%prog%sw_dwn1 = sib(i)%prog%sw_dwn2
00111     enddo
00112 
00113     ! switch files if needed
00114     if ( time%switch_driver ) then
00115         status = nf90_close( driver_id )
00116         
00117         write( filename, dr_format ) time%driver_year, time%driver_month
00118         status = nf90_open( trim(filename), nf90_nowrite, driver_id )
00119         if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',0)
00120         print *, 'switch driver to ',trim(filename)
00121     endif
00122 
00123     ! read new driver data
00124 !    print*, subname,tau,nextsecond,nextday,nextdoy,       &
00125 !        nmonth,nyear,nextmonth,nextyear
00126 
00127     ! check time values in driver data file
00128     status = nf90_inq_varid( driver_id,   'time', nctimeid )
00129     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',1)
00130     status = nf90_inq_varid( driver_id,   'year', ncyid )
00131     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',2)
00132     status = nf90_inq_varid( driver_id,   'month',ncmid )
00133     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',3)
00134     status = nf90_inq_varid( driver_id,   'doy',  ncdoyid )
00135     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',4)
00136     status = nf90_inq_varid( driver_id,   'day',  nctdid )
00137     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',5)
00138     status = nf90_inq_varid( driver_id,   'hour', nchid )
00139     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',6)
00140     print*,subname,nctimeid,ncyid,ncmid,ncdoyid,nctdid,nchid,time%driver_recnum
00141 
00142     ! read time
00143     mstart(1) = time%driver_recnum
00144     status = nf90_get_var( driver_id, nctimeid, xtime, mstart(1:1) )
00145     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',7)
00146     status = nf90_get_var( driver_id, ncyid,    xyear, mstart(1:1) )
00147     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',8)
00148     status = nf90_get_var( driver_id, ncmid,   xmonth, mstart(1:1) )
00149     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',10)
00150     status = nf90_get_var( driver_id, ncdoyid,   xdoy, mstart(1:1) )
00151     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',11)
00152     status = nf90_get_var( driver_id, nctdid,    xday, mstart(1:1) )
00153     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',12)
00154     status = nf90_get_var( driver_id, nchid,    xhour, mstart(1:1) )
00155     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',13)
00156 
00157     ihour=xhour
00158     iday =xday
00159     idoy =xdoy
00160     imon =xmonth
00161     iyear=xyear
00162     imin=0
00163 
00164     print*,subname,'Time level in file: ',ihour,iday,idoy,imon,iyear
00165 
00166     if( time%driver_day /= iday .or. time%driver_hour /= ihour) then
00167 !        print*,subname,nyear0,nmonthofyear,ndayofyear,nsecondofday/3600
00168 !        print*,subname,nextsecond,nexthour,nextday
00169 !        print*,subname,'Not the right data'
00170 !        print *, time%driver_day, iday, time%driver_hour, ihour
00171 !        stop
00172     endif
00173 
00174     ! get veriable id's
00175     status=nf90_inq_varid( driver_id, 't2m', nct2mid ) ! Temperature at 2 m
00176     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',14)
00177     status=nf90_inq_varid( driver_id, 'tcc', nctccid ) ! Total Cloud Cover
00178     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',15)
00179     status=nf90_inq_varid( driver_id, 'swd', ncswdid ) ! Surface solar rad downwards
00180     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',16)
00181     status=nf90_inq_varid( driver_id, 'ldw', ncldwid ) ! Surface thermal rad down
00182     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',17)
00183     status=nf90_inq_varid( driver_id, 'uwd', ncuwdid ) ! U-wind at 10 m
00184     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',18)
00185     status=nf90_inq_varid( driver_id, 'vwd', ncvwdid ) ! V-wind at 10 m
00186     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',19)
00187     status=nf90_inq_varid( driver_id, 'dpt', ncdptid ) ! Dewpoint at 2 m
00188     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',20)
00189     status=nf90_inq_varid( driver_id, 'sfp', ncsfpid ) ! Log Surface Pressure
00190     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',21)
00191     status=nf90_inq_varid( driver_id, 'lsp', nclspid ) ! Large Scale Precipitation
00192     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',22)
00193     status=nf90_inq_varid( driver_id, 'cvp', nccvpid ) ! Convective Precipitation
00194     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',23)
00195     status=nf90_inq_varid( driver_id, 'sfl', ncsflid ) ! Snow Fall
00196     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',24)
00197 
00198     ! get data
00199     mstart=(/1,time%driver_recnum/); mcount=(/nsib,1/)
00200     status = nf90_get_var( driver_id, nct2mid, t2m,    & !Temperature at 2 m
00201          mstart,  mcount )
00202     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',25)
00203     status = nf90_get_var( driver_id, nctccid, tcc,    & !Total Cloud Cover
00204          mstart,  mcount )
00205     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',26)
00206     status = nf90_get_var( driver_id, ncswdid, swd,    & !Surface solar rad downwards
00207         mstart,  mcount )
00208     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',27)
00209     status = nf90_get_var( driver_id, ncldwid, ldw,    & !Surface thermal rad downwards
00210          mstart,  mcount )
00211     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',28)
00212     status = nf90_get_var( driver_id, ncuwdid, uwd,    & ! U-wind at 10 m
00213          mstart,  mcount )
00214     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',29)
00215     status = nf90_get_var( driver_id, ncvwdid, vwd,    & ! V-wind at 10 m
00216          mstart,  mcount )
00217     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',30)
00218     status = nf90_get_var( driver_id, ncdptid, dpt,    & ! Dewpoint at 2 m
00219          mstart,  mcount )
00220     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',31)
00221     status = nf90_get_var( driver_id, ncsfpid, sfp,    & ! Surface Pressure
00222          mstart,  mcount )
00223     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',32)
00224     status = nf90_get_var( driver_id, nclspid, lsp,    & ! Large Scale Precipitation
00225          mstart,  mcount )
00226     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',33)
00227     status = nf90_get_var( driver_id, nccvpid, cvp,    & ! Convective Precipitation
00228          mstart,  mcount )
00229     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',34)
00230     status = nf90_get_var(driver_id, ncsflid, xx,    & ! Snow Fall
00231          mstart,  mcount )
00232     if(status/=nf90_noerr) call handle_err(status,'read_ecmwf',35)
00233 
00234     do i=1,subcount
00235         ! pull out landpoints in subdomain
00236         sib(i)%prog%tm2 = t2m(subset(i))
00237         sib(i)%prog%tcc2 = tcc(subset(i))
00238         sib(i)%prog%sw_dwn2 = swd(subset(i))
00239         sib(i)%prog%dlwbot2 = ldw(subset(i))
00240         temp_dpt(i) = dpt(subset(i))
00241         sib(i)%prog%ps2 = sfp(subset(i))
00242         sib(i)%prog%lspr2 = lsp(subset(i))
00243         sib(i)%prog%cupr2 = cvp(subset(i))
00244     
00245         ! scale radiation to w/m2
00246         sib(i)%prog%sw_dwn2 = sib(i)%prog%sw_dwn2/time%driver_step
00247         sib(i)%prog%dlwbot2 = sib(i)%prog%dlwbot2/time%driver_step
00248 
00249         ! 10 m wind
00250         sib(i)%prog%spdm2=SQRT(uwd(subset(i))*uwd(subset(i))+vwd(subset(i))*vwd(subset(i)))
00251 
00252         ! add snowfall to large scale precip and let SiB decide about snow.
00253         sib(i)%prog%lspr2 = sib(i)%prog%lspr2+xx(subset(i))
00254         ! convert to mm
00255         sib(i)%prog%lspr2 = sib(i)%prog%lspr2*1000
00256         if ( sib(i)%prog%lspr2 <= 1.e-3 ) sib(i)%prog%lspr2 = 0.0_dbl_kind
00257         sib(i)%prog%cupr2 = sib(i)%prog%cupr2*1000
00258 
00259         ! KS Conversion ln(pascals) to millibars
00260         sib(i)%prog%ps2 = exp(sib(i)%prog%ps2)*0.01
00261     enddo
00262 !
00263 ! KS convert dew point to specific humidity
00264     call qsat_eau(subcount,sib%prog%ps2*100.0,temp_dpt,sib%prog%sh2)
00265 
00266     print*,subname,'New driver data read ',ihour,iday,imon,iyear
00267     print*,'------------------------------------------------------'
00268     print*,'Extrema of new input data'
00269     print*, minval(sib%prog%tm2      ),maxval(sib%prog%tm2  ),' Temperature'
00270     print*, minval(sib%prog%tcc2     ),maxval(sib%prog%tcc2),' Total cloudiness'
00271     print*, minval(sib%prog%sh2    ),maxval(sib%prog%sh2 ),' humidity'
00272     print*, minval(sib%prog%spdm2    ),maxval(sib%prog%spdm2),' Surface wind'
00273     print*, minval(sib%prog%ps2      ),maxval(sib%prog%ps2 ),' Pressure'
00274     print*, minval(sib%prog%dlwbot2  ),maxval(sib%prog%dlwbot2),  &
00275         ' Long wave down'
00276     print*, minval(sib%prog%lspr2    ),maxval(sib%prog%lspr2),' Large sc precip'
00277     print*, minval(sib%prog%cupr2    ),maxval(sib%prog%cupr2),' Convective '
00278     print*, minval(sib%prog%sw_dwn2),maxval(sib%prog%sw_dwn2),  &
00279         ' Short wave down'
00280     print*,'-----------------------------------------------------'
00281 
00282 
00283 end subroutine sibdrv_read_ecmwf
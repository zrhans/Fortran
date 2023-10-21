00001 subroutine sibdrv_read_geos4( sib, time )
00002 
00003 !****--------------------------------------------------------------------
00004 !    This subroutines reads the forcing surface meteorological data 
00005 !    for the next time step.
00006 !    If required, it closes the current month's data file and opens the 
00007 !    next month's data file.
00008 
00009 !    precip, snowfall and radiation data are aggregated data over 3 hours
00010 !    They have been stored such that the data at time x represent the
00011 !    aggregation over the preceeding 3 hours.
00012 !    Thus data at 
00013 !     0 hours are the sum of the values 21-24 hours of the previous day
00014 !     3 hours are the sum of the values  0- 3 hours of the current day
00015 !     6 hours are the sum of the values  3- 6 hours of the current day
00016 !     9 hours are the sum of the values  6- 9 hours of the current day
00017 !    12 hours are the sum of the values  9-12 hours of the current day
00018 !    15 hours are the sum of the values 12-15 hours of the current day
00019 !    18 hours are the sum of the values 15-18 hours of the current day
00020 !    21 hours are the sum of the values 18-21 hours of the current day
00021 !
00022 !    Consequently new data are read for all variables at the same time.
00023 !    For all point data variables, e.g. temp., the data are for
00024 !    now + 3 hours. For precip and radiation data the new data
00025 !    are the aggregation over the next six hours.
00026 !
00027 ! Modifications:
00028 !  Kevin Schaefer removed conversion from millibars to pascals (8/16/04)
00029 !  Kevin Schaefer changed from tdew1/2 to sh1/2 because thats whats read in (8/17/04)
00030 !  Kevin Schaefer added calls to error handling routine (11/12/04)
00031 !  Kevin Schaefer deleted retreival of undefimed time variable (11/12/04)
00032 !****--------------------------------------------------------------------
00033 
00034 
00035 use sib_const_module, only:   &
00036     nsib,               &
00037     latsib,             &
00038     lonsib,             &
00039     subset,             &
00040     subcount
00041 
00042 use sib_io_module, only:   &
00043     dr_format,           &
00044     driver_id
00045 
00046 use physical_parameters, only:              &
00047     kapa => kappa,   &
00048     pi
00049 
00050 use kinds
00051 #ifdef PGF
00052 use netcdf
00053 use typeSizes
00054 #endif 
00055 use sibtype
00056 use timetype
00057 
00058 type(sib_t), dimension(subcount), intent(inout) :: sib
00059 type(time_struct), intent(in) :: time
00060 
00061 real(kind=dbl_kind) :: pid180 
00062 real(kind=dbl_kind) :: cosz(nsib)
00063 
00064 integer(kind=int_kind) :: i, iyear, imon, iday, idoy, ihour, imin
00065 
00066 integer(kind=int_kind) ::  n
00067 
00068 character*100 filename
00069 character*7 gchar
00070 integer(kind=int_kind) :: nctimeid,ncyid,ncmid,nctdid,ncdoyid,nchid
00071 integer(kind=int_kind), dimension(2) :: mstart,mcount
00072 
00073 integer(kind=int_kind) :: nct2mid ! Temperature at 2 m
00074 integer(kind=int_kind) :: ncswdid ! Surface solar radiation downwards           
00075 integer(kind=int_kind) :: albedoid! Albedo
00076 integer(kind=int_kind) :: ncldwid ! Surface thermal radiation downwards
00077 integer(kind=int_kind) :: ncuwdid ! U-wind at 10 m
00078 integer(kind=int_kind) :: ncvwdid ! V-wind at 10 m
00079 integer(kind=int_kind) :: ncshid ! humidity at 2 m
00080 integer(kind=int_kind) :: ncsfpid ! Log Surface Pressure
00081 integer(kind=int_kind) :: nclspid ! Large Scale Precipitation
00082 integer(kind=int_kind) :: nccvpid ! Convective Precipitation
00083 
00084 real(kind=real_kind), dimension(nsib) :: t2m ! Total Cloud Cover
00085 real(kind=real_kind), dimension(nsib) :: swd ! Surface solar radiation downwards           
00086 real(kind=real_kind), dimension(nsib) :: alb ! Total Cloud Cover
00087 real(kind=real_kind), dimension(nsib) :: ldw ! Surface thermal radiation downwards
00088 real(kind=real_kind), dimension(nsib) :: sh ! humidity at 2 m
00089 real(kind=real_kind), dimension(nsib) :: sfp ! Log Surface Pressure
00090 real(kind=real_kind), dimension(nsib) :: lsp ! Large Scale Precipitation
00091 real(kind=real_kind), dimension(nsib) :: cvp ! Convective Precipitation
00092 
00093 integer(kind=int_kind) :: status
00094 real(kind=real_kind) :: xtime,xyear,xmonth,xdoy,xday,xhour
00095 real(kind=dbl_kind), dimension(nsib) :: xx,uwd,vwd
00096 
00097 character(len=13) :: subname
00098 data subname/'sibdrv_read '/
00099 
00100 
00101     !*** Storing previous time steps data
00102     do i=1,subcount
00103         sib(i)%prog%ps1       = sib(i)%prog%ps2
00104         sib(i)%prog%tm1       = sib(i)%prog%tm2
00105         sib(i)%prog%tcc1      = sib(i)%prog%tcc2
00106         sib(i)%prog%sh1       = sib(i)%prog%sh2
00107         sib(i)%prog%spdm1     = sib(i)%prog%spdm2
00108         sib(i)%prog%lspr1     = sib(i)%prog%lspr2
00109         sib(i)%prog%cupr1     = sib(i)%prog%cupr2
00110         sib(i)%prog%dlwbot1   = sib(i)%prog%dlwbot2
00111         sib(i)%prog%sw_dwn1   = sib(i)%prog%sw_dwn2
00112     enddo
00113 
00114     ! switch files if needed
00115     if ( time%switch_driver ) then
00116         status = nf90_close( driver_id )
00117         
00118         write( filename, dr_format ) time%driver_year, time%driver_month
00119         status = nf90_open( trim(filename), nf90_nowrite, driver_id )
00120         if(status/=nf90_noerr) call handle_err(status,'read_geos4',1)
00121         print *, 'drvr file switched to ',trim(filename)
00122     endif
00123 
00124     ! read new driver data
00125     ! read time values from driver data file
00126     status = nf90_inq_varid( driver_id,   'year', ncyid )
00127     if(status/=nf90_noerr) call handle_err(status,'read_geos4',3)
00128     status = nf90_inq_varid( driver_id,   'month',ncmid )
00129     if(status/=nf90_noerr) call handle_err(status,'read_geos4',4)
00130     status = nf90_inq_varid( driver_id,   'doy',  ncdoyid )
00131     if(status/=nf90_noerr) call handle_err(status,'read_geos4',5)
00132     status = nf90_inq_varid( driver_id,   'day',  nctdid )
00133     if(status/=nf90_noerr) call handle_err(status,'read_geos4',6)
00134     status = nf90_inq_varid( driver_id,   'hour', nchid )
00135     if(status/=nf90_noerr) call handle_err(status,'read_geos4',7)
00136 
00137     ! read time
00138     mstart(1) = time%driver_recnum
00139     status = nf90_get_var( driver_id, ncyid,    xyear, mstart(1:1) )
00140     if(status/=nf90_noerr) call handle_err(status,'read_geos4',9)
00141     status = nf90_get_var( driver_id, ncmid,   xmonth, mstart(1:1) )
00142     if(status/=nf90_noerr) call handle_err(status,'read_geos4',10)
00143     status = nf90_get_var( driver_id, ncdoyid,   xdoy, mstart(1:1) )
00144     if(status/=nf90_noerr) call handle_err(status,'read_geos4',11)
00145     status = nf90_get_var( driver_id, nctdid,    xday, mstart(1:1) )
00146     if(status/=nf90_noerr) call handle_err(status,'read_geos4',12)
00147     status = nf90_get_var( driver_id, nchid,    xhour, mstart(1:1) )
00148     if(status/=nf90_noerr) call handle_err(status,'read_geos4',13)
00149 
00150     ! get veriable id's
00151     status=nf90_inq_varid( driver_id, 't2m', nct2mid ) ! Temperature at 2 m
00152     if(status/=nf90_noerr) call handle_err(status,'read_geos4',14)
00153     status=nf90_inq_varid( driver_id, 'radswg', ncswdid ) ! Surface solar rad downwards
00154     if(status/=nf90_noerr) call handle_err(status,'read_geos4',15)
00155     status=nf90_inq_varid( driver_id, 'albedo', albedoid ) ! Albedo
00156     if(status/=nf90_noerr) call handle_err(status,'read_geos4',16)
00157     status=nf90_inq_varid( driver_id, 'lwgdown', ncldwid ) ! Surface thermal rad down
00158     if(status/=nf90_noerr) call handle_err(status,'read_geos4',17)
00159     status=nf90_inq_varid( driver_id, 'u10m', ncuwdid ) ! U-wind at 10 m
00160     if(status/=nf90_noerr) call handle_err(status,'read_geos4',18)
00161     status=nf90_inq_varid( driver_id, 'v10m', ncvwdid ) ! V-wind at 10 m
00162     if(status/=nf90_noerr) call handle_err(status,'read_geos4',19)
00163     status=nf90_inq_varid( driver_id, 'q2m', ncshid ) ! humidity at 2 m
00164     if(status/=nf90_noerr) call handle_err(status,'read_geos4',20)
00165     status=nf90_inq_varid( driver_id, 'ps', ncsfpid ) ! Log Surface Pressure
00166     if(status/=nf90_noerr) call handle_err(status,'read_geos4',21)
00167     status=nf90_inq_varid( driver_id, 'preacc', nclspid ) ! Large Scale Precipitation
00168     if(status/=nf90_noerr) call handle_err(status,'read_geos4',22)
00169     status=nf90_inq_varid( driver_id, 'precon', nccvpid ) ! Convective Precipitation
00170     if(status/=nf90_noerr) call handle_err(status,'read_geos4',23)
00171 
00172     ! get data
00173     mstart=(/1,time%driver_recnum/); mcount=(/nsib,1/)
00174     status = nf90_get_var( driver_id, nct2mid, t2m,     & !Temperature at 2 m
00175          mstart,  mcount )
00176     if(status/=nf90_noerr) call handle_err(status,'read_geos4',24)
00177     status = nf90_get_var( driver_id, ncswdid, swd,     & !Surface solar rad downwards
00178          mstart,  mcount )
00179     if(status/=nf90_noerr) call handle_err(status,'read_geos4',25)
00180     status = nf90_get_var( driver_id, albedoid, alb,    & !Albedo
00181          mstart,  mcount )
00182     if(status/=nf90_noerr) call handle_err(status,'read_geos4',26)
00183     status = nf90_get_var( driver_id, ncldwid, ldw,     & !Surface thermal rad downwards
00184          mstart,  mcount )
00185     if(status/=nf90_noerr) call handle_err(status,'read_geos4',27)
00186     status = nf90_get_var( driver_id, ncuwdid, uwd,     & ! U-wind at 10 m
00187          mstart,  mcount )
00188     if(status/=nf90_noerr) call handle_err(status,'read_geos4',28)
00189     status = nf90_get_var( driver_id, ncvwdid, vwd,     & ! V-wind at 10 m
00190          mstart,  mcount )
00191     if(status/=nf90_noerr) call handle_err(status,'read_geos4',29)
00192     status = nf90_get_var( driver_id, ncshid, sh,     & ! specific humidity
00193          mstart,  mcount )
00194     if(status/=nf90_noerr) call handle_err(status,'read_geos4',30)
00195     status = nf90_get_var( driver_id, ncsfpid, sfp,     & ! Surface Pressure
00196          mstart,  mcount )
00197     if(status/=nf90_noerr) call handle_err(status,'read_geos4',31)
00198     status = nf90_get_var( driver_id, nclspid, lsp,     & ! Large Scale Precipitation
00199          mstart,  mcount )
00200     if(status/=nf90_noerr) call handle_err(status,'read_geos4',32)
00201     status = nf90_get_var( driver_id, nccvpid, cvp,     & ! Convective Precipitation
00202          mstart,  mcount )
00203     if(status/=nf90_noerr) call handle_err(status,'read_geos4',33)
00204 
00205 
00206     do i=1,subcount
00207         ! pull out landpoints in subdomain
00208         sib(i)%prog%tm2 = t2m(subset(i))
00209         sib(i)%prog%sw_dwn2 = swd(subset(i))/(1.-alb(subset(i)))
00210         sib(i)%prog%dlwbot2 = ldw(subset(i))
00211         sib(i)%prog%sh2 = sh(subset(i))
00212         sib(i)%prog%ps2 = sfp(subset(i))
00213         sib(i)%prog%lspr2 = lsp(subset(i))
00214         sib(i)%prog%cupr2 = cvp(subset(i))
00215         sib(i)%prog%tcc2 = 0.0_dbl_kind
00216     
00217         ! 10 m wind
00218         sib(i)%prog%spdm2=SQRT(uwd(subset(i))*uwd(subset(i))+vwd(subset(i))*vwd(subset(i)))
00219 
00220         ! convert to mm
00221         sib(i)%prog%lspr2 = sib(i)%prog%lspr2/8.0_dbl_kind
00222         sib(i)%prog%cupr2 = sib(i)%prog%cupr2/8.0_dbl_kind
00223         sib(i)%prog%lspr2 = sib(i)%prog%lspr2 - sib(i)%prog%cupr2
00224 
00225         !convert specific humidity from g kg-1 to kg kg-1
00226         sib(i)%prog%sh2 = sib(i)%prog%sh2*0.001_dbl_kind
00227 
00228     enddo
00229 
00230 !    print*,'read driver data ',xyear,xdoy,xhour
00231 !    print*,'------------------------------------------------------'
00232 !    print*,'Extrema of new input data'
00233 !    print*, minval(sib%prog%tm2      ),maxval(sib%prog%tm2  ),' Temperature'
00234 !    print*, minval(sib%prog%tcc2     ),maxval(sib%prog%tcc2),' Total cloudiness'
00235 !    print*, minval(sib%prog%sh2    ),maxval(sib%prog%sh2 ),' dew point'
00236 !    print*, minval(sib%prog%spdm2    ),maxval(sib%prog%spdm2),' Surface wind'
00237 !    print*, minval(sib%prog%ps2      ),maxval(sib%prog%ps2 ),' Pressure'
00238 !    print*, minval(sib%prog%dlwbot2  ),maxval(sib%prog%dlwbot2),  &
00239 !        ' Long wave down'
00240 !    print*, minval(sib%prog%lspr2    ),maxval(sib%prog%lspr2),' Large sc precip'
00241 !    print*, minval(sib%prog%cupr2    ),maxval(sib%prog%cupr2),' Convective '
00242 !    print*, minval(sib%prog%sw_dwn2),maxval(sib%prog%sw_dwn2),  &
00243 !        ' Short wave down'
00244 !    print*,'-----------------------------------------------------'
00245 
00246 
00247 end subroutine sibdrv_read_geos4
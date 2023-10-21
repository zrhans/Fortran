00001 subroutine create_qp2( out_path, numvars, subcount, ihr, jhr, year,  &
00002                        month, longitude, latitude, lonindex, latindex,  &
00003                        doqpsib, nameqpsib, listqpsib, qp2id, qp2varid,   &
00004                        qp2timeid, qp2charid, qp2startid, qp2endid,   &
00005                        qp2periodid, drvr_type, biome_source, soil_source,  &
00006                        soref_source, ndvi_source, c4_source, d13cresp_source,  &
00007                        rank )
00008 #ifdef PGF
00009 use netcdf
00010 use typeSizes
00011 #endif
00012 
00013 ! parameters
00014 character(len=256), intent(in) :: out_path      ! directory to write file to
00015 integer, intent(in) :: numvars                  ! max # of variables written to file
00016 integer, intent(in) :: subcount                 ! number of landpoints
00017 integer, intent(in) :: ihr                      ! number of longitude indices
00018 integer, intent(in) :: jhr                      ! number of latitude indices
00019 integer, intent(in) :: year                     ! current year (used in filename)
00020 integer, intent(in) :: month                    ! current month ( "  " )
00021 real, dimension(ihr), intent(in) :: longitude   ! array of longitude coordinates
00022 real, dimension(jhr), intent(in) :: latitude    ! array of latitude coordinates
00023 integer, dimension(subcount), intent(in) :: lonindex ! array of longitude indices
00024 integer, dimension(subcount), intent(in) :: latindex ! array of latitude indices
00025 logical, dimension(numvars), intent(in) :: doqpsib    ! defines which vars. to write to file
00026 character(len=16), dimension(numvars), intent(in) :: nameqpsib    ! names of vars.
00027 character(len=80), dimension(numvars), intent(in) :: listqpsib    ! variable descriptions
00028 integer, intent(out) :: qp2id                   ! qp file id#
00029 integer, dimension(numvars), intent(out):: qp2varid     ! variable id#s 
00030 integer, intent(out) :: qp2timeid               ! time variable id#
00031 integer, intent(out) :: qp2charid               ! char_time variable id#
00032 integer, intent(out) :: qp2startid              ! start_period variable id#
00033 integer, intent(out) :: qp2endid                ! end_period variable id#
00034 integer, intent(out) :: qp2periodid             ! period_length variable id#
00035 character(len=8), intent(in) :: drvr_type       ! type of driver data used
00036 character(len=100), intent(in) :: biome_source
00037 character(len=100), intent(in) :: soil_source
00038 character(len=100), intent(in) :: soref_source
00039 character(len=100), intent(in) :: ndvi_source
00040 character(len=100), intent(in) :: c4_source
00041 character(len=100), intent(in) :: d13cresp_source
00042 integer, intent(in) :: rank
00043 
00044 ! netcdf variables
00045 integer :: status           ! return status of netcdf functions
00046 integer :: latid            ! latitude dimension id #
00047 integer :: lonid            ! longitude dimension id #
00048 integer :: timeid           ! time dimension id #
00049 integer :: charid           ! char_len dimension id #
00050 integer :: latitudeid       ! latitude variable id #
00051 integer :: longitudeid      ! longitude variable id #
00052 integer :: subcountid       ! landpoints variable id #
00053 integer :: lonindexid      ! longitude indexing array id #
00054 integer :: latindexid      ! latitude indexing array id #
00055 
00056 ! local variables
00057 integer :: n                        ! index variable
00058 character(len=256) :: filename      ! filename string
00059 character(len=40) :: units          ! variable units
00060 character(len=80) :: longname       ! variable description
00061 integer :: unit_len, long_len       ! not used, returned by get_units()
00062 character*4 :: syear
00063 
00064     ! make sure qp2id is not tied to any open file
00065     status = nf90_close( qp2id )
00066 
00067     ! create file
00068     write( filename, '(a,i4.4,i2.2,a,i3.3,a)' ) trim(out_path)//'hsib_',   &
00069         year, month, 'p', rank, '.qp2.nc'
00070     status = nf90_create( trim(filename), nf90_clobber, qp2id)
00071 
00072     write (syear, '(i4.4)') year
00073 
00074     
00075     ! define global attributes
00076     call global_atts( qp2id, 'sib3', 'lat/lon', '1.0', drvr_type,  &
00077         biome_source, soil_source, soref_source, ndvi_source, c4_source,  &
00078         d13cresp_source, rank )
00079 
00080     ! define dimensions
00081     status = nf90_def_dim( qp2id, 'time', nf90_unlimited, timeid )
00082     status = nf90_def_dim( qp2id, 'char_len', 10, charid )
00083     status = nf90_def_dim( qp2id, 'latitude', jhr, latid )
00084     status = nf90_def_dim( qp2id, 'longitude', ihr, lonid )
00085     status = nf90_def_dim( qp2id, 'landpoints', subcount, subcountid )
00086     
00087     ! define control variables
00088     status = nf90_def_var( qp2id, 'time', nf90_double, (/timeid/), qp2timeid )
00089     status = nf90_put_att( qp2id, qp2timeid, 'quantity', 'time' )
00090     status = nf90_put_att( qp2id, qp2timeid, 'units', 'days since '//syear//'-01-01' )
00091     status = nf90_put_att( qp2id, qp2timeid, 'calender', 'noleap' )
00092     
00093     status = nf90_def_var( qp2id, 'char_time', nf90_char, (/charid,timeid/), qp2charid )
00094     status = nf90_put_att( qp2id, qp2charid, 'format', 'mm/dd/yyyy' )
00095     
00096     status = nf90_def_var( qp2id, 'start_period', nf90_double, (/timeid/), qp2startid )
00097     status = nf90_put_att( qp2id, qp2startid, 'long_name', 'start of averaged period' )
00098     status = nf90_put_att( qp2id, qp2startid, 'units', 'days since '//syear//'-01-01' )
00099     
00100     status = nf90_def_var( qp2id, 'end_period', nf90_double, (/timeid/), qp2endid )
00101     status = nf90_put_att( qp2id, qp2endid, 'long_name', 'end of averaged period' )
00102     status = nf90_put_att( qp2id, qp2endid, 'units', 'days since '//syear//'-01-01' )
00103     
00104     status = nf90_def_var( qp2id, 'period_length', nf90_double, (/timeid/), qp2periodid )
00105     status = nf90_put_att( qp2id, qp2periodid, 'long_name', 'length of averaged period' )
00106     status = nf90_put_att( qp2id, qp2periodid, 'units', 'days' )
00107     
00108     status = nf90_def_var( qp2id, 'latitude', nf90_float, (/latid/), latitudeid )
00109     status = nf90_put_att( qp2id, latitudeid, 'units', 'degrees_north' )
00110     status = nf90_put_att( qp2id, latitudeid, 'quantity', 'latitude' )
00111     
00112     status = nf90_def_var( qp2id, 'longitude', nf90_float, (/lonid/), longitudeid )
00113     status = nf90_put_att( qp2id, longitudeid, 'units', 'degrees_east' )
00114     status = nf90_put_att( qp2id, longitudeid, 'quantity', 'longitude' )
00115     
00116     status = nf90_def_var( qp2id, 'lonindex', nf90_int, (/subcountid/), lonindexid )
00117     status = nf90_put_att( qp2id, lonindexid, 'long_name', 'Longitude array index' )
00118     status = nf90_put_att( qp2id, lonindexid, 'units', 'index-integer' )
00119 
00120     status = nf90_def_var( qp2id, 'latindex', nf90_int, (/subcountid/), latindexid )
00121     status = nf90_put_att( qp2id, latindexid, 'long_name', 'Latitude array index' )
00122     status = nf90_put_att( qp2id, latindexid, 'units', 'index-integer' )
00123 
00124     ! define data variables
00125     do n = 1, numvars
00126         if ( doqpsib(n) ) then
00127             status = nf90_def_var( qp2id, trim(nameqpsib(n)), nf90_float, &
00128                 (/subcountid,timeid/), qp2varid(n) )
00129             call get_units( listqpsib(n), longname, long_len, units, unit_len )
00130             status = nf90_put_att( qp2id, qp2varid(n), 'long_name', trim(longname) )
00131             status = nf90_put_att( qp2id, qp2varid(n), 'title', trim(longname) )
00132             status = nf90_put_att( qp2id, qp2varid(n), 'units', trim(units) )
00133             status = nf90_put_att( qp2id, qp2varid(n), 'missing_value', 1.e36 )
00134         endif
00135     enddo
00136     
00137     ! switch from definition mode to data mode
00138     status = nf90_enddef( qp2id )
00139     
00140     ! assign values to variables not variant with time
00141     status = nf90_put_var( qp2id, latitudeid, latitude )
00142     status = nf90_put_var( qp2id, longitudeid, longitude )
00143     status = nf90_put_var( qp2id, lonindexid, lonindex )
00144     status = nf90_put_var( qp2id, latindexid, latindex )
00145 
00146 end subroutine create_qp2
00147 
00148 
00149 !-----------------------------------------------------------------------
00150 
00151 subroutine write_qp2( qp2id, qp2timeid, qp2startid, qp2endid, qp2periodid,  &
00152                       qp2charid, numvars, subcount, qp2varid, qpsib,  &
00153                       doqpsib, indxqpsib, year, month, day,  &
00154                       seconds, end_period, period_length )
00155 #ifdef PGF
00156 use netcdf
00157 use typeSizes
00158 #endif
00159 use kinds
00160 use sib_const_module, only: dtsib
00161 
00162 ! parameters
00163 integer, intent(in) :: qp2id            ! file id#
00164 integer, intent(in) :: qp2timeid        ! time variable id #
00165 integer, intent(in) :: qp2startid       ! start_period id #
00166 integer, intent(in) :: qp2endid         ! end_period id #
00167 integer, intent(in) :: qp2periodid      ! period_length id #
00168 integer, intent(in) :: qp2charid        ! char_time id #
00169 integer, intent(in) :: numvars          ! max # of variables written to file
00170 integer, intent(in) :: subcount         ! number of landpoints in subdomain
00171 integer, dimension(numvars), intent(in) :: qp2varid   ! variable id #s
00172 real(kind=dbl_kind), dimension(subcount,numvars), intent(in) :: qpsib     ! variable values
00173 logical, dimension(numvars), intent(in) :: doqpsib    ! defines which variables to write out
00174 integer, dimension(numvars), intent(in) :: indxqpsib  ! ??????????
00175 integer, intent(in) :: year     ! current year (used in char_time)
00176 integer, intent(in) :: month    ! current month (used in char_time)
00177 integer, intent(in) :: day      ! current day (used in char_time)
00178 integer, intent(in) :: seconds  ! current second of the year
00179 double precision, intent(in) :: end_period      ! end of averaged period
00180 double precision, intent(in) :: period_length   ! length of averaged period
00181 
00182 ! netcdf variables
00183 integer :: status       ! return status of netcdf functions
00184 integer :: dimid        ! dimension id #
00185 
00186 ! local variables
00187 integer :: n, i                      ! index variables
00188 integer :: step                      ! next time step in qp file
00189 double precision :: dyear
00190 character(len=10) :: char_time       ! mm/dd/yyyy
00191 character(len=10) :: name
00192 double precision :: secyear = 86400.
00193 
00194     ! find next time step
00195     status = nf90_inq_dimid( qp2id, 'time', dimid )
00196     status = nf90_inquire_dimension( qp2id, dimid, name, step )
00197     step = step + 1
00198     
00199     ! write out time variables
00200     dyear = seconds/secyear
00201     status = nf90_put_var( qp2id, qp2timeid, dyear, (/step/) )
00202     status = nf90_put_var( qp2id, qp2startid, end_period - period_length,  &
00203         (/step/) )
00204     status = nf90_put_var( qp2id, qp2endid, end_period, (/step/) )
00205     status = nf90_put_var( qp2id, qp2periodid, period_length, (/step/) )
00206     
00207     write(char_time, '(i2.2,a1,i2.2,a1,i4.4)') month, '/', day, '/', year
00208     status = nf90_put_var( qp2id, qp2charid, char_time, (/1,step/), (/10,1/) )
00209     
00210     ! write out data variables
00211     do n = 1, numvars
00212         if ( doqpsib(n) ) then
00213             status = nf90_put_var( qp2id, qp2varid(n), qpsib(:,indxqpsib(n)),  &
00214                 (/1,step/), (/subcount,1/) )
00215         endif
00216     enddo
00217 
00218 end subroutine write_qp2
00219 
00220 !-----------------------------------------------------------------------
00221 
00222 subroutine create_qp3( out_path, numvars, subcount, ihr, jhr,  &
00223                        year, month, nsoil, longitude, latitude,  &
00224                        lonindex, latindex, doqp3sib, nameqp3sib,   &
00225                        listqp3sib, drvr_type, biome_source, soil_source,  &
00226                        soref_source, ndvi_source, c4_source, d13cresp_source,  &
00227                        qp3id, qp3varid, qp3timeid, qp3charid, qp3startid,  &
00228                        qp3endid, qp3periodid, rank )
00229 #ifdef PGF
00230 use netcdf
00231 use typeSizes
00232 #endif
00233 
00234 ! parameters
00235 character(len=256), intent(in) :: out_path
00236 integer, intent(in) :: numvars          ! # of variables to write out
00237 integer, intent(in) :: subcount         ! # of landpoints
00238 integer, intent(in) :: ihr              ! # of longitude indices
00239 integer, intent(in) :: jhr              ! # of latitude indices
00240 integer, intent(in) :: year             ! current year of simulation
00241 integer, intent(in) :: month            ! current month
00242 integer, intent(in) :: nsoil            ! # of soil layers
00243 real, dimension(ihr), intent(in) :: longitude   ! array of longitude coordinates
00244 real, dimension(jhr), intent(in) :: latitude    ! array of latitude coordinates
00245 integer, dimension(subcount), intent(in) :: lonindex
00246 integer, dimension(subcount), intent(in) :: latindex
00247 logical, dimension(numvars), intent(in) :: doqp3sib  ! defines which variables are written out
00248 character(len=16), dimension(numvars), intent(in) :: nameqp3sib  ! variable names
00249 character(len=80), dimension(numvars), intent(in) :: listqp3sib  ! variable descriptions
00250 character(len=8), intent(in) :: drvr_type   ! driver data type used for simulation
00251 character(len=100), intent(in) :: biome_source
00252 character(len=100), intent(in) :: soil_source
00253 character(len=100), intent(in) :: soref_source
00254 character(len=100), intent(in) :: ndvi_source
00255 character(len=100), intent(in) :: c4_source
00256 character(len=100), intent(in) :: d13cresp_source
00257 integer, intent(out) :: qp3id           ! file id #
00258 integer, dimension(numvars), intent(out) :: qp3varid     ! variable id #s
00259 integer, intent(out) :: qp3timeid       ! time variable id #
00260 integer, intent(out) :: qp3charid       ! char_time id #
00261 integer, intent(out) :: qp3startid      ! start_period id #
00262 integer, intent(out) :: qp3endid        ! end_period id #
00263 integer, intent(out) :: qp3periodid     ! period_length id #
00264 integer, intent(in) :: rank
00265 
00266 ! netcdf variables
00267 integer :: status           ! return status of netcdf functions
00268 integer :: latid            ! latitude dimension id #
00269 integer :: lonid            ! longitude dimension id #
00270 integer :: timeid           ! time dimension id #
00271 integer :: charid           ! char_len dimension id #
00272 integer :: levelid          ! level dimension id #
00273 integer :: latitudeid       ! latitude variable id #
00274 integer :: longitudeid      ! longitude variable id #
00275 integer :: levid            ! level variable id #
00276 integer :: landpointsid     ! landpoints dimension id #
00277 integer :: lonindexid      ! lonindex variable id #
00278 integer :: latindexid      ! latindex variable id #
00279 
00280 
00281 ! local variables
00282 integer :: n                        ! index variable
00283 character(len=40) :: units          ! variable units
00284 character(len=80) :: longname       ! variable description
00285 integer :: unit_len, long_len       ! not used, used by get_units()
00286 real, dimension(nsoil) :: levels    ! numbering of levels
00287 character(len=256) :: filename      ! file name
00288 character*4 :: syear
00289 
00290     ! make sure qp3id is not tied to any open file
00291     status = nf90_close( qp3id )
00292     
00293     ! create file
00294     write( filename, '(a,i4.4,i2.2,a,i3.3,a)' ) trim(out_path)//'hsib_',   &
00295         year, month, 'p', rank, '.qp3.nc'
00296     status = nf90_create( trim(filename), nf90_clobber, qp3id)
00297     
00298     write (syear, '(i4.4)') year
00299 
00300     ! define global attributes
00301     call global_atts( qp3id, 'sib3', 'lat/lon', '1.0', drvr_type,  &
00302         biome_source, soil_source, soref_source, ndvi_source, c4_source,  &
00303         d13cresp_source, rank )
00304     
00305     ! define dimensions
00306     status = nf90_def_dim( qp3id, 'time', nf90_unlimited, timeid )
00307     status = nf90_def_dim( qp3id, 'char_len', 10, charid )
00308     status = nf90_def_dim( qp3id, 'latitude', jhr, latid )
00309     status = nf90_def_dim( qp3id, 'longitude', ihr, lonid )
00310     status = nf90_def_dim( qp3id, 'level', nsoil, levelid )
00311     status = nf90_def_dim( qp3id, 'landpoints', subcount, landpointsid )
00312     
00313     ! define control variables
00314     status = nf90_def_var( qp3id, 'time', nf90_double, (/timeid/), qp3timeid )
00315     status = nf90_put_att( qp3id, qp3timeid, 'quantity', 'time' )
00316     status = nf90_put_att( qp3id, qp3timeid, 'units', 'days since '//syear//'-01-01'  )
00317     status = nf90_put_att( qp3id, qp3timeid, 'calender', 'noleap' )
00318     
00319     status = nf90_def_var( qp3id, 'char_time', nf90_char, (/charid,timeid/), qp3charid )
00320     status = nf90_put_att( qp3id, qp3charid, 'format', 'mm/dd/yyyy' )
00321     
00322     status = nf90_def_var( qp3id, 'start_period', nf90_double, (/timeid/), qp3startid )
00323     status = nf90_put_att( qp3id, qp3startid, 'long_name', 'start of averaged period' )
00324     status = nf90_put_att( qp3id, qp3startid, 'units', 'days since '//syear//'-01-01' )
00325     
00326     status = nf90_def_var( qp3id, 'end_period', nf90_double, (/timeid/), qp3endid )
00327     status = nf90_put_att( qp3id, qp3endid, 'long_name', 'end of averaged period' )
00328     status = nf90_put_att( qp3id, qp3endid, 'units', 'days since '//syear//'-01-01' )
00329     
00330     status = nf90_def_var( qp3id, 'period_length', nf90_double, (/timeid/), qp3periodid )
00331     status = nf90_put_att( qp3id, qp3periodid, 'long_name', 'length of averaged period' )
00332     status = nf90_put_att( qp3id, qp3periodid, 'units', 'days' )
00333     
00334     status = nf90_def_var( qp3id, 'latitude', nf90_float, (/latid/), latitudeid )
00335     status = nf90_put_att( qp3id, latitudeid, 'units', 'degrees_north' )
00336     status = nf90_put_att( qp3id, latitudeid, 'quantity', 'latitude' )
00337     
00338     status = nf90_def_var( qp3id, 'longitude', nf90_float, (/lonid/), longitudeid )
00339     status = nf90_put_att( qp3id, longitudeid, 'units', 'degrees_east' )
00340     status = nf90_put_att( qp3id, longitudeid, 'quantity', 'longitude' )
00341     
00342     status = nf90_def_var( qp3id, 'level', nf90_float, (/levelid/), levid )
00343 
00344     status = nf90_def_var( qp3id, 'lonindex', nf90_int, (/landpointsid/),  &
00345         lonindexid )
00346     status = nf90_put_att( qp3id, lonindexid, 'long_name',  &
00347         'longitude index array' )
00348     status = nf90_put_att( qp3id, lonindexid, 'units', 'index-integer' )
00349 
00350     status = nf90_def_var( qp3id, 'latindex', nf90_int, (/landpointsid/),  &
00351         latindexid )
00352     status = nf90_put_att( qp3id, latindexid, 'long_name',  &
00353         'latitude index array' )
00354     status = nf90_put_att( qp3id, latindexid, 'units', 'index-integer' )
00355     
00356     ! define data variables
00357     do n = 1, numvars
00358         if ( doqp3sib(n) ) then
00359             status = nf90_def_var( qp3id, trim(nameqp3sib(n)), nf90_float, &
00360                 (/landpointsid,levelid,timeid/), qp3varid(n) )
00361             call get_units( listqp3sib(n), longname, long_len, units, unit_len )
00362             status = nf90_put_att( qp3id, qp3varid(n), 'long_name', trim(longname) )
00363             status = nf90_put_att( qp3id, qp3varid(n), 'title', trim(longname) )
00364             status = nf90_put_att( qp3id, qp3varid(n), 'units', trim(units) )
00365             status = nf90_put_att( qp3id, qp3varid(n), 'missing_value', 1.e36 )
00366         endif
00367     enddo
00368     
00369     ! switch from definition mode to data mode
00370     status = nf90_enddef( qp3id )
00371     
00372     ! assign values to variables not variant with time
00373     status = nf90_put_var( qp3id, latitudeid, latitude )
00374     status = nf90_put_var( qp3id, longitudeid, longitude )
00375     status = nf90_put_var( qp3id, latindexid, latindex )
00376     status = nf90_put_var( qp3id, lonindexid, lonindex )
00377     
00378     do n = 1, nsoil
00379         levels(n) = n
00380     enddo
00381     status = nf90_put_var( qp3id, levid, levels )
00382 
00383 end subroutine create_qp3
00384 
00385 !-----------------------------------------------------------------------
00386 
00387 subroutine write_qp3( qp3id, qp3timeid, qp3startid, qp3endid, qp3periodid,  &
00388                       qp3charid, numvars, subcount, nsoil, qp3varid,  &
00389                       qp3sib, doqp3sib, indxqp3sib, year, month,  &
00390                       day, seconds, end_period, period_length )
00391 !
00392 ! Modifications:
00393 !  Kevin Schaefer removed _date print statement (8/18/04)
00394 #ifdef PGF
00395 use netcdf
00396 use typeSizes
00397 #endif
00398 use kinds
00399 
00400 ! parameters
00401 integer, intent(in) :: qp3id            ! file id #
00402 integer, intent(in) :: qp3timeid        ! time variable id #
00403 integer, intent(in) :: qp3startid       ! start_period id #
00404 integer, intent(in) :: qp3endid         ! end_period id #
00405 integer, intent(in) :: qp3periodid      ! period_length id #
00406 integer, intent(in) :: qp3charid        ! char_time id #
00407 integer, intent(in) :: numvars          ! # of possible vars. written to file
00408 integer, intent(in) :: subcount         ! # landpoints in subdomain
00409 integer, intent(in) :: nsoil            ! # soil layers
00410 integer, dimension(numvars), intent(in) :: qp3varid    ! variable id #s
00411 real(kind=dbl_kind), dimension(subcount,nsoil,numvars), intent(in) :: qp3sib ! variable values
00412 logical, dimension(numvars), intent(in) :: doqp3sib      ! defines which variables to write out
00413 integer, dimension(numvars), intent(in) :: indxqp3sib    ! ???????????
00414 integer, intent(in) :: year     ! current year, used for char_time
00415 integer, intent(in) :: month    ! current month, used for char_time
00416 integer, intent(in) :: day      ! current day, used for char_time
00417 integer, intent(in) :: seconds  ! current second of the year
00418 double precision, intent(in) :: end_period      ! end of averaged period
00419 double precision, intent(in) :: period_length   ! length of averaged period
00420 
00421 ! netcdf variables
00422 integer :: status   ! return status of netcdf functions
00423 integer :: dimid    ! dimension id #
00424 
00425 ! local variables
00426 integer :: n, i, l      ! index variables
00427 integer :: step         ! next time step in qp file
00428 double precision :: dyear
00429 character(len=10) :: char_time      ! mm/dd/yyyy
00430 double precision :: secyear = 86400.
00431 character(len=10) :: name
00432     ! find next time step
00433     status = nf90_inq_dimid( qp3id, 'time', dimid )
00434     status = nf90_inquire_dimension( qp3id, dimid, name, step )
00435     step = step + 1
00436     
00437     ! write out time variables
00438     dyear = seconds/secyear
00439     status = nf90_put_var( qp3id, qp3timeid, dyear, (/step/) )
00440     status = nf90_put_var( qp3id, qp3startid, end_period - period_length,  &
00441         (/step/) )
00442     status = nf90_put_var( qp3id, qp3endid, end_period, (/step/) )
00443     status = nf90_put_var( qp3id, qp3periodid, period_length, (/step/) )
00444 
00445     write(char_time, '(i2.2,a1,i2.2,a1,i4.4)') month, '/', day, '/', year
00446     status = nf90_put_var( qp3id, qp3charid, char_time, (/1,step/), (/10,1/) )
00447     
00448     ! write out data variables
00449     do n = 1, numvars
00450         if ( doqp3sib(n) ) then
00451             status = nf90_put_var( qp3id, qp3varid(n),  &
00452                 qp3sib(:,:,indxqp3sib(n)), (/1,1,step/),  &
00453                 (/subcount,nsoil,1/) )
00454         endif
00455     enddo
00456 
00457 end subroutine write_qp3
00458 
00459 !-----------------------------------------------------------------------
00460 
00461 subroutine global_atts (fileID, runname, grid, version, driver,  &
00462     biome_source, soil_source, soref_source, ndvi_source, c4_source,  &
00463     d13cresp_source, rank )
00464 
00465 !-----------------------------------------------------------------------
00466 ! Purpose:
00467 !   sets global attributes 
00468 !
00469 ! Scope: 
00470 !   Module variables used:
00471 !
00472 ! Bugs:
00473 !   netcdf file (fileID) must be in define mode.
00474 !
00475 !-----------------------------------------------------------------------
00476 #ifdef PGF
00477 use netcdf
00478 use typeSizes
00479 #endif
00480 
00481 
00482 ! input parameters
00483 integer, intent(in) :: fileID
00484 character(len=*), intent(in) :: runname
00485 character(len=*), intent(in) :: grid
00486 character(len=*), intent(in) :: version
00487 character(len=*), intent(in) :: driver
00488 character(len=100), intent(in) :: biome_source
00489 character(len=100), intent(in) :: soil_source
00490 character(len=100), intent(in) :: soref_source
00491 character(len=100), intent(in) :: ndvi_source
00492 character(len=100), intent(in) :: c4_source
00493 character(len=100), intent(in) :: d13cresp_source
00494 integer, intent(in) :: rank
00495 
00496 ! local variables
00497 integer :: status
00498 character(len=30) :: current_time
00499 character(len=8) :: t_date
00500 character(len=10) :: t_time
00501 character(len=5) :: zone
00502 character(len=4) :: c_rank
00503 integer, dimension(8) :: values
00504 
00505     call date_and_time(t_date, t_time, zone, values)
00506 
00507     current_time = t_date(5:6) // "/" // t_date(7:8) // "/" // t_date(1:4)   &
00508         //" at "// t_time(1:2) // ":" //t_time(3:4) // " "      &
00509         // zone // " GMT "
00510 
00511     write( c_rank, '(i4.4)' ) rank
00512 
00513     !   add standard global attributes
00514     status = nf90_put_att ( fileID, nf90_global, 'calendar', 'noleap' )
00515     status = nf90_put_att ( fileID, nf90_global, 'institution',   &
00516         'Colorado State University' )
00517     status = nf90_put_att ( fileID, nf90_global, 'history',   &
00518         'Created: '//current_time )
00519     status = nf90_put_att( fileID, nf90_global, 'run', runname )
00520     status = nf90_put_att( fileID, nf90_global, 'rank', c_rank ) 
00521     status = nf90_put_att( fileID, nf90_global, 'grid', grid )
00522     status = nf90_put_att( fileID, nf90_global, 'version', version )
00523     status = nf90_put_att( fileID, nf90_global, 'Driver_Data', driver )
00524     status = nf90_put_att( fileID, nf90_global, 'biome_source',  &
00525         trim(biome_source) )
00526     status = nf90_put_att( fileID, nf90_global, 'soil_source',  &
00527         trim(soil_source) )
00528     status = nf90_put_att( fileID, nf90_global, 'soref_source',  &
00529         trim(soref_source) )
00530     status = nf90_put_att( fileID, nf90_global, 'ndvi_source',  &
00531         trim(ndvi_source) )
00532     status = nf90_put_att( fileID, nf90_global, 'c4_source',  &
00533         trim(c4_source) )
00534     status = nf90_put_att( fileID, nf90_global, 'd13cresp_source',  &
00535         trim(d13cresp_source) )
00536 
00537 end subroutine global_atts
00538 
00539 
00540 !-----------------------------------------------------------------------
00541 
00542 subroutine get_units(description, longname, long_len, units, unit_len)
00543 
00544 !-----------------------------------------------------------------------
00545 !  Purpose:
00546 !   extracts a string enclosed within parentheses - used for 
00547 !   units which are contained in a general description string.
00548 !   returns the units string (units) and its length (unit_len),
00549 !   the description string with the units removed (longname),
00550 !   and its length (long_len).
00551 !   note: embedded parentheses are ok
00552 !
00553 !  Variables:
00554 !   Module parameters used:
00555 !   MAXUNITS, MAXLONGN
00556 !   
00557 !  Bugs:
00558 !   1) if the rightmost parenthesis is unmatched, units will be 
00559 !      set to " " (one space) - this is to be interpreted as "none"
00560 !   2) if a "(" is unmatched, it will be part of the returned
00561 !      longname string
00562 !       3) strings of only units (i.e. entire string enclosed in 
00563 !      parentheses) do not work.
00564 !-----------------------------------------------------------------------
00565 
00566 character(len=*), intent(in) :: description
00567 character(len=*), intent(out) :: units
00568 character(len=*), intent(out) :: longname
00569 integer, intent(out) :: unit_len, long_len
00570 
00571 integer :: n, start_paren, end_paren, paren_count
00572 
00573     paren_count = 0
00574     start_paren = len_trim(description)
00575     end_paren = len_trim(description)
00576 
00577     do n = len(description), 1, -1
00578         if (description(n:n)==")") then
00579             if (paren_count == 0) then
00580                 end_paren = n
00581             endif
00582             paren_count = paren_count + 1
00583         else if (description(n:n) == "(") then
00584             paren_count = paren_count - 1
00585             if (paren_count == 0) then
00586                 start_paren = n
00587                 exit
00588             endif
00589         end if
00590     end do
00591 
00592     !   in case of confusion, clear units and return unaltered description
00593     !   note: start_paren > end_paren should not be possible, but just in case...
00594     !         start_paren = end_paren occurs when there are no units
00595     !         start_paren = end_paren-1 occurs when units are "()"
00596     !   FIXME: n==1 is too limiting - what if I wanted only units?
00597     if (n == 1 .or. start_paren >= end_paren) then   ! no units
00598         units = " "
00599         unit_len = 1
00600         longname = trim(description)
00601         long_len = len_trim(longname)
00602     else if (start_paren == (end_paren-1)) then      ! "()" case
00603         units = " "
00604         unit_len = 1
00605         longname = trim(description(:start_paren-1))// &
00606             description(end_paren+1:)
00607         long_len = len_trim(longname)
00608     else                                             ! normal units
00609         units = description(start_paren+1:end_paren-1)
00610         unit_len = len_trim(units)
00611         longname = trim(description(:start_paren-1))// &
00612             description(end_paren+1:)
00613         long_len = len_trim(longname)
00614     end if
00615 
00616 end subroutine get_units
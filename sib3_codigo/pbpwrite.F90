00001 subroutine create_pbp( npoints, year, month, numvars, totnumvars,  &
00002                        latitude, longitude, dopbpsib, namepbpsib, listpbpsib,  &
00003                        indxpbpsib, drvr_type, biome_source, soil_source,  &
00004                        soref_source, ndvi_source, c4_source, d13cresp_source,  &
00005                        outpath,pbptimeid, pbpcharid, pbpvarid,  &
00006                         rank1 )
00007 #ifdef PGF
00008 use netcdf 
00009 use typeSizes
00010 #endif
00011 
00012 
00013 ! parameters
00014 integer, intent(in) :: npoints
00015 integer, intent(in) :: year
00016 integer, intent(in) :: month
00017 integer, intent(in) :: numvars
00018 integer, intent(in) :: totnumvars
00019 real, dimension(npoints), intent(in) :: latitude
00020 real, dimension(npoints), intent(in) :: longitude
00021 logical, dimension(totnumvars), intent(in) :: dopbpsib
00022 character(len=16), dimension(totnumvars), intent(in) :: namepbpsib
00023 character(len=80), dimension(totnumvars), intent(in) :: listpbpsib
00024 integer, dimension(totnumvars), intent(in) :: indxpbpsib
00025 character(len=8), intent(in) :: drvr_type
00026 character(len=100), intent(in) :: biome_source
00027 character(len=100), intent(in) :: soil_source
00028 character(len=100), intent(in) :: soref_source
00029 character(len=100), intent(in) :: ndvi_source
00030 character(len=100), intent(in) :: c4_source
00031 character(len=100), intent(in) :: d13cresp_source
00032 character(len=256), intent(in) :: outpath
00033 
00034 integer, intent(out) :: pbptimeid
00035 integer, intent(out) :: pbpcharid
00036 integer, dimension(numvars), intent(out) :: pbpvarid
00037 integer, intent(in) :: rank1
00038 
00039 ! netcdf variables
00040 integer :: status
00041 integer :: tid
00042 integer :: clid
00043 integer :: npid
00044 integer :: npointsid
00045 integer :: latid
00046 integer :: lonid
00047 integer :: pbpid
00048 ! local variables
00049 integer :: n
00050 character(len=40) :: units          ! variable units
00051 character(len=80) :: longname       ! variable description
00052 integer :: unit_len, long_len       ! not used, returned by get_units()
00053 integer, dimension(npoints) :: npoints_array
00054 character(len =256) ::filename
00055 character*4 :: syear
00056 
00057 ! CSR : change pbpwrite time (days since year-01-01)
00058 
00059     ! create file name
00060     write( filename, '(a,i4.4,i2.2,a,i3.3,a)' ) trim(outpath)//'psib_',  &
00061         year, month, 'p', rank1, '.pbp1.nc'
00062 
00063     write (syear, '(i4.4)') year
00064 
00065     ! create file and define dimensions
00066     status = nf90_create( trim(filename), nf90_clobber, pbpid )
00067     status = nf90_def_dim( pbpid, 'time', nf90_unlimited, tid )
00068     status = nf90_def_dim( pbpid, 'char_len', 10, clid )
00069     status = nf90_def_dim( pbpid, 'npoints', npoints, npointsid )
00070     
00071     ! define global atts
00072     call global_atts( pbpid, 'sib3', 'lat/lon', '1.0', drvr_type,  &
00073         biome_source, soil_source, soref_source, ndvi_source, c4_source,  &
00074         d13cresp_source, rank1 )
00075 
00076 ! CSR redefined rank above to rank1 (no implicit programming!
00077     
00078     ! define variables
00079     status = nf90_def_var( pbpid, 'time', nf90_double, (/tid/), pbptimeid )
00080     status = nf90_put_att( pbpid, pbptimeid, 'quantity', 'time' )
00081     status = nf90_put_att( pbpid, pbptimeid, 'units', 'days since '//syear//'-01-01' )
00082     status = nf90_put_att( pbpid, pbptimeid, 'calendar', 'noleap' )
00083     
00084     status = nf90_def_var( pbpid, 'char_time', nf90_char, (/clid,tid/), pbpcharid )
00085     status = nf90_put_att( pbpid, pbpcharid, 'format', 'mm/dd/yyyy' )
00086     
00087     status = nf90_def_var( pbpid, 'npoints', nf90_int, (/npointsid/), npid )
00088 
00089     status = nf90_def_var( pbpid, 'latitude', nf90_float, (/npointsid/), latid )
00090     status = nf90_put_att( pbpid, latid, 'units', 'degrees_north' )
00091     status = nf90_put_att( pbpid, latid, 'quantity', 'latitude' )
00092     
00093     status = nf90_def_var( pbpid, 'longitude', nf90_float, (/npointsid/), lonid )
00094     status = nf90_put_att( pbpid, lonid, 'units', 'degrees_east' )
00095     status = nf90_put_att( pbpid, lonid, 'quantity', 'longitude' )
00096 
00097     do n = 1, totnumvars
00098         if ( dopbpsib(n) ) then
00099             status = nf90_def_var( pbpid, trim(namepbpsib(n)), nf90_float,  &
00100                 (/npointsid,tid/), pbpvarid(indxpbpsib(n)) )
00101             call get_units( listpbpsib(n), longname, long_len, units, unit_len )
00102             status = nf90_put_att( pbpid, pbpvarid(indxpbpsib(n)),  &
00103                 'long_name', trim(longname) )
00104             status = nf90_put_att( pbpid, pbpvarid(indxpbpsib(n)),  &
00105                 'title', trim(longname) )
00106             status = nf90_put_att( pbpid, pbpvarid(indxpbpsib(n)),  &
00107                 'units', trim(units) )
00108             status = nf90_put_att( pbpid, pbpvarid(indxpbpsib(n)),  &
00109                 'missing_value', 1.e36 )
00110         endif
00111     enddo
00112 
00113     ! switch from definition mode to data mode
00114     status = nf90_enddef( pbpid )
00115 
00116     ! assign values to variables not variant with time
00117     status = nf90_put_var( pbpid, latid, latitude )
00118     status = nf90_put_var( pbpid, lonid, longitude )
00119     
00120     do n = 1, npoints
00121         npoints_array(n) = n
00122     enddo
00123     status = nf90_put_var( pbpid, npid, npoints_array(:) )
00124     
00125     status = nf90_close( pbpid )
00126     
00127 end subroutine create_pbp
00128 
00129 
00130 !-----------------------------------------------------------------------
00131 
00132 subroutine write_pbp( npoints, year, month, day, seconds,  &
00133                       numvars, pbp, pbptid, pbpcid,  &
00134                       pbpvid,outpath, rank1 )
00135 #ifdef PGF
00136 use netcdf
00137 use typeSizes
00138 #endif
00139 use kinds
00140 
00141 ! parameters
00142 integer, intent(in) :: npoints
00143 integer, intent(in) :: year
00144 integer, intent(in) :: month
00145 integer, intent(in) :: day
00146 integer, intent(in) :: seconds
00147 integer, intent(in) :: numvars
00148 real(kind=dbl_kind), dimension(numvars+1,npoints), intent(in) :: pbp
00149 integer, intent(in) :: pbptid
00150 integer, intent(in) :: pbpcid
00151 integer, intent(in), dimension(numvars) :: pbpvid
00152 character(len=256), intent(in) :: outpath
00153 integer, intent(in) :: rank1
00154 
00155 ! netcdf variables
00156 integer :: status
00157 integer :: dimid
00158 
00159 ! local variables
00160 integer :: i, n
00161 integer :: step
00162 double precision :: dyear
00163 character(len=10) :: char_time
00164 character(len=256) :: filename
00165 character(len=10) :: name
00166 double precision :: secyear = 86400.
00167 integer :: pbpid
00168 
00169         !open file
00170         write( filename, '(a,i4.4,i2.2,a,i3.3,a)' ) trim(outpath)//'psib_',  &
00171            year, month, 'p', rank1, '.pbp1.nc'
00172         status = nf90_open( trim(filename), nf90_write, pbpid )
00173   
00174 
00175     ! find next time step
00176     status = nf90_inq_dimid( pbpid, 'time', dimid )
00177     status = nf90_inquire_dimension( pbpid, dimid, name,step )
00178     step = step + 1
00179     
00180     ! write out time variables
00181     dyear = seconds/secyear
00182     status = nf90_put_var( pbpid, pbptid, dyear, (/step/) )
00183 
00184     write( char_time, '(i2.2,a1,i2.2,a1,i4.4)' ) month, '/', day, '/', year
00185     status = nf90_put_var( pbpid, pbpcid, char_time, (/1,step/), (/10,1/) )
00186     
00187     ! write out data variables
00188     do i = 1, numvars
00189         status = nf90_put_var( pbpid, pbpvid(i), pbp(i,:),  &
00190             (/1,step/), (/npoints,1/) )
00191     enddo
00192     
00193 
00194    status = nf90_close( pbpid )
00195 
00196 end subroutine write_pbp
00197 
00198 
00199 !-----------------------------------------------------------------------
00200 
00201 subroutine create_pbp2( npoints, levels, year, month, numvars,  &
00202                         totnumvars, latitude, longitude, dopbp2sib,  &
00203                         namepbp2sib, listpbp2sib, indxpbp2sib, drvr_type,  &
00204                         biome_source, soil_source, soref_source, ndvi_source,  &
00205                         c4_source, d13cresp_source, out_path, &
00206                         pbp2timeid, pbp2charid, pbp2varid,rank )
00207 #ifdef PGF
00208 use netcdf
00209 use typeSizes
00210 #endif
00211 
00212 ! parameters
00213 integer, intent(in) :: npoints
00214 integer, intent(in) :: levels
00215 integer, intent(in) :: year
00216 integer, intent(in) :: month
00217 integer, intent(in) :: numvars
00218 integer, intent(in) :: totnumvars
00219 real, dimension(npoints), intent(in) :: latitude
00220 real, dimension(npoints), intent(in) :: longitude
00221 logical, dimension(totnumvars), intent(in) :: dopbp2sib
00222 character(len=16), dimension(totnumvars), intent(in) :: namepbp2sib
00223 character(len=80), dimension(totnumvars), intent(in) :: listpbp2sib
00224 integer, dimension(totnumvars), intent(in) :: indxpbp2sib
00225 character(len=8), intent(in) :: drvr_type
00226 character(len=100), intent(in) :: biome_source
00227 character(len=100), intent(in) :: soil_source
00228 character(len=100), intent(in) :: soref_source
00229 character(len=100), intent(in) :: ndvi_source
00230 character(len=100), intent(in) :: c4_source
00231 character(len=100), intent(in) :: d13cresp_source
00232 character(len=256), intent(in) :: out_path
00233 integer, intent(out) :: pbp2timeid
00234 integer, intent(out) :: pbp2charid
00235 integer, dimension(numvars), intent(out) :: pbp2varid
00236 integer, intent(in) :: rank
00237 
00238 ! netcdf variables
00239 integer :: status
00240 integer :: tid
00241 integer :: charid
00242 integer :: npointsid
00243 integer :: levelid
00244 integer :: npid
00245 integer :: levid
00246 integer :: latid
00247 integer :: lonid
00248 character(len=256) :: filename
00249 integer :: pbp2id
00250 
00251 ! local variables
00252 integer :: x
00253 integer, dimension(npoints) :: npoints_array
00254 integer, dimension(levels) :: levels_array
00255 character(len=40) :: units          ! variable units
00256 character(len=80) :: longname       ! variable description
00257 integer :: unit_len, long_len       ! not used, returned by get_units()
00258 character*4 :: syear
00259 
00260     ! create file name
00261     write( filename, '(a,i4.4,i2.2,a,i3.3,a)' ) trim(out_path)//'psib_',  &
00262         year, month, 'p', rank, '.pbp2.nc'
00263     
00264     write (syear,'(i4.4)') year
00265 
00266     ! make sure pbp2id is not tied to any open file
00267     status = nf90_close( pbp2id )
00268     
00269     ! create file and define dimensions
00270     status = nf90_create( trim(filename), nf90_clobber, pbp2id )
00271     status = nf90_def_dim( pbp2id, 'time', nf90_unlimited, tid )
00272     status = nf90_def_dim( pbp2id, 'char_len', 10, charid )
00273     status = nf90_def_dim( pbp2id, 'npoints', npoints, npointsid )
00274     status = nf90_def_dim( pbp2id, 'level', levels, levelid )
00275     
00276     ! define global atts
00277     call global_atts( pbp2id, 'sib3', 'lat/lon', '1.0', drvr_type,  &
00278         biome_source, soil_source, soref_source, ndvi_source, c4_source,  &
00279         d13cresp_source, rank )
00280 
00281     ! define variables
00282     status = nf90_def_var( pbp2id, 'time', nf90_double, (/tid/), pbp2timeid )
00283     status = nf90_put_att( pbp2id, pbp2timeid, 'quantity', 'time' )
00284     status = nf90_put_att( pbp2id, pbp2timeid, 'units', 'days since '//syear//'-01-01' )
00285     status = nf90_put_att( pbp2id, pbp2timeid, 'calendar', 'noleap' )
00286     
00287     status = nf90_def_var( pbp2id, 'char_time', nf90_char, (/charid,tid/), pbp2charid )
00288     status = nf90_put_att( pbp2id, pbp2charid, 'format', 'mm/dd/yyyy' )
00289     
00290     status = nf90_def_var( pbp2id, 'npoints', nf90_int, (/npointsid/), npid )
00291     
00292     status = nf90_def_var( pbp2id, 'latitude', nf90_float, (/npointsid/), latid )
00293     status = nf90_put_att( pbp2id, latid, 'units', 'degrees_north' )
00294     status = nf90_put_att( pbp2id, latid, 'quantity', 'latitude' )
00295     
00296     status = nf90_def_var( pbp2id, 'longitude', nf90_float, (/npointsid/), lonid )
00297     status = nf90_put_att( pbp2id, lonid, 'units', 'degrees_east' )
00298     status = nf90_put_att( pbp2id, lonid, 'quantity', 'longitude' )
00299     
00300     status = nf90_def_var( pbp2id, 'level', nf90_int, (/levelid/), levid )
00301 
00302     do x = 1, numvars
00303         if ( dopbp2sib(x) ) then
00304             status = nf90_def_var( pbp2id, trim(namepbp2sib(x)),  nf90_float,  &
00305                 (/npointsid,levelid,tid/), pbp2varid(indxpbp2sib(x)) )
00306             call get_units( listpbp2sib(x), longname, long_len, units, unit_len )
00307             status = nf90_put_att( pbp2id, pbp2varid(indxpbp2sib(x)),  &
00308                 'long_name', trim(longname) )
00309             status = nf90_put_att( pbp2id, pbp2varid(indxpbp2sib(x)),  &
00310                 'title', trim(longname) )
00311             status = nf90_put_att( pbp2id, pbp2varid(indxpbp2sib(x)),  &
00312                 'units', trim(units) )
00313             status = nf90_put_att( pbp2id, pbp2varid(indxpbp2sib(x)),  &
00314                 'missing_value', 1.e36 )
00315         endif
00316     enddo
00317 
00318     ! switch from define mode to data mode
00319     status = nf90_enddef( pbp2id )
00320     
00321     ! assign values to variables not variant with time
00322     status = nf90_put_var( pbp2id, latid, latitude )
00323     status = nf90_put_var( pbp2id, lonid, longitude )
00324     
00325     do x = 1, npoints
00326         npoints_array(x) = x
00327     enddo
00328     status = nf90_put_var( pbp2id, npid, npoints_array(:) )
00329     
00330     do x = 1, levels
00331         levels_array(x) = x
00332     enddo
00333     status = nf90_put_var( pbp2id, levid, levels_array(:) )
00334 
00335         status = nf90_close( pbp2id )
00336     
00337     
00338 end subroutine create_pbp2
00339 
00340 
00341 !-----------------------------------------------------------------------
00342 
00343 subroutine write_pbp2( npoints, levels, year, month, day,  &
00344                        seconds, numvars, pbp2, pbp2timeid,  &
00345                        pbp2charid, pbp2varid,out_path, rank )
00346 #ifdef PGF
00347 use netcdf
00348 use typeSizes
00349 #endif
00350 use kinds
00351 
00352 
00353 ! parameters
00354 integer, intent(in) :: npoints
00355 integer, intent(in) :: levels
00356 integer, intent(in) :: year
00357 integer, intent(in) :: month
00358 integer, intent(in) :: day
00359 integer, intent(in) :: seconds
00360 integer, intent(in) :: numvars
00361 real(kind=dbl_kind), dimension(levels,numvars+1,npoints), intent(in) :: pbp2
00362 integer, intent(in) :: pbp2timeid
00363 integer, intent(in) :: pbp2charid
00364 integer, dimension(numvars), intent(in) :: pbp2varid
00365 character(len=256), intent(in) :: out_path
00366 integer, intent(in) :: rank
00367 
00368 ! netcdf variables
00369 integer :: status
00370 integer :: dimid
00371 
00372 
00373 ! local variables
00374 integer :: i
00375 integer :: step
00376 double precision :: dyear
00377 character(len=10) :: char_time
00378 character(len=256) :: filename
00379 double precision :: secyear = 86400.
00380 character(len=10) :: name
00381 integer :: pbp2id
00382 
00383         ! create file name
00384         write( filename, '(a,i4.4,i2.2,a,i3.3,a)' ) trim(out_path)//'psib_',  &
00385             year, month, 'p', rank, '.pbp2.nc'
00386         status = nf90_open( trim(filename), nf90_write, pbp2id )
00387 
00388 
00389     ! find next time step
00390     status = nf90_inq_dimid( pbp2id, 'time', dimid )
00391     status = nf90_inquire_dimension( pbp2id, dimid, name,step )
00392     step = step + 1
00393     
00394     ! write out time variables
00395     dyear = seconds/secyear
00396     status = nf90_put_var( pbp2id, pbp2timeid, dyear, (/step/) )
00397 
00398     write( char_time, '(i2.2,a1,i2.2,a1,i4.4)' ) month, '/', day, '/', year
00399     status = nf90_put_var( pbp2id, pbp2charid, char_time, (/1,step/), (/10,1/) )
00400     
00401     ! write out data variables
00402     do i = 1, numvars
00403         status = nf90_put_var( pbp2id, pbp2varid(i), pbp2(:,i,:),  &
00404             (/1,1,step/), (/npoints,levels,1/) )
00405     enddo
00406  status = nf90_close( pbp2id )
00407 
00408 
00409 end subroutine write_pbp2
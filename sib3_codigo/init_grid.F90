00001 subroutine init_grid( rank, nchunks )
00002 !-------------------------------------------------------------
00003 ! reads in sibdrv control variables and inputs
00004 ! sets up grid
00005 !
00006 ! modofications:
00007 #ifdef PGF
00008 use netcdf
00009 use typeSizes
00010 #endif
00011 use kinds
00012 use sib_const_module
00013 use sib_io_module
00014 
00015 
00016 ! parameters
00017 integer(kind=int_kind), intent(in) :: rank
00018 integer(kind=int_kind), intent(in) :: nchunks
00019 
00020 ! netcdf variables
00021 integer(kind=int_kind) :: status    ! return value of netcdf functions
00022 integer(kind=int_kind) :: ncid      ! netcdf file id#
00023 integer(kind=int_kind) :: varid     ! netcdf variable id#
00024 integer(kind=int_kind) :: dimid     ! netcdf dimension id#
00025 integer(kind=int_kind) :: dimlen    ! netcdf dimension length
00026 character(len=12)      :: dim_name  ! netcdf dimension name
00027 ! local grid variables
00028 integer(kind=int_kind), allocatable, dimension(:,:) :: newmap ! map containing only
00029                                                           ! subdomain landpoints
00030                                                           ! indexed to nsib vector
00031 integer(kind=int_kind) :: i,j,k                 ! index variables
00032 integer(kind=int_kind) :: ntest1, ntest2, ntest3
00033 integer(kind=int_kind) :: lowerlon, upperlon    ! longitude subdomain limits
00034 integer(kind=int_kind) :: lowerlat, upperlat    ! latitude subdomain limits
00035 integer(kind=int_kind) :: lat_index             ! index value for pbp's
00036 integer(kind=int_kind) :: lon_index             ! index value for pbp's
00037 integer(kind=int_kind), allocatable, dimension(:,:) :: temp_pbp ! temporary array of
00038                                                             ! pbp coordinates
00039 
00040 
00041 
00042 
00043 ! subgridding and parallelization variables
00044 real(kind=dbl_kind) ::     dlat   ! latitude gridcell spacing
00045 real(kind=dbl_kind) ::     dlon   ! longitude gridcell spacing
00046 real(kind=dbl_kind) ::     lllat  ! lower left corner of domain
00047 real(kind=dbl_kind) ::     lllon  ! lower left corner of domain
00048 real(kind=real_kind) :: minlon    ! subdomain limits
00049 real(kind=real_kind) :: maxlon
00050 real(kind=real_kind) :: minlat
00051 real(kind=real_kind) :: maxlat
00052 integer(kind=int_kind) :: init_subcount     ! initial subcount before parallel
00053 integer(kind=int_kind), dimension(:), allocatable :: init_subset  ! initial
00054                                                      ! subset before parallel
00055 integer(kind=int_kind) :: olength
00056 integer(kind=int_kind) :: start_index   ! start index for parallelization
00057 integer(kind=int_kind) :: end_index     ! end index for parallelization
00058 real(kind=dbl_kind), dimension(:,:), allocatable :: lonlatpbp ! temporary array of
00059                                                     ! pbp coordinates
00060 
00061 !     NAMELISTS
00062 namelist /inlist_sibdrv/ & ! USER DEFINED PARAMETERS
00063     nsib,ztemp,zwind
00064 namelist /IOLIST_SIBDRV/ & !jk USER SELETED I/O OPTIONS
00065     param_path, ic_path, dr_format, out_path, qp_path,  &
00066     pbp_path, co2_path, drvr_type
00067 namelist /SUBGRID_SIBDRV/ &
00068     minlon, maxlon, minlat, maxlat
00069 namelist /PBPLIST_SIBDRV/ & ! USER DEFINED PBP DIAGNOSTIC LOCATIONS
00070     IJTLENsib
00071 namelist /SIBDRV_CONTROL_LIST/ &
00072     starttime, startyear, endtime, endyear, dtsib, dtsibmetin,  &
00073     dtsibout, dtsibres, ndtsibpbp, dtsibbcin, roll_respf, use_diffuse
00074 
00075     print *, 'INIT_GRID:'
00076 
00077     !-----------------------------------------------------------------------
00078     ! read in namel_sibdrv
00079     !-----------------------------------------------------------------------
00080     open(unit=2,file='namel_sibdrv',form='formatted')  !jk
00081     print *,'\t reading sib inlist'
00082     read (2,INLIST_SIBDRV)
00083     print *,'\t reading sib i/olst'      !jk
00084     read (2,IOLIST_SIBDRV)                         !jk
00085     print *,'\t reading subgrid values'
00086     read (2,SUBGRID_SIBDRV)
00087     print *,'\t reading sib pbplst'
00088     read (2,PBPLIST_SIBDRV)
00089     allocate (lonlatpbp(2,ijtlensib))
00090     lonlatpbp = 0.0
00091     read(2,*,err=919)lonlatpbp
00092     919  continue
00093     print *,'\t reading sib_control_lst'
00094     read (2,SIBDRV_CONTROL_LIST)
00095     close(2)
00096     print *, '\t SiB time step (s) = ',dtsib
00097     if(dtsibout > 0) then
00098         print *, '\t SiB out written (s) = ',dtsibout
00099     else
00100         print *, '\t SiB out written (months) = ',-dtsibout
00101     endif
00102     if(dtsibres > 0) then
00103         print *, '\t SiB restart written (s) = ',dtsibres
00104     else
00105         print *, '\t SiB restart written (months) = ',-dtsibres
00106     endif
00107 
00108     histpp = ndtsibpbp /= 0
00109     !-----------------------------------------------------------------------
00110     ! read in grid information
00111     !-----------------------------------------------------------------------
00112     allocate( latsib(nsib) )
00113     allocate( lonsib(nsib) )
00114  print*, 'drvr_type=',drvr_type
00115     if(drvr_type=='single')then  
00116   
00117         ! assign some grid information and exit subroutine
00118         subcount = 1
00119         allocate(subset(1))
00120         allocate(imultpbpsib(1))
00121         allocate(newmap(1,1))
00122         allocate(latpbp(1))
00123         allocate(lonpbp(1))
00124         allocate(latindex(1))
00125         allocate(lonindex(1))
00126         allocate(sublon(1))
00127         allocate(sublat(1))
00128         allocate(latitude(1))
00129         allocate(longitude(1))
00130         subset(1) = 1
00131         imultpbpsib(1) = 1
00132         ijtlensib = 1
00133         newmap(1,1) = 1
00134         ihr = 1
00135         jhr = 1
00136         nhr = 1
00137         latpbp(1) = lonlatpbp(2,1)
00138         lonpbp(1) = lonlatpbp(1,1)
00139         latindex(1) = 1
00140         lonindex(1) = 1
00141         sublon(1) = 1
00142         sublat(1) = 1
00143 
00144     endif
00145 
00146     allocate( latindex(nsib) )
00147     allocate( lonindex(nsib) )
00148     status = nf90_open( trim(param_path)//'TI.nc', nf90_nowrite, ncid )
00149     if ( status /= nf90_noerr ) call handle_err( status )
00150     status = nf90_inq_dimid( ncid, 'nsib', dimid )
00151     if ( status /= nf90_noerr ) call handle_err( status )
00152     status = nf90_inquire_dimension( ncid, dimid, dim_name,dimlen )
00153     if ( status /= nf90_noerr ) call handle_err( status )
00154     if ( dimlen /= nsib ) print *, dimlen, 'and', nsib, "don\'t match"
00155     
00156     status = nf90_inq_varid( ncid, 'latsib', varid )
00157     status = nf90_get_var( ncid, varid, latsib )
00158     status = nf90_inq_varid( ncid, 'lonsib', varid )
00159     status = nf90_get_var( ncid, varid, lonsib )
00160 
00161 !itb...if a single point, can leave this now...
00162    if(drvr_type == 'single' ) then
00163         latitude(1) = latsib(1)
00164         longitude(1) = lonsib(1)  
00165         status = nf90_close( ncid )
00166         return
00167    endif
00168 
00169 
00170     status = nf90_inq_varid( ncid, 'latindex', varid )
00171     status = nf90_get_var( ncid, varid, latindex )
00172     status = nf90_inq_varid( ncid, 'lonindex', varid )
00173     status = nf90_get_var( ncid, varid, lonindex )
00174     status = nf90_inq_varid( ncid, 'numlat', varid )
00175     status = nf90_get_var( ncid, varid, jhr )
00176     status = nf90_inq_varid( ncid, 'numlon', varid )
00177     status = nf90_get_var( ncid, varid, ihr )
00178     status = nf90_inq_varid( ncid, 'dlat', varid )
00179     status = nf90_get_var( ncid, varid, dlat )
00180     status = nf90_inq_varid( ncid, 'dlon', varid )
00181     status = nf90_get_var( ncid, varid, dlon )
00182     status = nf90_inq_varid( ncid, 'lllat', varid )
00183     status = nf90_get_var( ncid, varid, lllat )
00184     status = nf90_inq_varid( ncid, 'lllon', varid )
00185     status = nf90_get_var( ncid, varid, lllon )
00186     
00187     status = nf90_close( ncid )
00188 
00189     nhr = ihr * jhr
00190 
00191     allocate( latitude(jhr) )
00192     allocate( longitude(ihr) )
00193     longitude(1) = lllon
00194     do i = 2, ihr
00195         longitude(i) = longitude(i-1) + dlon
00196     enddo
00197     latitude(1) = lllat
00198     do i = 2, jhr
00199         latitude(i) = latitude(i-1) + dlat
00200     enddo
00201 
00202     !-----------------------------------------------------------------------
00203     ! calculate subset
00204     !-----------------------------------------------------------------------
00205     allocate( newmap(ihr,jhr) )
00206     newmap(:,:) = 0
00207     do i = 1, nsib
00208         newmap( lonindex(i), latindex(i) ) = i
00209     enddo
00210     
00211     ! convert domain limits to indices
00212     lowerlon = int( (minlon-lllon)/dlon + 1 )
00213     upperlon = int( (maxlon-lllon)/dlon + 1 )
00214     lowerlat = int( (minlat-lllat)/dlat + 1 )
00215     upperlat = int( (maxlat-lllat)/dlat + 1 )
00216     
00217     ! make sure we stay within the domain
00218     if ( lowerlon < 1 ) lowerlon = 1
00219     if ( upperlon > ihr ) upperlon = ihr
00220     if ( lowerlat < 1 ) lowerlat = 1
00221     if ( upperlat > jhr ) upperlat = jhr
00222     
00223     ! count number of landpoints in subdomain
00224     init_subcount = 0
00225     do j = lowerlon, upperlon
00226         do i = lowerlat, upperlat
00227             if ( newmap(j,i) > 0 ) init_subcount = init_subcount + 1
00228         enddo
00229     enddo
00230     
00231 
00232     ! create vector indexing landpoints in subdomain
00233     allocate( init_subset(init_subcount) )
00234     init_subcount = 0
00235     do i = lowerlat, upperlat
00236         do j = lowerlon, upperlon
00237             if ( newmap(j,i) > 0 ) then
00238                 init_subcount = init_subcount + 1
00239                 init_subset(init_subcount) = newmap(j,i)
00240             endif
00241         enddo
00242     enddo
00243     
00244     ! calculate subcount for parallelization
00245     olength = init_subcount / nchunks
00246     subcount = olength
00247     if ( nchunks == 1 ) then
00248         subcount = init_subcount
00249     elseif ( rank == nchunks ) then
00250         if ( (rank-1)*olength + olength <= init_subcount ) then
00251             subcount = init_subcount - (rank-1)*olength
00252         else
00253             subcount = mod( init_subcount, olength*(nchunks-1) )
00254         endif
00255     endif
00256     print*, '\t nsib=',subcount, 'nsibmax=',nsib
00257     
00258     ! calculate starting and ending vertices
00259     start_index = (rank-1) * olength + 1
00260     end_index = start_index + subcount - 1
00261    
00262     ! allocate subset and assign values
00263     allocate( subset(subcount) )
00264     subset(:) = init_subset( start_index : end_index )
00265     deallocate( init_subset )
00266 
00267     ! fill 2d map with new landmask
00268     allocate( sublat(subcount) )
00269     allocate( sublon(subcount) )
00270     newmap(:,:) = 0
00271     do i = 1, subcount
00272         newmap(lonindex(subset(i)),latindex(subset(i))) = i
00273         sublat(i) = latindex(subset(i))
00274         sublon(i) = lonindex(subset(i))
00275     enddo
00276     
00277    
00278     !-----------------------------------------------------------------------
00279     ! Find pbp indices and remove duplicates
00280     !-----------------------------------------------------------------------
00281     allocate(temp_pbp(2,ijtlensib))
00282     temp_pbp(:,:) = 0
00283     do i = 1, ijtlensib
00284         ! find latitude index
00285         lat_index = int( (lonlatpbp(2,i)-lllat)/dlat + 1 )
00286 
00287         ! find longitude index
00288         lon_index = int( (lonlatpbp(1,i)-lllon)/dlon + 1 )
00289 
00290         if ( lat_index < 1 .or. lat_index > jhr .or.  &
00291              lon_index < 1 .or. lon_index > ihr ) then
00292              
00293             print *, 'Point ', lonlatpbp(1,i), lonlatpbp(2,i),  &
00294                 'is not inside the grid, please fix this.'
00295             stop
00296         endif
00297         if ( newmap(lon_index,lat_index) /= 0 ) then
00298             temp_pbp(1,i) = lon_index
00299             temp_pbp(2,i) = lat_index
00300         endif
00301     enddo
00302 
00303     ! remove duplicates
00304     do i = 1, ijtlensib-1
00305         do j = i+1, ijtlensib
00306             if ( temp_pbp(1,i) == temp_pbp(1,j) .and.  &
00307                  temp_pbp(2,i) == temp_pbp(2,j) .and.  &
00308                  temp_pbp(1,i) /= 0 .and. temp_pbp(2,i) /= 0 ) then
00309 
00310                 ! duplicate, set second instance to zero
00311                 temp_pbp(1,j) = 0
00312                 temp_pbp(2,j) = 0
00313                 print *, 'point', lonlatpbp(1,j), lonlatpbp(2,j),  &
00314                     ' is a duplicate of point', lonlatpbp(1,i), lonlatpbp(2,i)
00315                 print *, 'This point has been removed'
00316             endif
00317         enddo
00318     enddo
00319 
00320     ! count remaining number of pbp's, allocate new array and copy subset index
00321     j = 0
00322     do i = 1, ijtlensib
00323         if ( temp_pbp(1,i) /= 0 .and. temp_pbp(2,i) /= 0 )  j = j + 1
00324     enddo
00325 
00326     if ( j > 0 ) then
00327         ! there is at least one pbp in the subdomain
00328         allocate(imultpbpsib(j))
00329         allocate( latpbp(j) )
00330         allocate( lonpbp(j) )
00331         j = 0
00332         do i = 1, ijtlensib
00333             if ( temp_pbp(1,i) /= 0 .and. temp_pbp(2,i) /= 0 )  then
00334                 j = j + 1
00335                 imultpbpsib(j) = newmap(temp_pbp(1,i),temp_pbp(2,i))
00336                 latpbp(j) = lonlatpbp(2,i)
00337                 lonpbp(j) = lonlatpbp(1,i)
00338             endif
00339         enddo
00340         ijtlensib = j
00341     else
00342         histpp = .false.
00343     endif
00344 
00345     deallocate( temp_pbp )
00346     deallocate( lonlatpbp )
00347     deallocate( newmap )
00348 
00349 end subroutine init_grid
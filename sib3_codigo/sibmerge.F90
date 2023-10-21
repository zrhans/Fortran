00001 
00002 program sibmerge
00003 !-------------------------------------------------------------------------------
00004 ! Author  :  Owen Leonard
00005 ! Date    :  May 19, 2004
00006 ! Purpose :
00007 !   This program merges the output files from SiB3 after a simulation has run 
00008 ! using multiple processes.  For each month, sibmerge 
00009 !   1) copies the files saved for each cluster node to your current directory
00010 !   2) merges them
00011 !   3) writes the merged file to the output directory
00012 !   4) deletes the original, unmerged files from your local directory
00013 ! This eliminates the need to manually copy files to and from directories,
00014 ! a tedious process prone to error and limited by local storage space.
00015 ! The merging of each file type is optional, since merging of all file types
00016 ! simultaniously is not always required or desirable.
00017 !
00018 ! Notes   :
00019 !   This program requires the netcdf library.
00020 !
00021 ! Modifications:
00022 !  Owen Leonard Created program (5/19/04)
00023 !  Kevin Schaefer switched from argument list to input control file (9/22/04)
00024 !  Kevin Schaefer separated file merge loops by file type & made each optional (9/22/04)
00025 !  Kevin Schaefer made all file copy operations internal to sibmerge (9/22/04)
00026 !-------------------------------------------------------------------------------
00027 
00028 implicit none
00029 
00030 ! input variables
00031   integer :: start_year       ! beginning year of simulation
00032   integer :: start_month      ! beginning month of simulation
00033   integer :: end_year         ! ending year of simulation
00034   integer :: end_month        ! ending month of simulation
00035   logical :: merge_restart    ! flag to merge restart files
00036   logical :: merge_qp2        ! flag to merge qp2 files
00037   logical :: merge_qp3        ! flag to merge qp3 files
00038   logical :: merge_pbp2       ! flag to merge pbp2 files
00039   logical :: merge_pbp3       ! flag to merge pbp3 files
00040   logical :: merge_respfac    ! flag to merge respfactor files
00041   integer :: nprocs           ! number of processes in parallel run
00042   character(len=256), dimension(:), allocatable :: in_path ! paths of input files to be merged
00043   character(len=256) :: out_path  ! directory where merged output files will be written
00044 
00045 ! internal variables
00046   character(len=256) :: outfile   ! name of merged output file
00047   character(len=256), dimension(:), allocatable :: filenames ! file names for input files to be merged
00048   character(len=256) :: command                  ! temp variable holding value of command line args
00049   Character(len=45) Junk   ! garbage variable for reading input descriptors
00050   integer :: p, m, y          ! index variables
00051 !
00052 ! Open input control file, merge.in
00053     OPEN (Unit=9, File='sibmerge.in', Status='old')
00054 !
00055 ! read input file header lines
00056     read (9,10) junk
00057     read (9,10) junk
00058 !
00059 ! read inputs
00060     read (9,11) junk, start_year
00061     read (9,11) junk, start_month
00062     read (9,11) junk, end_year
00063     read (9,11) junk, end_month
00064 !print*,start_year,start_month, end_year,end_month
00065     read (9,24) junk, merge_restart
00066 !print*,merge_restart
00067     read (9,24) junk, merge_qp2
00068     read (9,24) junk, merge_qp3
00069     read (9,24) junk, merge_pbp2
00070     read (9,24) junk, merge_pbp3
00071     read (9,24) junk, merge_respfac
00072 !
00073 ! read input & output paths
00074     read (9,11) junk, nprocs
00075       allocate(in_path(nprocs))
00076       read (9,10) junk
00077       do p=1,nprocs
00078         read (9,17) in_path(p)
00079       enddo
00080     read (9,22) junk, out_path
00081 !
00082 ! standard formats for input
00083 10    Format (a45)
00084 11    Format (a45, I4)
00085 12    Format (a45, a2)
00086 13    Format (a45, E15.8)
00087 14    Format (a45, f6.4)
00088 16    Format (a45, a40)
00089 17    Format (a100)
00090 18    Format (I2,x,a10,I4,a1)
00091 19    Format (a45, a15)
00092 20    Format (a45, a11)
00093 21    Format (a45, a8)
00094 22    Format (a45, a100)
00095 23    Format (a45, a20)
00096 24    Format (a45, L7)
00097 !
00098 ! check inputs
00099     if ( nprocs < 2 ) stop 'number processors must be > 1'
00100 !
00101 ! allocate input filename array
00102     allocate( filenames(nprocs) )
00103 !
00104 !-------------------------------------------------------
00105 ! Merge Restart files
00106 !-------------------------------------------------------
00107     if(merge_restart) then
00108       print *, '  combining restart files'
00109       do y = start_year, end_year
00110         do m = 1, 12
00111 !
00112 ! don't exceed start and stop months
00113             if ( y == start_year .and. m < start_month ) cycle
00114             if ( y == end_year .and. m > end_month ) exit
00115             print *, 'Processing restart ', m, y
00116 !
00117 ! create input filenames
00118             do p = 1, nprocs
00119                 write( filenames(p), '(a,i4.4,i2.2,a,i3.3,a)' )  &
00120                     'sib_r', y, m, 'p', p, '.nc'
00121             enddo
00122 !
00123 ! Copy files to local directory
00124             do p = 1, nprocs
00125               command="scp "//trim(in_path(p))//trim(filenames(p))//" ."
00126               call system( trim(command) )
00127             enddo
00128 !
00129 ! create output filename
00130             write( outfile, '(a,i4.4,i2.2,a)' ) trim(out_path)//'sib_r', y, m, '.nc'
00131 !
00132 ! merge files
00133             call restartmerge( nprocs, filenames, outfile )
00134 !
00135 ! remove files from local/source directory
00136             do p = 1, nprocs
00137               command="rm "//trim(filenames(p))
00138               call system( trim(command) )
00139 
00140               command="rm "//trim(in_path(p))//trim(filenames(p))
00141               call system( trim(command) )
00142             enddo
00143         enddo
00144       enddo
00145     endif
00146 !
00147 !-------------------------------------------------------
00148 ! merge qp2
00149 !-------------------------------------------------------
00150     if(merge_qp2) then
00151       print *, '  combining qp2 files'
00152       do y = start_year, end_year
00153         do m = 1, 12
00154 !
00155 ! don't exceed start and stop months
00156             if ( y == start_year .and. m < start_month ) cycle
00157             if ( y == end_year .and. m > end_month ) exit
00158             print *, 'Processing qp2 ', m, y
00159 !
00160 ! create input filenames
00161             do p = 1, nprocs
00162               write( filenames(p), '(a,i4.4,i2.2,a,i3.3,a)' )  &
00163                         'hsib_', y, m, 'p', p, '.qp2.nc'
00164             enddo
00165 !
00166 ! Copy files to local directory
00167             do p = 1, nprocs
00168               command="scp "//trim(in_path(p))//trim(filenames(p))//" ."
00169               call system( trim(command) )
00170             enddo
00171 !
00172 ! create output filename
00173             write( outfile, '(a,i4.4,i2.2)' ) trim(out_path)//'hsib_', y, m
00174 !
00175 ! merge files
00176             call qp2merge( nprocs, filenames, outfile )
00177 !
00178 ! remove files from local/source directory
00179             do p = 1, nprocs
00180               command="rm "//trim(filenames(p))
00181               call system( trim(command) )
00182 
00183               command="rm "//trim(in_path(p))//trim(filenames(p))
00184               call system( trim(command) )
00185             enddo
00186         enddo
00187       enddo
00188     endif
00189 !
00190 !-------------------------------------------------------
00191 ! merge qp3
00192 !-------------------------------------------------------
00193     if(merge_qp3) then
00194       print *, '  combining qp3 files'
00195       do y = start_year, end_year
00196         do m = 1, 12
00197 !
00198 ! don't exceed start and stop months
00199             if ( y == start_year .and. m < start_month ) cycle
00200             if ( y == end_year .and. m > end_month ) exit
00201             print *, 'Processing qp3 ', m, y
00202 !
00203 ! create input filenames
00204             do p = 1, nprocs
00205               write( filenames(p), '(a,i4.4,i2.2,a,i3.3,a)' )  &
00206                         'hsib_', y, m, 'p', p, '.qp3.nc'
00207             enddo
00208 !
00209 ! Copy files to local directory
00210             do p = 1, nprocs
00211               command="scp "//trim(in_path(p))//trim(filenames(p))//" ."
00212               call system( trim(command) )
00213             enddo
00214 !
00215 ! create output filename
00216             write( outfile, '(a,i4.4,i2.2)' ) trim(out_path)//'hsib_', y, m
00217 !
00218 ! merge files
00219             call qp3merge( nprocs, filenames, outfile )
00220 !
00221 ! remove files from local/source directory
00222             do p = 1, nprocs
00223               command="rm "//trim(filenames(p))
00224               call system( trim(command) )
00225 
00226               command="rm "//trim(in_path(p))//trim(filenames(p))
00227               call system( trim(command) )
00228             enddo
00229         enddo
00230       enddo
00231     endif
00232 !
00233 !-------------------------------------------------------
00234 ! merge pbp2
00235 !-------------------------------------------------------
00236     if(merge_pbp2) then
00237       print *, '  combining pbp2 files'
00238       do y = start_year, end_year
00239         do m = 1, 12
00240 !
00241 ! don't exceed start and stop months
00242             if ( y == start_year .and. m < start_month ) cycle
00243             if ( y == end_year .and. m > end_month ) exit
00244             print *, 'Processing pbp1 ', m, y
00245 !
00246 ! create input filenames
00247             do p = 1, nprocs
00248               write( filenames(p), '(a,i4.4,i2.2,a,i3.3,a)' )  &
00249                         'psib_', y, m, 'p', p, '.pbp1.nc'
00250             enddo
00251 !
00252 ! Copy files to local directory
00253             do p = 1, nprocs
00254               command="scp "//trim(in_path(p))//trim(filenames(p))//" ."
00255               call system( trim(command) )
00256             enddo
00257 !
00258 ! create output filename
00259             write( outfile, '(a,i4.4,i2.2,a)' ) trim(out_path)//'psib_', y, m, '.pbp1.nc'
00260 !
00261 ! merge files
00262             call pbpmerge( nprocs, filenames, outfile )
00263 !
00264 ! remove files from local/source directory
00265             do p = 1, nprocs
00266               command="rm "//trim(filenames(p))
00267               call system( trim(command) )
00268 
00269               command="rm "//trim(in_path(p))//trim(filenames(p))
00270               call system( trim(command) )
00271             enddo
00272         enddo
00273       enddo
00274     endif
00275 !
00276 !-------------------------------------------------------
00277 ! merge pbp3
00278 !-------------------------------------------------------
00279     if(merge_pbp3) then
00280       print *, '  combining pbp3 files'
00281       do y = start_year, end_year
00282         do m = 1, 12
00283 !
00284 ! don't exceed start and stop months
00285             if ( y == start_year .and. m < start_month ) cycle
00286             if ( y == end_year .and. m > end_month ) exit
00287             print *, 'Processing pbp2 ', m, y
00288 !
00289 ! create input filenames
00290             do p = 1, nprocs
00291               write( filenames(p), '(a,i4.4,i2.2,a,i3.3,a)' )  &
00292                         'psib_', y, m, 'p', p, '.pbp2.nc'
00293             enddo
00294 !
00295 ! Copy files to local directory
00296             do p = 1, nprocs
00297               command="scp "//trim(in_path(p))//trim(filenames(p))//" ."
00298               call system( trim(command) )
00299             enddo
00300 !
00301 ! create output filename
00302             write( outfile, '(a,i4.4,i2.2,a)' ) trim(out_path)//'psib_', y, m, '.pbp2.nc'
00303 !
00304 ! merge files
00305             call pbpmerge( nprocs, filenames, outfile )
00306 !
00307 ! remove files from local directory
00308             do p = 1, nprocs
00309               command="rm "//trim(filenames(p))
00310               call system( trim(command) )
00311 
00312               command="rm "//trim(in_path(p))//trim(filenames(p))
00313               call system( trim(command) )
00314             enddo
00315         enddo
00316       enddo
00317     endif
00318 !
00319 !-------------------------------------------------------
00320 ! merge respfactor files
00321 !-------------------------------------------------------
00322     if(merge_respfac) then
00323       print *, '  combining respfactor files'
00324       do y = start_year, end_year
00325         print *, 'Processing respfactor ',y
00326 !
00327 ! create input filenames
00328         do p = 1, nprocs
00329           write( filenames(p), '(a,i4.4,a,i3.3,a)' )  &
00330              'CO2_respf_', y, 'p', p, '.nc'
00331         enddo
00332 !
00333 ! Copy files to local directory
00334             do p = 1, nprocs
00335               command="scp "//trim(in_path(p))//trim(filenames(p))//" ."
00336               call system( trim(command) )
00337             enddo
00338 !
00339 ! create output filename
00340         write( outfile, '(a,i4.4,a)') trim(out_path)//'CO2_respf_', y,'.nc'
00341 !
00342 ! merge files
00343         call new_respfmerge( nprocs, filenames, outfile )
00344 !
00345 ! remove files from local directory
00346         do p = 1, nprocs
00347           command="rm "//trim(filenames(p))
00348           call system( trim(command) )
00349 
00350           command="rm "//trim(in_path(p))//trim(filenames(p))
00351           call system( trim(command) )
00352         enddo
00353       enddo
00354     endif
00355 
00356     deallocate( filenames )
00357 
00358 print*, 'end sibmerge program'
00359 end program sibmerge
00360 !
00361 !========================================================
00362 subroutine qp2merge( num, filenames, out_path )
00363 !========================================================
00364 ! Modifications:
00365 !  Owen Leonard Created subroutine (5/19/04)
00366 !  Kevin Schaefer added comments (9/22/04)
00367 !  Kevin Schaefer added calls to error handle subroutine (9/22/04)
00368 !  Kevin Schaefer deleted single processor copy (never allowed) (9/22/04)
00369 !--------------------------------------------------------
00370 
00371 use netcdf
00372 use typeSizes
00373 implicit none
00374 
00375 ! parameters
00376 integer, intent(in) :: num                                   ! # processes
00377 character(len=256), dimension(num), intent(in) :: filenames  ! input file names
00378 character(len=256), intent(in) :: out_path                   ! output file name
00379 
00380 ! netcdf variables
00381 integer :: status                              ! return value of netcdf function
00382 integer, dimension(num) :: ncid                ! input file id#s
00383 integer :: outid                               ! output file id#
00384 integer :: unlimitid                           ! unlimited dimension id#
00385 integer :: numvars                             ! # variables in input files
00386 integer :: numatts                             ! # attributes in input files
00387 character(len=20) :: name                      ! variable name
00388 integer, dimension(:), allocatable :: dimid    ! dimension id#s
00389 integer, dimension(:), allocatable :: vardims  ! variable dimensions
00390 integer :: dimlen                              ! dimension length
00391 integer :: timelen                             ! dimension length
00392 integer :: landlen                             ! dimension length
00393 integer :: latlen                              ! dimension length
00394 integer :: lonlen                              ! dimension length
00395 integer :: xtype                               ! variable data type
00396 integer :: numdims                             ! # dimensions in input files
00397 integer :: varid                               ! variable id#
00398 
00399 ! local variables
00400 character(len=256) :: outfile                  ! output file name
00401 character(len=256) :: command                  ! output file name
00402 double precision, dimension(:), allocatable :: dvalues         ! input data values
00403 character(len=10), dimension(:), allocatable :: cvalues        ! output data values
00404 real, dimension(:,:), allocatable :: values   ! input data values
00405 real, dimension(:,:), allocatable :: result   ! output data values
00406 integer, dimension(:), allocatable :: lonindex
00407 integer, dimension(:), allocatable :: latindex
00408 integer :: f, n, a, d                          ! index variables
00409 integer :: landpoints
00410 !
00411 ! create output file name and create file
00412     write( outfile, '(a,i1.1,a)' ), trim(out_path)//'.qp', 2, '.nc'
00413 !
00414 ! open all input files
00415     do f = 1, num
00416         status = nf90_open( trim(filenames(f)), nf90_nowrite, ncid(f) )
00417         call handle_err(status,'qpmerge',1)
00418     enddo
00419 !
00420 ! get dimensions and attributes
00421     status = nf90_inquire( ncid(1), nDimensions=numdims, nVariables=numvars,  &
00422         nAttributes=numatts, unlimitedDimId=unlimitid )
00423     call handle_err(status,'qpmerge',2)
00424 !
00425 ! create merged output file
00426     status = nf90_create( trim(outfile), nf90_clobber, outid )
00427     call handle_err(status,'qpmerge',3)
00428 !
00429 ! copy global attributes over to output file
00430     do a = 1, numatts
00431         status = nf90_inq_attname( ncid(1), nf90_global, a, name )
00432         call handle_err(status,'qpmerge',4)
00433         status = nf90_copy_att( ncid(1), nf90_global, trim(name),  &
00434             outid, nf90_global )
00435         call handle_err(status,'qpmerge',5)
00436     enddo
00437 !
00438 ! copy variable dimensions over to output file
00439     allocate( dimid(numdims) )
00440     do d = 1, numdims - 1
00441         if ( d /= unlimitid ) then
00442             status = nf90_inquire_dimension( ncid(1), d, name=name, len=dimlen )
00443             call handle_err(status,'qpmerge',6)
00444             status = nf90_def_dim( outid, trim(name), dimlen, dimid(d) )
00445             call handle_err(status,'qpmerge',7)
00446         else
00447             status = nf90_def_dim( outid, 'time', nf90_unlimited, dimid(d) )
00448             call handle_err(status,'qpmerge',8)
00449         endif
00450     enddo
00451 !
00452 ! calculate total number of land points and write to output file
00453     landpoints = 0
00454     do f = 1, num
00455         status = nf90_inquire_dimension( ncid(f), numdims, name=name, len=dimlen )
00456         call handle_err(status,'qpmerge',9)
00457         landpoints = landpoints + dimlen
00458     enddo
00459     status = nf90_def_dim( outid, trim(name), landpoints, dimid(numdims) )
00460     call handle_err(status,'qpmerge',10)
00461 !
00462 ! define time, lat, & lon variables in output file (excluding lonindex and latindex)
00463     allocate( vardims(numdims) )
00464     do n = 1, 7
00465         status = nf90_inquire_variable( ncid(1), n, name=name, xtype=xtype,  &
00466             ndims=numdims, dimids=vardims, natts=numatts )
00467         call handle_err(status,'qpmerge',11)
00468         status = nf90_def_var( outid, trim(name), xtype,  &
00469             vardims(1:numdims), varid )
00470         call handle_err(status,'qpmerge',12)
00471 
00472         ! copy attributes over
00473         do a = 1, numatts
00474             status = nf90_inq_attname( ncid(1), n, a, name=name )
00475             call handle_err(status,'qpmerge',13)
00476             status = nf90_copy_att( ncid(1), n, trim(name), outid, n )
00477             call handle_err(status,'qpmerge',14)
00478         enddo
00479     enddo
00480 !
00481 ! define lonindex variable in output file
00482     status = nf90_inquire_variable( ncid(1), 8, name=name, xtype=xtype,  &
00483         natts=numatts )
00484     call handle_err(status,'qpmerge',15)
00485     status = nf90_def_var( outid, trim(name), xtype, (/5/), varid )
00486     call handle_err(status,'qpmerge',16)
00487     do a = 1, numatts
00488         status = nf90_inq_attname( ncid(1), 8, a, name=name )
00489         call handle_err(status,'qpmerge',17)
00490         status = nf90_copy_att( ncid(1), 8, trim(name), outid, varid )
00491         call handle_err(status,'qpmerge',18)
00492     enddo
00493 !
00494 ! define latindex variable in output file
00495     status = nf90_inquire_variable( ncid(1), 9, name=name, xtype=xtype,  &
00496         natts=numatts )
00497     call handle_err(status,'qpmerge',19)
00498     status = nf90_def_var( outid, trim(name), xtype, (/5/), varid )
00499     call handle_err(status,'qpmerge',20)
00500     do a = 1, numatts
00501         status = nf90_inq_attname( ncid(1), 9, a, name=name )
00502         call handle_err(status,'qpmerge',21)
00503         status = nf90_copy_att( ncid(1), 9, trim(name), outid, varid )
00504         call handle_err(status,'qpmerge',22)
00505     enddo
00506 !
00507 ! define primary variables in output file
00508     do n = 10, numvars
00509         status = nf90_inquire_variable( ncid(1), n, name=name, xtype=xtype,  &
00510             natts=numatts )
00511         call handle_err(status,'qpmerge',23)
00512         status = nf90_def_var( outid, trim(name), xtype, (/5,1/), varid )
00513         call handle_err(status,'qpmerge',24)
00514         do a = 1, numatts
00515             status = nf90_inq_attname( ncid(1), n, a, name=name )
00516             call handle_err(status,'qpmerge',25)
00517             status = nf90_copy_att( ncid(1), n, trim(name), outid, varid )
00518             call handle_err(status,'qpmerge',26)
00519         enddo
00520     enddo
00521 !
00522 ! stop defining variables
00523     status = nf90_enddef( outid )
00524     call handle_err(status,'qpmerge',27)
00525 !
00526 ! move values for time and lat/lon variables to output file
00527     do n = 1, 7
00528         status = nf90_inquire_variable( ncid(1), n, xtype=xtype,  &
00529             ndims=numdims, dimids=vardims )
00530         call handle_err(status,'qpmerge',28)
00531         if ( xtype == nf90_double ) then
00532             status = nf90_inquire_dimension( ncid(1), vardims(1), len=dimlen )
00533             call handle_err(status,'qpmerge',29)
00534             allocate( dvalues(dimlen) )
00535             status = nf90_get_var( ncid(1), n, dvalues )
00536             call handle_err(status,'qpmerge',30)
00537             status = nf90_put_var( outid, n, dvalues )
00538             call handle_err(status,'qpmerge',31)
00539             deallocate( dvalues )
00540         else if ( xtype == nf90_char ) then
00541             status = nf90_inquire_dimension( ncid(1), vardims(2), len=dimlen )
00542             call handle_err(status,'qpmerge',32)
00543             allocate( cvalues(dimlen) )
00544             status = nf90_get_var( ncid(1), n, cvalues )
00545             call handle_err(status,'qpmerge',33)
00546             status = nf90_put_var( outid, n, cvalues )
00547             call handle_err(status,'qpmerge',34)
00548             deallocate( cvalues )
00549         else 
00550             status = nf90_inquire_dimension( ncid(1), vardims(1), len=dimlen )
00551             call handle_err(status,'qpmerge',35)
00552             allocate( values(dimlen,1) )
00553             status = nf90_get_var( ncid(1), n, values )
00554             call handle_err(status,'qpmerge',36)
00555             status = nf90_put_var( outid, n, values )
00556             call handle_err(status,'qpmerge',37)
00557             deallocate( values )
00558         endif
00559     enddo
00560 !
00561 ! copy lonindex and latindex values over to output file
00562     allocate( lonindex(landpoints) )
00563     allocate( latindex(landpoints) )
00564     a = 1
00565     do f = 1, num
00566         status = nf90_inquire_dimension( ncid(f), 5, len=landlen )
00567         call handle_err(status,'qpmerge',38)
00568         status = nf90_get_var( ncid(f), 8, lonindex(a:a+landlen-1) )
00569         call handle_err(status,'qpmerge',39)
00570         status = nf90_get_var( ncid(f), 9, latindex(a:a+landlen-1) )
00571         call handle_err(status,'qpmerge',40)
00572         a = a + landlen
00573     enddo
00574     
00575     status = nf90_put_var( outid, 8, lonindex )
00576     call handle_err(status,'qpmerge',41)
00577     status = nf90_put_var( outid, 9, latindex )
00578     call handle_err(status,'qpmerge',41)
00579     deallocate( lonindex )
00580     deallocate( latindex )
00581 !
00582 ! copy variables over to output file
00583     status = nf90_inquire_dimension( ncid(1), 1, len=timelen )
00584     call handle_err(status,'qpmerge',42)
00585     allocate( result(landpoints,timelen) )
00586     do n = 10, numvars
00587         a = 1
00588         do f = 1, num
00589             status = nf90_inquire_dimension( ncid(f), 5, len=landlen )
00590             call handle_err(status,'qpmerge',43)
00591             status = nf90_get_var( ncid(f), n, result(a:a+landlen-1,:) )
00592             call handle_err(status,'qpmerge',44)
00593             a = a + landlen
00594         enddo
00595         status = nf90_put_var( outid, n, result )
00596         call handle_err(status,'qpmerge',45)
00597     enddo
00598     deallocate( result )
00599 !
00600 ! close merged output file
00601     status = nf90_close( outid )
00602     call handle_err(status,'qpmerge',46)
00603 
00604 end subroutine qp2merge
00605 !
00606 !========================================================
00607 subroutine qp3merge( num, filenames, out_path )
00608 !========================================================
00609 use netcdf
00610 use typeSizes
00611 implicit none
00612 
00613 ! parameters
00614 integer, intent(in) :: num                                   ! # processes
00615 character(len=256), dimension(num), intent(in) :: filenames  ! input file names
00616 character(len=256), intent(in) :: out_path                   ! output file name
00617 
00618 ! netcdf variables
00619 integer :: status                              ! return value of netcdf function
00620 integer, dimension(num) :: ncid                ! input file id#s
00621 integer :: outid                               ! output file id#
00622 integer :: unlimitid                           ! unlimited dimension id#
00623 integer :: numvars                             ! # variables in input files
00624 integer :: numatts                             ! # attributes in input files
00625 character(len=20) :: name                      ! variable name
00626 integer, dimension(:), allocatable :: dimid    ! dimension id#s
00627 integer, dimension(:), allocatable :: vardims  ! variable dimensions
00628 integer :: dimlen                              ! dimension length
00629 integer :: timelen                             ! dimension length
00630 integer :: landlen                             ! dimension length
00631 integer :: latlen                              ! dimension length
00632 integer :: lonlen                              ! dimension length
00633 integer :: levlen                              ! dimension length
00634 integer :: xtype                               ! variable data type
00635 integer :: numdims                             ! # dimensions in input files
00636 integer :: varid                               ! variable id#
00637 
00638 ! local variables
00639 character(len=256) :: outfile                  ! output file name
00640 double precision, dimension(:), allocatable :: dvalues         ! input data values
00641 character(len=10), dimension(:), allocatable :: cvalues        ! output data values
00642 real, dimension(:,:,:), allocatable :: values   ! input data values
00643 real, dimension(:,:,:), allocatable :: result   ! output data values
00644 integer, dimension(:), allocatable :: lonindex
00645 integer, dimension(:), allocatable :: latindex
00646 integer :: f, n, a, d                          ! index variables
00647 integer :: landpoints
00648 character(len=256) :: command
00649 
00650     ! create output file name and create file
00651     write( outfile, '(a,i1.1,a)' ) trim(out_path)//'.qp', 3, '.nc'
00652 
00653     ! copy file if only one process, then return out of subroutine
00654     if ( num == 1 ) then
00655         command = "cp "//trim(filenames(1))//" "//trim(outfile)
00656         call system( trim(command) )
00657         return
00658     endif
00659 
00660     ! open all input files
00661     do f = 1, num
00662         status = nf90_open( trim(filenames(f)), nf90_nowrite, ncid(f) )
00663     enddo
00664     status = nf90_inquire( ncid(1), nDimensions=numdims, nVariables=numvars,  &
00665         nAttributes=numatts, unlimitedDimId=unlimitid )
00666 
00667     
00668     status = nf90_create( trim(outfile), nf90_clobber, outid )
00669     ! copy global attributes and dimensions 
00670     !   over to output file
00671     do a = 1, numatts
00672         status = nf90_inq_attname( ncid(1), nf90_global, a, name )
00673         status = nf90_copy_att( ncid(1), nf90_global, trim(name),  &
00674             outid, nf90_global )
00675     enddo
00676     allocate( dimid(numdims) )
00677     do d = 1, numdims - 1
00678         if ( d /= unlimitid ) then
00679             status = nf90_inquire_dimension( ncid(1), d, name=name, len=dimlen )
00680             status = nf90_def_dim( outid, trim(name), dimlen, dimid(d) )
00681         else
00682             status = nf90_def_dim( outid, 'time', nf90_unlimited, dimid(d) )
00683         endif
00684     enddo
00685     landpoints = 0
00686     do f = 1, num
00687         status = nf90_inquire_dimension( ncid(f), numdims, name=name, len=dimlen )
00688         landpoints = landpoints + dimlen
00689     enddo
00690     status = nf90_def_dim( outid, trim(name), landpoints, dimid(numdims) )
00691     
00692     ! define variables in the files 
00693     allocate( vardims(numdims) )
00694     do n = 1, 8
00695         status = nf90_inquire_variable( ncid(1), n, name=name, xtype=xtype,  &
00696             ndims=numdims, dimids=vardims, natts=numatts )
00697         status = nf90_def_var( outid, trim(name), xtype,  &
00698             vardims(1:numdims), varid )
00699 
00700         ! copy attributes over
00701         do a = 1, numatts
00702             status = nf90_inq_attname( ncid(1), n, a, name=name )
00703             status = nf90_copy_att( ncid(1), n, trim(name), outid, n )
00704         enddo
00705     enddo
00706     status = nf90_inquire_variable( ncid(1), 9, name=name, xtype=xtype,  &
00707         natts=numatts )
00708     status = nf90_def_var( outid, trim(name), xtype, (/6/), varid )
00709     do a = 1, numatts
00710         status = nf90_inq_attname( ncid(1), 9, a, name=name )
00711         status = nf90_copy_att( ncid(1), 9, trim(name), outid, varid )
00712     enddo
00713     status = nf90_inquire_variable( ncid(1), 10, name=name, xtype=xtype,  &
00714         natts=numatts )
00715     status = nf90_def_var( outid, trim(name), xtype, (/6/), varid )
00716     do a = 1, numatts
00717         status = nf90_inq_attname( ncid(1), 10, a, name=name )
00718         status = nf90_copy_att( ncid(1), 10, trim(name), outid, varid )
00719     enddo
00720     do n = 11, numvars
00721         status = nf90_inquire_variable( ncid(1), n, name=name, xtype=xtype,  &
00722             natts=numatts )
00723         status = nf90_def_var( outid, trim(name), xtype, (/6,5,1/), varid )
00724         ! copy attributes over
00725         do a = 1, numatts
00726             status = nf90_inq_attname( ncid(1), n, a, name=name )
00727             status = nf90_copy_att( ncid(1), n, trim(name), outid, varid )
00728         enddo
00729     enddo
00730     
00731     status = nf90_enddef( outid )
00732     
00733     ! copy 1-D and 2-D variables over
00734     do n = 1, 8
00735         status = nf90_inquire_variable( ncid(1), n, xtype=xtype,  &
00736             ndims=numdims, dimids=vardims )
00737         if ( xtype == nf90_double ) then
00738             status = nf90_inquire_dimension( ncid(1), vardims(1), len=dimlen )
00739             allocate( dvalues(dimlen) )
00740             status = nf90_get_var( ncid(1), n, dvalues )
00741             status = nf90_put_var( outid, n, dvalues )
00742             deallocate( dvalues )
00743         else if ( xtype == nf90_char ) then
00744             status = nf90_inquire_dimension( ncid(1), vardims(2), len=dimlen )
00745             allocate( cvalues(dimlen) )
00746             status = nf90_get_var( ncid(1), n, cvalues )
00747             status = nf90_put_var( outid, n, cvalues )
00748             deallocate( cvalues )
00749         else 
00750             status = nf90_inquire_dimension( ncid(1), vardims(1), len=dimlen )
00751             allocate( values(dimlen,1,1) )
00752             status = nf90_get_var( ncid(1), n, values )
00753             status = nf90_put_var( outid, n, values )
00754             deallocate( values )
00755         endif
00756     enddo
00757     
00758     ! copy lonindex and latindex values over
00759     allocate( lonindex(landpoints) )
00760     allocate( latindex(landpoints) )
00761     a = 1
00762     do f = 1, num
00763         status = nf90_inquire_dimension( ncid(f), 6, len=landlen )
00764         status = nf90_get_var( ncid(f), 9, lonindex(a:a+landlen-1) )
00765         status = nf90_get_var( ncid(f), 10, latindex(a:a+landlen-1) )
00766         a = a + landlen
00767     enddo
00768     
00769     status = nf90_put_var( outid, 9, lonindex )
00770     status = nf90_put_var( outid, 10, latindex )
00771     deallocate( lonindex )
00772     deallocate( latindex )
00773     
00774     ! copy variables over to output file
00775     status = nf90_inquire_dimension( ncid(1), 1, len=timelen )
00776     status = nf90_inquire_dimension( ncid(1), 5, len=levlen )
00777     allocate( result(landpoints,levlen,timelen) )
00778     do n = 11, numvars
00779         a = 1
00780         do f = 1, num
00781             status = nf90_inquire_dimension( ncid(f), 6, len=landlen )
00782             status = nf90_get_var( ncid(f), n, result(a:a+landlen-1,:,:) )
00783             a = a + landlen
00784         enddo
00785         status = nf90_put_var( outid, n, result )
00786     enddo
00787     deallocate( result )
00788     deallocate( dimid )
00789     deallocate( vardims )
00790 
00791     status = nf90_close( outid )
00792 
00793 end subroutine qp3merge
00794 !
00795 !========================================================
00796 subroutine restartmerge( num, filenames, out_path )
00797 !========================================================
00798 use netcdf
00799 use typeSizes
00800 implicit none
00801 
00802 ! parameters
00803 integer, intent(in) :: num                                   ! # processes
00804 character(len=256), dimension(num), intent(in) :: filenames  ! input file names
00805 character(len=256), intent(in) :: out_path                   ! output directory
00806 
00807 ! netcdf variables
00808 integer :: status                              ! return value of netcdf function
00809 integer, dimension(num) :: ncid                ! input file id#s
00810 integer :: outid                               ! output file id#
00811 integer :: varid                               ! variable id#
00812 integer, dimension(:), allocatable :: dimid    ! dimension id#s
00813 integer :: dimlen                              ! dimension length
00814 integer :: numdims                             ! # dimensions in input files
00815 integer :: numvars                             ! # variables in input files
00816 integer, dimension(:), allocatable :: vardims  ! variable dimensions
00817 character(len=20) :: name                      ! variable name
00818 integer :: xtype                               ! variable data type
00819 
00820 ! local variables
00821 integer :: d,v,f,n                                  ! index variables
00822 character(len=256) :: outfile                       ! output file name
00823 integer, dimension(:), allocatable :: lengths       ! dimension lengths
00824 integer, dimension(:), allocatable :: intvalues     ! input data values
00825 integer, dimension(:), allocatable :: intresults    ! output data values
00826 real, dimension(:,:,:), allocatable :: realvalues     ! input data values
00827 real, dimension(:,:,:), allocatable :: realresults    ! output data values
00828 integer vartype  ! flag indicating type of variable
00829 character(len=256) :: command
00830 !
00831 ! create output file
00832     status = nf90_create( trim(out_path), nf90_clobber, outid )
00833     call handle_err(status,'restartmerge',1)
00834 !
00835 ! open all restart files to merge
00836     do f = 1, num
00837         status = nf90_open( trim(filenames(f)), nf90_nowrite, ncid(f) )
00838         call handle_err(status,'restartmerge',2)
00839     enddo
00840 !
00841 ! find out how many dimensions and variables from first file
00842     status = nf90_inquire( ncid(1), nDimensions=numdims, nVariables=numvars )
00843     call handle_err(status,'restartmerge',3)
00844 !
00845 ! copy dimensions over to output file
00846     allocate( dimid(numdims) )
00847     do d = 1, numdims
00848         status = nf90_inquire_dimension( ncid(1), d, name=name, len=dimlen )
00849         call handle_err(status,'restartmerge',4)
00850         status = nf90_def_dim( outid, trim(name), dimlen, dimid(d) )
00851         call handle_err(status,'restartmerge',5)
00852     enddo
00853 !
00854 ! define variables in merged output file 
00855     allocate( vardims(numdims) )
00856     allocate( lengths(numdims) )
00857     do n = 1, numvars
00858         status = nf90_inquire_variable( ncid(1), n, name=name, xtype=xtype,  &
00859             ndims=numdims, dimids=vardims )
00860         call handle_err(status,'restartmerge',6)
00861         status = nf90_def_var( outid, trim(name), xtype,  &
00862             vardims(1:numdims), varid )
00863         call handle_err(status,'restartmerge',7)
00864     enddo
00865 !
00866 ! stop defining variables
00867     status = nf90_enddef( outid )
00868 !
00869 ! copy non-variant values to output file
00870     allocate( intvalues(1) )
00871     allocate( realvalues(1,1,1) )
00872     status = nf90_get_var( ncid(1), 1, intvalues )
00873     call handle_err(status,'restartmerge',8)
00874     status = nf90_put_var( outid, 1, intvalues )
00875     call handle_err(status,'restartmerge',9)
00876     status = nf90_get_var( ncid(1), 2, intvalues )
00877     call handle_err(status,'restartmerge',10)
00878     status = nf90_put_var( outid, 2, intvalues )
00879     call handle_err(status,'restartmerge',11)
00880     status = nf90_get_var( ncid(1), 3, intvalues )
00881     call handle_err(status,'restartmerge',12)
00882     status = nf90_put_var( outid, 3, intvalues )
00883     call handle_err(status,'restartmerge',13)
00884     status = nf90_get_var( ncid(1), 4, realvalues )
00885     call handle_err(status,'restartmerge',14)
00886     status = nf90_put_var( outid, 4, realvalues )
00887     call handle_err(status,'restartmerge',15)
00888     status = nf90_get_var( ncid(1), 5, intvalues )
00889     call handle_err(status,'restartmerge',16)
00890     status = nf90_put_var( outid, 5, intvalues )
00891     call handle_err(status,'restartmerge',17)
00892     
00893 ! add up subcount and write to file
00894     allocate( intresults(1) )
00895     intresults = 0
00896     do f = 1, num
00897         status = nf90_get_var( ncid(f), 6, intvalues )
00898         call handle_err(status,'restartmerge',19)
00899         intresults = intresults + intvalues
00900     enddo
00901     status = nf90_put_var( outid, 6, intresults )
00902     call handle_err(status,'restartmerge',20)
00903 !
00904 ! add up all other variables and write out to file
00905     deallocate( intvalues )
00906     deallocate( realvalues )
00907     deallocate( intresults )
00908     do n = 7, numvars
00909         status = nf90_inquire_variable( ncid(1), n, ndims=numdims,  &
00910             dimids=vardims, xtype=xtype, name=name )
00911         call handle_err(status,'restartmerge',21)
00912 !
00913 ! determine type and dimension of variable
00914         vartype=0
00915         if(xtype == nf90_int) vartype=-1  ! 1D integer
00916         if(xtype == nf90_double) then
00917            if(numdims==1) vartype=1 ! 1-D double precision
00918            if(numdims==2) vartype=2 ! 2-D double precision
00919            if(numdims==3) vartype=3 ! 3-D double precision
00920         endif
00921         if(vartype==0) then
00922             print*, 'Error: no data type/dimension match in restart merge'
00923             stop
00924         endif
00925 !
00926 ! combine variables
00927         if ( vartype==-1 ) then ! 1D integer vector
00928             status = nf90_inquire_dimension( ncid(1), vardims(1), len=lengths(1) )
00929             call handle_err(status,'restartmerge',22)
00930             allocate( intvalues(lengths(1)) )
00931             allocate( intresults(lengths(1)) )
00932             intresults(:) = 0
00933             do f = 1, num
00934                 status = nf90_get_var( ncid(f), n, intvalues(:) )
00935                 call handle_err(status,'restartmerge',23)
00936                 intresults = intresults + intvalues
00937             enddo
00938             status = nf90_put_var( outid, n, intresults(:) )
00939             call handle_err(status,'restartmerge',24)
00940             deallocate( intvalues )
00941             deallocate( intresults )
00942         elseif ( vartype==1 ) then ! 1D double precision vector
00943             status = nf90_inquire_dimension( ncid(1), vardims(1), len=lengths(1) )
00944             call handle_err(status,'restartmerge',34)
00945             if ( numdims == 1 ) then
00946                 lengths(2) = 1
00947             else
00948                 status = nf90_inquire_dimension( ncid(1), vardims(2),  &
00949                     len=lengths(2) )
00950                 call handle_err(status,'restartmerge',35)
00951             endif
00952             allocate( realvalues(lengths(1),lengths(2),1) )
00953             allocate( realresults(lengths(1),lengths(2),1) )
00954             realresults(:,:,:) = 1.e36
00955             do f = 1, num
00956                 status = nf90_get_var( ncid(f), n, realvalues )
00957                 call handle_err(status,'restartmerge',36)
00958                 do v = 1, lengths(1)
00959                     if ( realvalues(v,1,1) /= 1.e36 )  &
00960                         realresults(v,:,1) = realvalues(v,:,1)
00961                 enddo
00962             enddo
00963             status = nf90_put_var( outid, n, realresults )
00964             call handle_err(status,'restartmerge',37)
00965             deallocate( realvalues )
00966             deallocate( realresults )
00967         else if ( vartype==2 ) then ! 2-D double precision vector
00968 
00969             status = nf90_inquire_dimension( ncid(1), vardims(1), len=lengths(1) )
00970             call handle_err(status,'restartmerge',25)
00971             status = nf90_inquire_dimension( ncid(1), vardims(2), len=lengths(2) )
00972             call handle_err(status,'restartmerge',26)
00973             allocate( realvalues(lengths(1), lengths(2), 1) )
00974             allocate( realresults(lengths(1), lengths(2), 1) )
00975             realresults(:,:,:) = 1.e36
00976             ! find out which dimension is the nsib dimension
00977             allocate( intvalues(1) )  
00978             status = nf90_get_var( ncid(1), 1, intvalues )  !intvalues = nsib 
00979             call handle_err(status,'restartmerge')          
00980             do f = 1, num
00981                 status = nf90_get_var( ncid(f), n, realvalues )
00982                 call handle_err(status,'restartmerge',27)
00983                 if (lengths(2)== intvalues(1)) then             
00984                     do v = 1, lengths(2)
00985                         if ( realvalues(1,v,1) /= 1.e36 )  &
00986                             realresults(:,v,1) = realvalues(:,v,1)
00987                     enddo
00988                 else  ! so lengths(1) == intvalues(1)
00989                     do v = 1, lengths(1)
00990                         if (  realvalues(v,1,1) /= 1.e36 ) &
00991                             realresults(v,:,1) = realvalues(v,:,1)
00992                     end do
00993                 end if                                                                      
00994             enddo
00995             status = nf90_put_var( outid, n, realresults )
00996             call handle_err(status,'restartmerge',28)
00997             deallocate( realvalues )
00998             deallocate( realresults )
00999         else if ( vartype==3 ) then ! 3-D double precision vector
01000             status = nf90_inquire_dimension( ncid(1), vardims(1), len=lengths(1) )
01001             call handle_err(status,'restartmerge',29)
01002             status = nf90_inquire_dimension( ncid(1), vardims(2), len=lengths(2) )
01003             call handle_err(status,'restartmerge',30)
01004             status = nf90_inquire_dimension( ncid(1), vardims(3), len=lengths(3) )
01005             call handle_err(status,'restartmerge',31)
01006             allocate( realvalues(lengths(1), lengths(2), lengths(3)) )
01007             allocate( realresults(lengths(1), lengths(2), lengths(3)) )
01008             realresults(:,:,:) = 1.e36
01009             do f = 1, num
01010                 status = nf90_get_var( ncid(f), n, realvalues )
01011                 call handle_err(status,'restartmerge',32)
01012                 do v = 1, lengths(2)
01013                     if ( realvalues(1,v,1) /= 1.e36 )  &
01014                         realresults(:,v,:) = realvalues(:,v,:)
01015                 enddo
01016             enddo
01017             status = nf90_put_var( outid, n, realresults )
01018             call handle_err(status,'restartmerge',33)
01019             deallocate( realvalues )
01020             deallocate( realresults )
01021         endif
01022     enddo
01023     
01024     ! close all files
01025     do f = 1, num
01026         status = nf90_close( ncid(f) )
01027         call handle_err(status,'restartmerge',38)
01028     enddo
01029     status = nf90_close( outid )
01030     call handle_err(status,'restartmerge',39)
01031 
01032     deallocate( dimid )
01033     deallocate( vardims )
01034     deallocate( lengths )
01035 
01036 end subroutine restartmerge
01037 !
01038 !========================================================
01039 subroutine pbpmerge( num, filenames, outfile )
01040 !========================================================
01041 use netcdf
01042 use typeSizes
01043 implicit none
01044 
01045 ! parameters
01046 integer, intent(in) :: num                                   ! # processes
01047 character(len=256), dimension(num), intent(in) :: filenames  ! input file names
01048 character(len=256), intent(in) :: outfile                    ! output directory
01049 
01050 ! netcdf variables
01051 integer :: status
01052 integer, dimension(num) :: ncid
01053 integer :: outid
01054 integer :: varid
01055 integer :: numatts
01056 integer :: numdims
01057 integer :: vdims
01058 integer :: npoints
01059 integer :: numvars
01060 integer :: unlimitid
01061 integer :: dimlen
01062 character(len=20) :: name
01063 integer, dimension(:), allocatable :: dimid
01064 integer, dimension(:), allocatable :: vardims
01065 integer :: dimcount
01066 integer :: xtype
01067 
01068 ! local variables
01069 integer :: a, d, f, e, n, v
01070 logical, dimension(num) :: exists
01071 integer, dimension(:), allocatable :: ivalues
01072 character(len=10), dimension(:), allocatable :: cvalues        ! output data values
01073 double precision, dimension(:), allocatable :: dvalues
01074 real, dimension(:,:,:), allocatable :: fvalues
01075 real, dimension(:,:,:), allocatable :: fresults
01076 character(len=256) :: command
01077 
01078     ! copy file if only one process, then return out of subroutine
01079     if ( num == 1 ) then
01080         command = "cp "//trim(filenames(1))//" "//trim(outfile)
01081         call system( trim(command) )
01082         return
01083     endif
01084 
01085     ! open existing pbp files
01086     e = 0
01087     do f = 1, num
01088         status = nf90_open( trim(filenames(f)), nf90_nowrite, ncid(f) )
01089         if ( status == 2 ) then
01090             exists(f) = .false.
01091         else
01092             exists(f) = .true.
01093             e = f
01094         endif
01095     enddo
01096 
01097     ! make sure there is at least one existing pbp file
01098     if ( e == 0 ) then
01099         print *, 'pbp files do not exists for this month'
01100         return
01101     endif
01102     
01103     ! find out information from input files
01104     status = nf90_inquire( ncid(e), nDimensions=numdims, nVariables=numvars,  &
01105         nAttributes=numatts, unlimitedDimId=unlimitid )
01106     
01107     ! create output file
01108     status = nf90_create( trim(outfile), nf90_clobber, outid )
01109 
01110     ! copy global attributes over to output file
01111     do a = 1, numatts
01112         status = nf90_inq_attname( ncid(e), nf90_global, a, name )
01113         status = nf90_copy_att( ncid(e), nf90_global, trim(name),  &
01114             outid, nf90_global )
01115     enddo
01116     
01117     ! copy dimensions over to output file
01118     allocate( dimid(numdims) )
01119     status = nf90_def_dim( outid, 'time', nf90_unlimited, dimid(1) )
01120     status = nf90_def_dim( outid, 'char_len', 10, dimid(2) )
01121     dimcount = 0
01122     do f = 1, num
01123         if ( exists(f) ) then
01124             status = nf90_inquire_dimension( ncid(f), 3, name=name,  &
01125                 len=dimlen )
01126             dimcount = dimcount + dimlen
01127         endif
01128     enddo
01129     status = nf90_def_dim( outid, trim(name), dimcount, dimid(3) )
01130     if ( numdims == 4 )  &
01131         status = nf90_def_dim( outid, 'level', 10, dimid(4) )
01132     
01133     ! define variables in the files 
01134     allocate( vardims(numdims) )
01135     do n = 1, numvars
01136 
01137         status = nf90_inquire_variable( ncid(e), n, name=name, xtype=xtype,  &
01138             ndims=vdims, dimids=vardims, natts=numatts )
01139         status = nf90_def_var( outid, trim(name), xtype,  &
01140             vardims(1:vdims), varid )
01141 
01142         ! copy attributes over
01143         do a = 1, numatts
01144             status = nf90_inq_attname( ncid(e), n, a, name=name )
01145             status = nf90_copy_att( ncid(e), n, trim(name), outid, n )
01146         enddo
01147     enddo
01148     
01149     status = nf90_enddef( outid )
01150     
01151     ! copy variables that don't depend on npoints
01152     status = nf90_inquire_dimension( ncid(e), 1, len=dimlen )
01153     allocate( dvalues(dimlen) )
01154     status = nf90_get_var( ncid(e), 1, dvalues )
01155     status = nf90_put_var( outid, 1, dvalues )
01156     deallocate( dvalues )
01157     allocate( cvalues(dimlen) )
01158     status = nf90_get_var( ncid(e), 2, cvalues )
01159     status = nf90_put_var( outid, 2, cvalues )
01160     deallocate( cvalues )
01161     if ( numdims == 4 ) then
01162         allocate( ivalues(10) )
01163         status = nf90_get_var( ncid(e), 6, ivalues )
01164         status = nf90_put_var( outid, 6, ivalues )
01165         deallocate( ivalues )
01166     endif
01167     
01168     ! write npoints out to file
01169     allocate( ivalues(dimcount) )
01170     do n = 1, dimcount
01171         ivalues(n) = n
01172     enddo
01173     status = nf90_put_var( outid, 3, ivalues )
01174     deallocate( ivalues )
01175     
01176     ! copy latitude and longitude variables
01177     allocate( fvalues(dimcount,1,1) )
01178     do v = 4, 5
01179         d = 1
01180         do f = 1, num
01181             if ( exists(f) ) then
01182                 status = nf90_inquire_dimension( ncid(f), 3, len=e )
01183                 status = nf90_get_var( ncid(f), v, fvalues(d:d+e-1,:,:) )
01184                 d = d + e
01185             endif
01186         enddo
01187         status = nf90_put_var( outid, v, fvalues )
01188     enddo
01189     deallocate( fvalues )
01190     
01191     ! copy the rest of the variables
01192     if ( numdims == 3 ) then
01193         e = 6
01194         allocate( fvalues(dimcount,dimlen,1) )
01195     else
01196         e = 7
01197         allocate( fvalues(dimcount,10,dimlen) )
01198     endif
01199     do v = e, numvars
01200         d = 1
01201         do f = 1, num
01202             if ( exists(f) ) then
01203                 status = nf90_inquire_dimension( ncid(f), 3, len=e )
01204                 status = nf90_get_var( ncid(f), v, fvalues(d:d+e-1,:,:) )
01205                 d = d + e
01206             endif
01207         enddo
01208         status = nf90_put_var( outid, v, fvalues )
01209     enddo
01210 
01211     status = nf90_close( outid )
01212 
01213     deallocate( fvalues )
01214     deallocate( vardims )
01215     deallocate( dimid )
01216 
01217 end subroutine pbpmerge
01218 !
01219 !========================================================
01220 subroutine respfmerge( nprocs, filenames, outfile )
01221 !========================================================
01222 ! merges binary respfactor files
01223 !
01224 ! Modifications:
01225 !  Kevin Schaefer added autotrophic respfactor
01226 !--------------------------------------------------------
01227 !
01228 implicit none
01229 !
01230 ! parameters
01231 integer, intent(in) :: nprocs
01232 character(len=256), dimension(nprocs), intent(in) :: filenames
01233 character(len=256), intent(in) :: outfile
01234 
01235 ! local variables
01236 integer :: nsib
01237 integer :: nsoil
01238 double precision, dimension(:,:), allocatable :: respfactor
01239 double precision, dimension(:,:), allocatable :: resptotal
01240 double precision, dimension(:), allocatable :: auto_respfactor
01241 double precision, dimension(:), allocatable :: auto_resptotal
01242 
01243 !itb_iso 
01244 double precision, dimension(:), allocatable :: d13c_het
01245 double precision, dimension(:), allocatable :: d13c_hettotal
01246 !itb_iso 
01247 
01248 integer :: p, i, j
01249 integer :: status
01250 character(len=256) :: command
01251 
01252     ! copy file if only one process, then return out of subroutine
01253     if ( nprocs == 1 ) then
01254         command = "cp "//trim(filenames(1))//" "//trim(outfile)
01255         call system( trim(command) )
01256         return
01257     endif
01258 !
01259 ! get number of sib points and soil layers
01260     open( unit=3, file=trim(filenames(1)), form='unformatted',  &
01261         iostat=status ) !jk
01262     read( unit=3, iostat=status ) nsib
01263     read( unit=3, iostat=status ) nsoil
01264     close( unit=3 )
01265 !
01266 ! allocate respfactor variables
01267     allocate( respfactor(nsib, nsoil) )
01268     allocate( resptotal(nsib,nsoil) )
01269     allocate( auto_respfactor(nsib) )
01270     allocate( auto_resptotal(nsib) )
01271 
01272 !itb_iso 
01273     allocate( d13c_het(nsib) )
01274     allocate( d13c_hettotal(nsib) )
01275 !itb_iso 
01276 !
01277 ! clear out totals
01278     resptotal(:,:) = 0.0
01279     auto_resptotal(:) = 0.0
01280 
01281 !itb_iso 
01282     d13c_hettotal(:) = 0.0
01283 !itb_iso 
01284 !
01285 ! open eacH file and overlay them
01286 ! each file is a complete set of nsib points
01287     do p = 1, nprocs
01288         open( unit=3, file=trim(filenames(p)), form='unformatted' ) !jk
01289         read( unit=3, iostat=status ) nsib
01290         read( unit=3, iostat=status ) nsoil
01291         read( unit=3, iostat=status ) respfactor(:,:)
01292         read( unit=3, iostat=status ) auto_respfactor(:)
01293 
01294 !itb_iso 
01295         read( unit=3, iostat=status ) d13c_het(:)
01296 !itb_iso 
01297 
01298         close( unit=3 )
01299         if ( status > 0 ) then
01300             stop 'Error reading respfactor'
01301         endif
01302         do j = 1, nsoil
01303             do i = 1, nsib
01304                 resptotal(i,j) = resptotal(i,j) + respfactor(i,j)
01305             enddo
01306         enddo
01307         do i = 1, nsib
01308            auto_resptotal(i) = auto_resptotal(i) + auto_respfactor(i)
01309 
01310 !itb_iso 
01311            d13c_hettotal(i) = d13c_hettotal(i) + d13c_het(i)
01312 !itb_iso 
01313 
01314         enddo
01315 
01316     enddo
01317 !
01318 ! write combined respfactor to new file
01319     open( unit=3, file=trim(outfile), form='unformatted' )
01320     write( unit=3 ) nsib
01321     write( unit=3 ) nsoil
01322     write( unit=3 ) resptotal(:,:)
01323     write( unit=3 ) auto_resptotal(:)
01324 !itb_iso 
01325     write( unit=3 ) d13c_hettotal(:)
01326 !itb_iso 
01327     close( unit=3 )
01328 !
01329 ! deallocate variables
01330     deallocate( respfactor )
01331     deallocate( resptotal )
01332     deallocate( auto_respfactor )
01333     deallocate( auto_resptotal )
01334 !itb_iso 
01335     deallocate( d13c_het )
01336     deallocate( d13c_hettotal )
01337 !itb_iso 
01338     
01339 end subroutine respfmerge
01340 !
01341 !========================================================
01342 subroutine new_respfmerge( nprocs, filenames, outfile )
01343 !========================================================
01344 ! merges netcdf respfactor files
01345 !
01346 ! Modifications:
01347 !  Kevin Schaefer created routine (5/17/05)
01348 !--------------------------------------------------------
01349 !
01350 use netcdf
01351 use typeSizes
01352 !
01353 implicit none
01354 !
01355 ! inputs
01356 integer, intent(in) :: nprocs                        ! number processors
01357 character(len=256), intent(in) :: filenames(nprocs)  ! input file names
01358 character(len=256), intent(in) :: outfile            ! output file name
01359 !
01360 ! netcdf variables
01361 integer status                      ! return value of netcdf function
01362 integer ncid_in(nprocs)             ! input file ids
01363 integer ncid_out                    ! output file id
01364 integer unlimitid                   ! unlimited dimension id
01365 integer numvars                     ! number variables in input files
01366 integer numatts                     ! number attributes in input files
01367 character(len=20) tname             ! temporary name
01368 integer, allocatable :: dimid(:)    ! dimension ids
01369 character(len=20), allocatable :: dim_name(:)    ! dimension names
01370 integer, allocatable :: dim_len(:)  ! dimension lengths
01371 integer, allocatable :: vardims(:)  ! variable dimensions
01372 integer dimlen                      ! dimension length
01373 integer timelen                     ! dimension length
01374 integer landlen                     ! dimension length
01375 integer latlen                      ! dimension length
01376 integer lonlen                      ! dimension length
01377 integer xtype                       ! variable data type
01378 integer numdims                     ! number dimensions in input files
01379 integer varid                       ! variable id
01380 integer did_time    ! dimension id - time
01381 integer did_nsib    ! dimension id - nsib
01382 integer did_nsoil   ! dimension id - nsoil
01383 integer did_lat     ! dimension id - latitude
01384 integer did_lon     ! dimension id - longitude
01385 integer did_subcount ! dimension id - subcount
01386 integer did_char    ! dimension id - character
01387 integer vid_time    ! variable id - time
01388 integer vid_start   ! variable id - start time
01389 integer vid_end     ! variable id - end time
01390 integer vid_period  ! variable id - period of time
01391 integer vid_lon     ! variable id - longitude
01392 integer vid_lat     ! variable id - latitude
01393 integer vid_lonindx ! variable id - longitude index
01394 integer vid_latindx ! variable id - latitude index
01395 integer vid_sibindx ! variable id - sib point index
01396 integer vid_subc    ! variable id - subcount
01397 integer vid_het     ! variable id - heterotrophic respfac
01398 integer vid_auto    ! variable id - autotrophic respfac
01399 integer vid_d13c    ! variable id - del 13C of heterotrophic resp
01400 integer vid_nsec    ! variable ID - time
01401 !
01402 ! local variables
01403 character(len=256) command          ! external command
01404 ! CSR cvalues and dvalues need to be allocatable
01405 double precision, dimension(:), allocatable :: dvalues         ! input data values
01406 character(len=10), dimension(:), allocatable :: cvalues        ! output data values
01407 real, allocatable :: values(:,:)    ! input data values
01408 real, allocatable :: result(:,:)    ! output data values
01409 integer, allocatable :: lonindex(:)
01410 integer, allocatable :: latindex(:)
01411 integer, allocatable :: sibindex(:)
01412 double precision, allocatable :: het_respfac(:,:)
01413 double precision, allocatable :: auto_respfac(:)
01414 
01415 !itb_iso 
01416 double precision, allocatable :: d13c_het(:)
01417 !itb_iso 
01418 
01419 integer i,j,k,l,m,n  ! indeces
01420 integer :: landpoints
01421 !
01422 ! open all input files
01423     do i = 1, nprocs
01424         status = nf90_open( trim(filenames(i)), nf90_nowrite, ncid_in(i) )
01425     enddo
01426 !
01427 ! create merged output file
01428     status = nf90_create( trim(outfile), nf90_clobber, ncid_out )
01429 !
01430 ! get dimensions and attributes
01431     status = nf90_inquire( ncid_in(1), nDimensions=numdims, nVariables=numvars,  &
01432         nAttributes=numatts, unlimitedDimId=unlimitid )
01433 !
01434 ! copy global attributes over to output file
01435     do i = 1, numatts
01436         status = nf90_inq_attname( ncid_in(1), nf90_global, i, tname )
01437         status = nf90_copy_att( ncid_in(1), nf90_global, trim(tname), ncid_out, nf90_global )
01438     enddo
01439 !
01440 ! transfer dimensions
01441     status=nf90_inq_dimid(ncid_in(1),'time',did_time)
01442     status = nf90_inquire_dimension( ncid_in(1), did_time, len=dimlen )
01443     status = nf90_def_dim( ncid_out, 'time', nf90_unlimited, did_time )
01444     
01445     status=nf90_inq_dimid(ncid_in(1),'nsib',did_nsib)
01446     status = nf90_inquire_dimension( ncid_in(1), did_nsib, len=dimlen )
01447     status = nf90_def_dim( ncid_out, 'nsib', dimlen, did_nsib )
01448     
01449     status=nf90_inq_dimid(ncid_in(1),'level',did_nsoil)
01450     status = nf90_inquire_dimension( ncid_in(1), did_nsoil, len=dimlen )
01451     status = nf90_def_dim( ncid_out, 'level', dimlen, did_nsoil )
01452     
01453     status=nf90_inq_dimid(ncid_in(1),'char_len',did_char)
01454     status = nf90_inquire_dimension( ncid_in(1), did_char, len=dimlen )
01455     status = nf90_def_dim( ncid_out, 'char_len', dimlen, did_char )
01456     
01457     status=nf90_inq_dimid(ncid_in(1),'latitude',did_lat)
01458     status = nf90_inquire_dimension( ncid_in(1), did_lat, len=dimlen )
01459     status = nf90_def_dim( ncid_out, 'latitude', dimlen, did_lat )
01460     
01461     status=nf90_inq_dimid(ncid_in(1),'longitude',did_lon)
01462     status = nf90_inquire_dimension( ncid_in(1), did_lon, len=dimlen )
01463     status = nf90_def_dim( ncid_out, 'longitude', dimlen, did_lon )
01464     
01465     landpoints = 0
01466     do i = 1, nprocs
01467         status=nf90_inq_dimid(ncid_in(i),'landpoints',did_subcount)
01468         status = nf90_inquire_dimension( ncid_in(i), did_subcount, len=dimlen )
01469         landpoints = landpoints + dimlen
01470     enddo
01471     status = nf90_def_dim( ncid_out, 'landpoints', landpoints, did_subcount )
01472 !
01473 ! define time
01474     status=nf90_inq_varid(ncid_in(1),'time',vid_time)
01475     status = nf90_inquire_variable( ncid_in(1), vid_time, natts=numatts )
01476     status = nf90_def_var( ncid_out, 'time', nf90_double, (/did_time/), vid_time )
01477     do i = 1, numatts
01478        status = nf90_inq_attname( ncid_in(1), vid_time, i, name=tname )
01479        status = nf90_copy_att( ncid_in(1), vid_time, trim(tname), ncid_out, vid_time )
01480     enddo
01481 !
01482 ! define start period
01483     status=nf90_inq_varid(ncid_in(1),'start_period',vid_start)
01484     status = nf90_inquire_variable( ncid_in(1), vid_start, natts=numatts )
01485     status = nf90_def_var( ncid_out, 'start_period', nf90_int, (/did_time/), vid_start )
01486     do i = 1, numatts
01487        status = nf90_inq_attname( ncid_in(1), vid_start, i, name=tname )
01488        status = nf90_copy_att( ncid_in(1), vid_start, trim(tname), ncid_out, vid_start )
01489     enddo
01490 !
01491 ! define end_period
01492     status=nf90_inq_varid(ncid_in(1),'end_period',vid_end)
01493     status = nf90_inquire_variable( ncid_in(1), vid_end, natts=numatts )
01494     status = nf90_def_var( ncid_out, 'end_period', nf90_int, (/did_time/), vid_end )
01495     do i = 1, numatts
01496        status = nf90_inq_attname( ncid_in(1), vid_end, i, name=tname )
01497        status = nf90_copy_att( ncid_in(1), vid_end, trim(tname), ncid_out, vid_end )
01498     enddo
01499 !
01500 ! define period length
01501     status=nf90_inq_varid(ncid_in(1),'period_length',vid_period)
01502     status = nf90_inquire_variable( ncid_in(1), vid_period, natts=numatts )
01503     status = nf90_def_var( ncid_out, 'period_length', nf90_double, (/did_time/), vid_period )
01504     do i = 1, numatts
01505        status = nf90_inq_attname( ncid_in(1), vid_period, i, name=tname )
01506        status = nf90_copy_att( ncid_in(1), vid_period, trim(tname), ncid_out, vid_period )
01507     enddo
01508 !
01509 ! define latitude
01510     status=nf90_inq_varid(ncid_in(1),'latitude',vid_lat)
01511     status = nf90_inquire_variable( ncid_in(1), vid_lat, natts=numatts )
01512     status = nf90_def_var( ncid_out, 'latitude', nf90_float, (/did_lat/), vid_lat )
01513     do i = 1, numatts
01514        status = nf90_inq_attname( ncid_in(1), vid_lat, i, name=tname )
01515        status = nf90_copy_att( ncid_in(1), vid_lat, trim(tname), ncid_out, vid_lat )
01516     enddo
01517 !
01518 ! define longitude
01519     status=nf90_inq_varid(ncid_in(1),'longitude',vid_lon)
01520     status = nf90_inquire_variable( ncid_in(1), vid_lon, natts=numatts )
01521     status = nf90_def_var( ncid_out, 'longitude', nf90_float, (/did_lon/), vid_lon )
01522     do i = 1, numatts
01523        status = nf90_inq_attname( ncid_in(1), vid_lon, i, name=tname )
01524        status = nf90_copy_att( ncid_in(1), vid_lon, trim(tname), ncid_out, vid_lon )
01525     enddo
01526 !
01527 ! define longitude index
01528     status=nf90_inq_varid(ncid_in(1),'lonindex',vid_lonindx)
01529     status = nf90_inquire_variable( ncid_in(1), vid_lonindx, natts=numatts )
01530     status = nf90_def_var( ncid_out, 'lonindex', nf90_int, (/did_subcount/), vid_lonindx )
01531     do i = 1, numatts
01532        status = nf90_inq_attname( ncid_in(1), vid_lonindx, i, name=tname )
01533        status = nf90_copy_att( ncid_in(1), vid_lonindx, trim(tname), ncid_out, vid_lonindx )
01534     enddo
01535 !
01536 ! define latitude index
01537     status=nf90_inq_varid(ncid_in(1),'latindex',vid_latindx)
01538     status = nf90_inquire_variable( ncid_in(1), vid_latindx, natts=numatts )
01539     status = nf90_def_var( ncid_out, 'latindex', nf90_int, (/did_subcount/), vid_latindx )
01540     do i = 1, numatts
01541        status = nf90_inq_attname( ncid_in(1), vid_latindx, i, name=tname )
01542        status = nf90_copy_att( ncid_in(1), vid_latindx, trim(tname), ncid_out, vid_latindx )
01543     enddo
01544 !
01545 ! define sib point index
01546     status=nf90_inq_varid(ncid_in(1),'sibindex',vid_sibindx)
01547     status = nf90_inquire_variable( ncid_in(1), vid_sibindx, natts=numatts )
01548     status = nf90_def_var( ncid_out, 'sibindex', nf90_int, (/did_subcount/), vid_sibindx )
01549     do i = 1, numatts
01550        status = nf90_inq_attname( ncid_in(1), vid_sibindx, i, name=tname )
01551        status = nf90_copy_att( ncid_in(1), vid_sibindx, trim(tname), ncid_out, vid_sibindx )
01552     enddo
01553 !
01554 ! define heterotrophic respfactor
01555     status=nf90_inq_varid(ncid_in(1),'het_respfac',vid_het)
01556     status = nf90_inquire_variable( ncid_in(1), vid_het, natts=numatts )
01557     status = nf90_def_var( ncid_out, 'het_respfac', nf90_double, (/did_subcount,did_nsoil/), vid_het)
01558     do i = 1, numatts
01559        status = nf90_inq_attname( ncid_in(1), vid_het, i, name=tname )
01560        status = nf90_copy_att( ncid_in(1), vid_het, trim(tname), ncid_out, vid_het )
01561     enddo
01562 !
01563 ! define autotrophic respfactor
01564     status=nf90_inq_varid(ncid_in(1),'auto_respfac',vid_auto)
01565     status = nf90_inquire_variable( ncid_in(1), vid_auto, natts=numatts )
01566     status = nf90_def_var( ncid_out, 'auto_respfac', nf90_double, (/did_subcount/), vid_auto)
01567     do i = 1, numatts
01568        status = nf90_inq_attname( ncid_in(1), vid_auto, i, name=tname )
01569        status = nf90_copy_att( ncid_in(1), vid_auto, trim(tname), ncid_out, vid_auto )
01570     enddo
01571 
01572 !itb_iso 
01573 !define del 13C of heterotrophic respiration
01574     status=nf90_inq_varid(ncid_in(1),'d13c_het',vid_d13c)
01575     status = nf90_inquire_variable( ncid_in(1), vid_d13c, natts=numatts )
01576     status = nf90_def_var( ncid_out, 'd13c_het', nf90_double, (/did_subcount/), vid_d13c)
01577     do i = 1, numatts
01578        status = nf90_inq_attname( ncid_in(1), vid_d13c, i, name=tname )
01579        status = nf90_copy_att( ncid_in(1), vid_d13c, trim(tname), ncid_out, vid_d13c )
01580     enddo
01581 !itb_iso 
01582 !
01583 ! stop defining variables
01584     status = nf90_enddef( ncid_out )
01585 !
01586 ! move values for file independent variables to output file
01587     allocate(Vardims(3))
01588     do n = 1, 6
01589         vardims=0
01590         status = nf90_inquire_variable( ncid_in(1), n, xtype=xtype,  &
01591             ndims=numdims, dimids=vardims )
01592         call handle_err(status,'rfmerge',1)
01593         if ( xtype == nf90_double ) then
01594             status = nf90_inquire_dimension( ncid_in(1), vardims(1), len=dimlen )
01595             allocate( dvalues(dimlen) )
01596             status = nf90_get_var( ncid_in(1), n, dvalues )
01597             status = nf90_put_var( ncid_out, n, dvalues )
01598             deallocate( dvalues )
01599         else if ( xtype == nf90_char ) then
01600             status = nf90_inquire_dimension( ncid_in(1), vardims(2), len=dimlen )
01601             allocate( cvalues(dimlen) )
01602             status = nf90_get_var( ncid_in(1), n, cvalues )
01603             status = nf90_put_var( ncid_out, n, cvalues )
01604             deallocate( cvalues )
01605         else 
01606             status = nf90_inquire_dimension( ncid_in(1), vardims(1), len=dimlen )
01607             allocate( values(dimlen,1) )
01608             status = nf90_get_var( ncid_in(1), n, values )
01609             status = nf90_put_var( ncid_out, n, values )
01610             deallocate( values )
01611         endif
01612     enddo
01613 !
01614 ! copy lonindex and latindex values over to output file
01615     allocate( lonindex(landpoints) )
01616     allocate( latindex(landpoints) )
01617     allocate( sibindex(landpoints) )
01618     status = nf90_inquire_dimension( ncid_in(1), did_nsoil, len=dimlen )
01619     allocate( het_respfac(landpoints,dimlen) )
01620     allocate( auto_respfac(landpoints) )
01621 
01622 !itb_iso 
01623     allocate( d13c_het(landpoints) )
01624 !itb_iso 
01625 
01626     n = 1
01627     do i = 1, nprocs
01628         status = nf90_inquire_dimension( ncid_in(i), 7, len=landlen )
01629         status = nf90_get_var( ncid_in(i), 7, lonindex(n:n+landlen-1) )
01630         status = nf90_get_var( ncid_in(i), 8, latindex(n:n+landlen-1) )
01631         status = nf90_get_var( ncid_in(i), 9, sibindex(n:n+landlen-1) )
01632         status = nf90_get_var( ncid_in(i), 10, het_respfac(n:n+landlen-1,:) )
01633         status = nf90_get_var( ncid_in(i), 11, auto_respfac(n:n+landlen-1) )
01634 !itb_iso 
01635         status = nf90_get_var( ncid_in(i), 12, d13c_het(n:n+landlen-1) )
01636 !itb_iso 
01637         n = n + landlen
01638     enddo
01639     
01640     status = nf90_put_var( ncid_out, 7, lonindex )
01641     status = nf90_put_var( ncid_out, 8, latindex )
01642     status = nf90_put_var( ncid_out, 9, sibindex )
01643     status = nf90_put_var( ncid_out, 10, het_respfac )
01644     status = nf90_put_var( ncid_out, 11, auto_respfac )
01645 !itb_iso 
01646     status = nf90_put_var( ncid_out, 12, d13c_het )
01647 !itb_iso 
01648     deallocate( lonindex )
01649     deallocate( latindex )
01650     deallocate( sibindex )
01651     deallocate( het_respfac )
01652     deallocate( auto_respfac )
01653 !itb_iso 
01654     deallocate( d13c_het )
01655 !itb_iso 
01656 
01657 !
01658 ! close merged output file
01659     status = nf90_close( ncid_out )
01660 !    
01661 end subroutine new_respfmerge
01662 !
01663 !========================================================
01664 subroutine handle_err( status, routine, number )
01665 !========================================================
01666 ! handle_err is a generic subroutine that checks for errors in the netcdf 
01667 ! functions and prints out a description of the error.  it then terminates 
01668 ! the program
01669 !
01670 use netcdf
01671 use typeSizes
01672 implicit none
01673 
01674 ! parameters
01675 integer, intent(in) :: status                       ! error status
01676 character(len=*), intent(in), optional :: routine   ! routine where err occurred
01677 integer, intent(in), optional :: number             ! error "line" number
01678                                                     ! used to pinpoint which line
01679                                                     ! caused the error
01680 if(status/=nf90_noerr) then
01681    print *, 'error', status, trim(nf90_strerror(status))
01682    if ( present(routine) ) print *, trim(routine)
01683    if ( present(number) ) print *, number
01684    stop 'stopped-netcdf error'
01685 endif
01686 
01687 end subroutine handle_err
01688 
01689 
01690 
01691 
01692 
01693 
01694 
01695 
01696 
01697 
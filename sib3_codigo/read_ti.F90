00001 !=============================================================
00002 subroutine read_ti (sib)
00003 !=============================================================
00004 ! Opens sib_bc_TI.nc and reads in the boundary condition data.
00005 !     REFERENCES:
00006 !     CREATED:
00007 !       Owen Leonard   August 3, 2001
00008 !     MODIFICATIONS:
00009 !   Owen Leonard  added code to read tables   10/15/01
00010 !   Owen Leonard  added check on zwind and ztemp  10/24/01
00011 !   Owen Leonard  modified code to be f90 freeform  4/25/02
00012 !   Owen Leonard  modified code to be compatible w/SiB3  4/25/02
00013 !     SUBROUTINES CALLED:
00014 !     FUNCTIONS CALLED:
00015 !       netcdf library
00016 !
00017 
00018 use kinds
00019 use sibtype
00020 use sib_const_module, only:  &
00021     zwind, &
00022     ztemp, &
00023     nsib,  &
00024     subcount, &
00025     subset
00026 use sib_io_module, only:  &
00027     param_path,  &
00028     biome_source,  &
00029     soil_source,  &
00030     soref_source
00031 use sib_bc_module
00032 #ifdef PGF
00033 use netcdf
00034 use typeSizes
00035 #endif
00036 
00037 
00038 ! declare input variables
00039 type(sib_t) :: sib(subcount)
00040 
00041 ! declare local variables
00042 character*100 filename  ! filename used to read in ndvi data
00043 integer(kind=int_kind) ::  i,q,j           !  index variables for loops
00044 integer(kind=int_kind) ::  start (2)      !  arrays that define where
00045 integer(kind=int_kind) ::  done (2)       !  to start and stop reading 
00046 integer(kind=int_kind) ::  begin (3)      !  variables from 
00047 integer(kind=int_kind) ::  finish (3)     !  sib_bc_TI.nc
00048 integer(kind=int_kind) ::  tiid           !  file id#
00049 integer(kind=int_kind) ::  status         !  error status check
00050 integer(kind=int_kind) ::  ndims          !  number of dimensions (soiltype)
00051 integer(kind=int_kind) ::  dimid, nvar    !  dimension id#, number of variables
00052 integer(kind=int_kind) ::  phys           !  phys dimension value
00053 !  used for AeroVar structure
00054 integer(kind=int_kind) ::  varid          ! varible ID
00055 integer(kind=int_kind) ::  numsoil,        ! soil type #s
00056     biovar, 
00057     svar,   
00058     morphvar
00059 integer(kind=int_kind) :: soilnum(nsib)
00060 real(kind=real_kind), dimension(nsib) :: biome
00061 real(kind=real_kind), dimension(nsib) :: clayfrac
00062 real(kind=real_kind), dimension(nsib) :: sandfrac
00063 real(kind=real_kind), dimension(nsib) :: vcover
00064 real(kind=real_kind), dimension(nsib) :: soref1
00065 real(kind=real_kind), dimension(nsib) :: soref2
00066 real(kind=real_kind) :: fclay
00067 real(kind=real_kind) :: fsand
00068 !itb_ncep_single
00069 integer(kind=int_kind) :: temp_1x1
00070 
00071 real(kind=real_kind), dimension(:,:), allocatable :: biovart3   ! 2D arrays for the tables
00072 real(kind=real_kind), dimension(:,:), allocatable :: biovart4   !   "   "
00073 real(kind=real_kind), dimension(:,:), allocatable :: soilvart   !   "   "
00074 real(kind=real_kind), dimension(:,:), allocatable :: morphvart !    "   "
00075 integer(kind=int_kind), dimension(:,:), allocatable :: phystype
00076 
00077 real(kind=dbl_kind) testzwind   !  used to test if zwind and ztemp in TI file
00078 real(kind=dbl_kind) testztemp   !  match the values defined in namel file
00079 character(len=10) name
00080 
00081 ! print message to screen
00082     print *, '\t reading sib_bc_TI.nc'
00083 
00084     ! open sib_bc_TI.nc file 
00085     filename=trim(param_path)//'TI.nc'
00086     print*, trim(filename)
00087     status = nf90_open ( trim(filename), nf90_nowrite, tiid )
00088     if (status /= nf90_noerr) call handle_err (status)
00089 
00090     ! check zwind and ztemp values
00091     status = nf90_inq_varid (tiid, 'zwind', varid)
00092     if (status /= nf90_noerr) call handle_err (status)
00093     status = nf90_get_var ( tiid, varid, testzwind )
00094     if (status /= nf90_noerr) call handle_err (status)
00095     status = nf90_inq_varid ( tiid, 'ztemp', varid )
00096     if (status /= nf90_noerr) call handle_err (status)
00097     status = nf90_get_var ( tiid, varid, testztemp )
00098     if ( status /= nf90_noerr ) call handle_err (status)
00099     if ( zwind /= testzwind ) then
00100         print *, '\t\t zwind value in ti file does not match namel file'
00101         print *, '\t\t ti:  ', testzwind, 'namel:  ', zwind
00102         !      stop
00103     endif
00104     if ( ztemp /= testztemp ) then
00105         print *, '\t\t ztemp value in ti file does not match namel file'
00106         print *, '\t\t ti:  ', testztemp, 'namel:  ', ztemp
00107         !      stop
00108     endif
00109 
00110     ! read in variables
00111     status = nf90_inq_dimid ( tiid, 'numvar', dimid )
00112     if (status /= nf90_noerr) call handle_err (status)
00113     status = nf90_inquire_dimension ( tiid, dimid, name,nvar )
00114     if (status /= nf90_noerr) call handle_err (status)
00115 
00116     ! biome
00117     status = nf90_inq_varid ( tiid, 'biome', varid )
00118     if (status /= nf90_noerr) call handle_err (status)
00119     status = nf90_get_var ( tiid, varid, biome )
00120     if (status /= nf90_noerr) call handle_err (status)
00121 
00122     ! soilnum
00123     status = nf90_inq_varid ( tiid, 'soilnum', varid )
00124     if (status /= nf90_noerr) call handle_err (status)
00125     status = nf90_inquire_variable ( tiid, varid, name,ndims )
00126     if (status /= nf90_noerr) call handle_err (status)
00127     start (1) = 1
00128     start (2) = 1
00129     done (1)  = nsib
00130     done (2)  = 1
00131     if (ndims.eq.1) then
00132         status = nf90_get_var ( tiid, varid, soilnum )
00133         if (status /= nf90_noerr) call handle_err (status)
00134     else 
00135         status = nf90_get_var ( tiid, varid, clayfrac, start, done )
00136         if (status /= nf90_noerr) call handle_err (status) 
00137         start (2) = 2
00138         status = nf90_get_var ( tiid, varid, sandfrac, start, done )
00139         if (status /= nf90_noerr) call handle_err (status)
00140     endif
00141 
00142     ! fvcover
00143     status = nf90_inq_varid ( tiid, 'fvcover', varid )
00144     if (status /= nf90_noerr) call handle_err (status)
00145     status = nf90_get_var ( tiid, varid, vcover )
00146     if (status /= nf90_noerr) call handle_err (status)
00147 
00148     ! phystype
00149     status = nf90_inq_dimid ( tiid, 'phys', dimid )
00150     if (status /= nf90_noerr) call handle_err (status)
00151     status = nf90_inquire_dimension ( tiid, dimid, name, phys )
00152     if (status /= nf90_noerr) call handle_err (status)
00153     allocate(phystype(nsib,phys))
00154     status = nf90_inq_varid ( tiid, 'phystype', varid )
00155     if (status /= nf90_noerr) call handle_err (status)
00156     status = nf90_get_var ( tiid, varid, phystype )
00157     if (status /= nf90_noerr) call handle_err (status)
00158 
00159     ! sorefvis
00160     status = nf90_inq_varid ( tiid, 'sorefvis', varid )
00161     if (status /= nf90_noerr) call handle_err (status)
00162     status = nf90_get_var ( tiid, varid, soref1 )
00163     if (status /= nf90_noerr) call handle_err (status)
00164 
00165     ! sorefnir
00166     status = nf90_inq_varid ( tiid, 'sorefnir', varid )
00167     if (status /= nf90_noerr) call handle_err (status)
00168     status = nf90_get_var ( tiid, varid, soref2 )
00169     if (status /= nf90_noerr) call handle_err (status)
00170 
00171 !itb_ncep_single...
00172     if(nsib == 1) then
00173       status = nf90_inq_varid ( tiid, 'pt_1x1', varid )
00174       if (status /= nf90_noerr) call handle_err (status)
00175       status = nf90_get_var ( tiid, varid, temp_1x1 )
00176       if (status /= nf90_noerr) call handle_err (status)
00177       sib(1)%param%pt_1x1 = temp_1x1
00178     endif
00179 !itb_ncep_single...
00180 
00181     ! laigrid
00182     status = nf90_inq_varid ( tiid, 'laigrid', varid )
00183     if (status /= nf90_noerr) call handle_err (status)
00184     status = nf90_get_var ( tiid, varid, laigrid )
00185     if (status /= nf90_noerr) call handle_err (status)
00186 
00187     ! fvcgrid
00188     status = nf90_inq_varid ( tiid, 'fvcgrid', varid )
00189     if (status /= nf90_noerr) call handle_err (status)
00190     status = nf90_get_var ( tiid, varid, fvcovergrid )
00191     if (status /= nf90_noerr) call handle_err (status)
00192 
00193     ! aero_zo
00194     allocate (aerovar(50,50,nvar))
00195     status = nf90_inq_varid ( tiid, 'aero_zo', varid )
00196     if (status /= nf90_noerr) call handle_err (status)
00197     status = nf90_get_var ( tiid, varid, aerovar%zo )
00198     if (status /= nf90_noerr) call handle_err (status)
00199 
00200     ! aero_zp
00201     status = nf90_inq_varid ( tiid, 'aero_zp', varid )
00202     if (status /= nf90_noerr) call handle_err (status)
00203     status = nf90_get_var ( tiid, varid, aerovar%zp_disp )
00204     if (status /= nf90_noerr) call handle_err (status)
00205 
00206     ! aero_rbc
00207     status = nf90_inq_varid ( tiid, 'aero_rbc', varid )
00208     if (status /= nf90_noerr) call handle_err (status)
00209     status = nf90_get_var ( tiid, varid, aerovar%rbc )
00210     if (status /= nf90_noerr) call handle_err (status)
00211 
00212     ! areo_rdc
00213     status = nf90_inq_varid ( tiid, 'aero_rdc', varid )
00214     if (status /= nf90_noerr) call handle_err (status)
00215     status = nf90_get_var ( tiid, varid, aerovar%rdc )
00216     if (status /= nf90_noerr) call handle_err (status)
00217 
00218     !---------------read in the tables----------------------------------
00219 
00220     status = nf90_inq_dimid ( tiid, 'numsoil', dimid )
00221     if (status /= nf90_noerr) call handle_err (status)
00222     status = nf90_inquire_dimension ( tiid, dimid, name, numsoil )
00223     if (status /= nf90_noerr) call handle_err (status)
00224     status = nf90_inq_dimid ( tiid, 'biovar', dimid )
00225     if (status /= nf90_noerr) call handle_err (status)
00226     status = nf90_inquire_dimension ( tiid, dimid, name,biovar )
00227     if (status /= nf90_noerr) call handle_err (status)
00228     status = nf90_inq_dimid ( tiid, 'soilvar', dimid )
00229     if (status /= nf90_noerr) call handle_err (status)
00230     status = nf90_inquire_dimension ( tiid, dimid, name,svar )
00231     if (status /= nf90_noerr) call handle_err (status)
00232     status = nf90_inq_dimid ( tiid, 'morphvar', dimid )
00233     if (status /= nf90_noerr) call handle_err (status)
00234     status = nf90_inquire_dimension ( tiid, dimid,name,morphvar )
00235     if (status /= nf90_noerr) call handle_err (status)
00236 
00237     allocate(morphtab(nvar))
00238     allocate(biovart3(nvar,biovar))
00239     allocate(biovart4(nvar,biovar))
00240     allocate(soilvart(numsoil,svar))
00241     allocate(morphvart(nvar,morphvar))
00242 
00243     status = nf90_inq_varid ( tiid, 'biome_tablec3', varid )
00244     if (status /= nf90_noerr) call handle_err (status)
00245     status = nf90_get_var ( tiid, varid, biovart3 )
00246     if (status /= nf90_noerr) call handle_err (status)
00247     status = nf90_inq_varid ( tiid, 'biome_tablec4', varid )
00248     if (status /= nf90_noerr) call handle_err (status)
00249     status = nf90_get_var ( tiid, varid, biovart4 )
00250     if (status /= nf90_noerr) call handle_err (status)
00251     status = nf90_inq_varid ( tiid, 'soil_table', varid )
00252     if (status /= nf90_noerr) call handle_err (status)
00253     status = nf90_get_var ( tiid, varid, soilvart )
00254     if (status /= nf90_noerr) call handle_err (status)
00255     status = nf90_inq_varid ( tiid, 'morph_table', varid )
00256     if (status /= nf90_noerr) call handle_err (status)
00257     status = nf90_get_var ( tiid, varid, morphvart )
00258     if (status /= nf90_noerr) call handle_err (status)
00259 
00260     ! read in global attributes that are passed on to output files
00261     status = nf90_get_att( tiid, nf90_global, 'biome_source', biome_source )
00262     status = nf90_get_att( tiid, nf90_global, 'soil_source', soil_source )
00263     status = nf90_get_att( tiid, nf90_global, 'soref_source', soref_source )
00264 
00265     status = nf90_close(tiid)
00266     
00267     !------------Assign values from tables into data structures--------
00268 
00269     do i = 1, subcount
00270 
00271 
00272         sib(i)%param%biome = biome(subset(i))
00273         sib(i)%param%soref(1) = soref1(subset(i))
00274         sib(i)%param%soref(2) = soref2(subset(i))
00275         sib(i)%param%vcover = vcover(subset(i))
00276         if ( ndims /= 1 ) then
00277             sib(i)%param%clayfrac = clayfrac(subset(i))
00278             sib(i)%param%sandfrac = sandfrac(subset(i))
00279         endif
00280         do j=1,phys  ! phystype loop
00281             sib(i)%param%phystype(j) = phystype(subset(i),j)
00282             if ( sib(i)%param%phystype(j) == 3 ) then
00283                 ! C3 table
00284                 sib(i)%param%z2 = biovart3(int(sib(i)%param%biome),1)
00285                 sib(i)%param%z1 = biovart3(int(sib(i)%param%biome),2)
00286                 sib(i)%param%chil = biovart3(int(sib(i)%param%biome),4)
00287                 sib(i)%param%phc = biovart3(int(sib(i)%param%biome),7)
00288                 sib(i)%param%tran(1,1) = biovart3(int(sib(i)%param%biome),8)
00289                 sib(i)%param%tran(2,1) = biovart3(int(sib(i)%param%biome),9)
00290                 sib(i)%param%tran(1,2) = biovart3(int(sib(i)%param%biome),10)
00291                 sib(i)%param%tran(2,2) = biovart3(int(sib(i)%param%biome),11)
00292                 sib(i)%param%ref(1,1) = biovart3(int(sib(i)%param%biome),12)
00293                 sib(i)%param%ref(2,1) = biovart3(int(sib(i)%param%biome),13)
00294                 sib(i)%param%ref(1,2) = biovart3(int(sib(i)%param%biome),14)
00295                 sib(i)%param%ref(2,2) = biovart3(int(sib(i)%param%biome),15)
00296                 sib(i)%param%vmax0(1) = biovart3(int(sib(i)%param%biome),16)
00297                 sib(i)%param%effcon(1) = biovart3(int(sib(i)%param%biome),17)
00298                 sib(i)%param%gradm(1) = biovart3(int(sib(i)%param%biome),18)
00299                 sib(i)%param%binter(1) = biovart3(int(sib(i)%param%biome),19)
00300                 sib(i)%param%atheta(1) = biovart3(int(sib(i)%param%biome),20)
00301                 sib(i)%param%btheta(1) = biovart3(int(sib(i)%param%biome),21)
00302                 sib(i)%param%trda(1) = biovart3(int(sib(i)%param%biome),22)
00303                 sib(i)%param%trdm(1) = biovart3(int(sib(i)%param%biome),23)
00304                 sib(i)%param%trop(1) = biovart3(int(sib(i)%param%biome),24)
00305                 sib(i)%param%respcp(1) = biovart3(int(sib(i)%param%biome),25)
00306                 sib(i)%param%slti(1) = biovart3(int(sib(i)%param%biome),26)
00307                 sib(i)%param%hltii(1) = biovart3(int(sib(i)%param%biome),27)
00308                 sib(i)%param%shti(1) = biovart3(int(sib(i)%param%biome),28)
00309                 sib(i)%param%hhti(1) = biovart3(int(sib(i)%param%biome),29)
00310 
00311             elseif ( sib(i)%param%phystype(j) == 4 ) then
00312 
00313                 ! C4 table
00314                 sib(i)%param%z2 = biovart4(int(sib(i)%param%biome),1)
00315                 sib(i)%param%z1 = biovart4(int(sib(i)%param%biome),2)
00316                 sib(i)%param%chil = biovart4(int(sib(i)%param%biome),4)
00317                 sib(i)%param%phc = biovart4(int(sib(i)%param%biome),7)
00318                 sib(i)%param%tran(1,1) = biovart4(int(sib(i)%param%biome),8)
00319                 sib(i)%param%tran(2,1) = biovart4(int(sib(i)%param%biome),9)
00320                 sib(i)%param%tran(1,2) = biovart4(int(sib(i)%param%biome),10)
00321                 sib(i)%param%tran(2,2) = biovart4(int(sib(i)%param%biome),11)
00322                 sib(i)%param%ref(1,1) = biovart4(int(sib(i)%param%biome),12)
00323                 sib(i)%param%ref(2,1) = biovart4(int(sib(i)%param%biome),13)
00324                 sib(i)%param%ref(1,2) = biovart4(int(sib(i)%param%biome),14)
00325                 sib(i)%param%ref(2,2) = biovart4(int(sib(i)%param%biome),15)
00326                 sib(i)%param%vmax0(2) = biovart4(int(sib(i)%param%biome),16)
00327                 sib(i)%param%effcon(2) = biovart4(int(sib(i)%param%biome),17)
00328                 sib(i)%param%gradm(2) = biovart4(int(sib(i)%param%biome),18)
00329                 sib(i)%param%binter(2) = biovart4(int(sib(i)%param%biome),19)
00330                 sib(i)%param%atheta(2) = biovart4(int(sib(i)%param%biome),20)
00331                 sib(i)%param%btheta(2) = biovart4(int(sib(i)%param%biome),21)
00332                 sib(i)%param%trda(2) = biovart4(int(sib(i)%param%biome),22)
00333                 sib(i)%param%trdm(2) = biovart4(int(sib(i)%param%biome),23)
00334                 sib(i)%param%trop(2) = biovart4(int(sib(i)%param%biome),24)
00335                 sib(i)%param%respcp(2) = biovart4(int(sib(i)%param%biome),25)
00336                 sib(i)%param%slti(2) = biovart4(int(sib(i)%param%biome),26)
00337                 sib(i)%param%hltii(2) = biovart4(int(sib(i)%param%biome),27)
00338                 sib(i)%param%shti(2) = biovart4(int(sib(i)%param%biome),28)
00339                 sib(i)%param%hhti(2) = biovart4(int(sib(i)%param%biome),29)
00340 
00341             elseif ( sib(i)%param%phystype(j) == 0 ) then
00342                 sib(i)%param%phystype(j) = 0
00343             else   ! not C3 or C4 case (CAM?)
00344                 stop'WE DO NOT HAVE PHYSIOLOGY OTHER THAN C3/C4 IN THE MODEL YET'
00345             endif
00346         enddo  ! physiology loop
00347 
00348     
00349         ! Soil Variables Table
00350         if (ndims == 1) then
00351             sib(i)%param%bee   = soilvart(int(soilnum(subset(i))),1)
00352             sib(i)%param%phsat = soilvart(int(soilnum(subset(i))),2)
00353             sib(i)%param%satco = soilvart(int(soilnum(subset(i))),3)
00354             sib(i)%param%poros = soilvart(int(soilnum(subset(i))),4)
00355             sib(i)%param%slope = soilvart(int(soilnum(subset(i))),5)
00356             sib(i)%param%wopt  = soilvart(int(soilnum(subset(i))),6)
00357             sib(i)%param%zm    = soilvart(int(soilnum(subset(i))),7)
00358             sib(i)%param%wsat  = soilvart(int(soilnum(subset(i))),8)
00359     
00360             ! calculate %sand and $clay from table based on the approximate
00361             !   centroid of soil texture category within the UDSA texture triangle
00362             !   centroid % clay/sand estimated by Kevin Schaefer  (3/30/01)
00363             select case (soilnum(subset(i)))
00364             case (1)
00365                 sib(i)%param%clayfrac = 3.
00366                 sib(i)%param%sandfrac = 92.
00367             case (2)
00368                 sib(i)%param%clayfrac = 5.
00369                 sib(i)%param%sandfrac = 82.
00370             case (3)
00371                 sib(i)%param%clayfrac = 10.
00372                 sib(i)%param%sandfrac = 65.
00373             case (4)
00374                 sib(i)%param%clayfrac = 13.
00375                 sib(i)%param%sandfrac = 22.
00376             case (5)
00377                 sib(i)%param%clayfrac = 7.
00378                 sib(i)%param%sandfrac = 7.
00379             case (6)
00380                 sib(i)%param%clayfrac = 18.
00381                 sib(i)%param%sandfrac = 42.
00382             case (7)
00383                 sib(i)%param%clayfrac = 28.
00384                 sib(i)%param%sandfrac = 58.
00385             case (8)
00386                 sib(i)%param%clayfrac = 40.
00387                 sib(i)%param%sandfrac = 52.
00388             case (9)
00389                 sib(i)%param%clayfrac = 39.
00390                 sib(i)%param%sandfrac = 32.
00391             case (10)
00392                 sib(i)%param%clayfrac = 39.
00393                 sib(i)%param%sandfrac = 10.
00394             case (11)
00395                 sib(i)%param%clayfrac = 41.
00396                 sib(i)%param%sandfrac = 7.
00397             case (12)
00398                 sib(i)%param%clayfrac = 65.
00399                 sib(i)%param%sandfrac = 19.
00400             case default
00401                 print *, 'Illegal value for soil type:  ', soilnum(subset(i))
00402                 print *, 'nsib point:  ', (subset(i))
00403                 sib(i)%param%clayfrac = 33.
00404                 sib(i)%param%sandfrac = 33.
00405             end select
00406 
00407         else  ! ndims
00408             ! calculate soil variables from %sand, %clay
00409             ! equations taken from SoilProperties subroutine in mapper.F
00410             ! resp. variable curve fits from Raich et al., 1991 
00411 
00412             ! patch!!!!!!!!!  %sand, %clay maps have bad data points
00413             ! values of 3.3961514E+38 :: patch changes them to 33 (neutral ground)
00414             ! approx. 50 bad data points
00415             if (sib(i)%param%clayfrac > 100)  sib(i)%param%clayfrac = 33.
00416             if (sib(i)%param%sandfrac > 100)  sib(i)%param%sandfrac = 33.
00417 
00418             fclay = sib(i)%param%clayfrac/100.
00419             fsand = sib(i)%param%sandfrac/100.
00420             sib(i)%param%bee = 2.91+0.159*sib(i)%param%clayfrac
00421             sib(i)%param%phsat = -10.*10**(1.88-0.0131*sib(i)%param%sandfrac)/ &
00422                 1000.
00423             sib(i)%param%satco = 0.0070556*10**(-0.884+0.0153* &
00424                 sib(i)%param%sandfrac)/1000.
00425             sib(i)%param%poros = 0.489-0.00126*sib(i)%param%sandfrac
00426             sib(i)%param%slope = 0.176
00427             sib(i)%param%wopt = (-0.08*fclay**2+0.22*fclay+0.59)*100.
00428             sib(i)%param%zm = -2*fclay**3-0.4491*fclay**2+0.2101*fclay+0.3478 
00429 
00430 !------------------------------------------------
00431 !itb_LBAMIP...hardwiring wopt to 75% for biome 1
00432 
00433             if(sib(i)%param%biome == 1.0) then
00434 
00435                sib(i)%param%woptzm = (75.0/100.0) ** sib(i)%param%zm
00436 
00437             else
00438 
00439                sib(i)%param%woptzm = (sib(i)%param%wopt/100.) ** sib(i)%param%zm
00440 
00441             endif
00442 
00443 !------------------------------------------------
00444 
00445 
00446 
00447 
00448 
00449 
00450 
00451             sib(i)%param%wsat = 0.25*fclay+0.5
00452         endif  ! ndims
00453 
00454     enddo  ! subcount loop
00455 
00456     ! Morph Table used in mapper
00457     do q = 1, nvar
00458         morphtab(q)%zc = morphvart (q,1)
00459         morphtab(q)%lwidth = morphvart (q,2)
00460         morphtab(q)%llength = morphvart (q,3)
00461         morphtab(q)%laimax = morphvart (q,4)
00462         morphtab(q)%stems = morphvart (q,5)
00463         morphtab(q)%ndvimax = morphvart (q,6)
00464         morphtab(q)%ndvimin = morphvart (q,7)
00465         morphtab(q)%srmax = morphvart (q,8)
00466         morphtab(q)%srmin = morphvart (q,9)
00467     enddo
00468 
00469     !---------deallocate 2D arrays---------------------------
00470 
00471     deallocate(biovart3)
00472     deallocate(biovart4)
00473     deallocate(soilvart)
00474     deallocate(morphvart)
00475     deallocate(phystype)
00476 
00477     print*,'done reading TI file'
00478 
00479 end subroutine read_ti
00480 
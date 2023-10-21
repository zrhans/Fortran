00001 
00002 
00003 
00004 
00005 !-------------------------------------------------------------------------------
00006 subroutine rtape_sib ( sib, time, rank )
00007 !-------------------------------------------------------------------------------
00008 ! This subroutine creates a netcdf restart file for all prognostic variables.
00009 !
00010 ! Modifications:
00011 !  Kevin Schaefer added calls to netcdf error handling routine (10/26/04)
00012 !-------------------------------------------------------------------------------
00013 
00014 use kinds
00015 #ifdef PGF
00016 use netcdf
00017 use typeSizes
00018 #endif
00019 use sibtype
00020 use timetype
00021 use sib_io_module
00022 use sib_const_module
00023 
00024 
00025 
00026 !jlc...parameters
00027 type(sib_t), dimension(subcount),intent(in) :: sib
00028 type(time_struct), intent(in) :: time
00029 integer(kind=int_kind), intent(in) :: rank
00030 
00031 !Bio...local variables
00032 integer(kind=int_kind) :: i,j,status,ncid,i1,i2
00033 integer(kind=int_kind) :: vdims(3)
00034 integer(kind=int_kind) :: start(2)
00035 integer(kind=int_kind) :: vcount(2)
00036 integer(kind=int_kind) :: nsibdid   ! dimension id - nsib
00037 integer(kind=int_kind) :: nsoildid  ! dimension id - nsoil
00038 integer(kind=int_kind) :: nsnowdid  ! dimension id - nsnow
00039 integer(kind=int_kind) :: nphysdid  ! dimension id - physiology types(6)
00040 integer(kind=int_kind) :: ntotdid   ! dimension id - soil + snow 
00041 integer(kind=int_kind) :: monthid   ! dimension id - number of months
00042 integer(kind=int_kind) :: nsibvid   ! variable id - nsib
00043 integer(kind=int_kind) :: nsoilvid  ! variable id - nsoil
00044 integer(kind=int_kind) :: nsnowvid  ! variable id - nsnow
00045 integer(kind=int_kind) :: vervid    ! variable id - version number
00046 integer(kind=int_kind) :: subcountid! variable id - subcount
00047 integer(kind=int_kind) :: tavid     ! variable id - ta (CAS temp)
00048 integer(kind=int_kind) :: tcvid     ! variable id - tcanopy
00049 integer(kind=int_kind) :: nslvid    ! variable id - # of snow layers
00050 integer(kind=int_kind) :: pco2avid  ! variable id - pco2a
00051 
00052 !itb_cos
00053 integer(kind=int_kind) :: pcosavid  ! variable id - pco2a
00054 
00055 !itb_iso 
00056 integer(kind=int_kind) :: d13cavid  ! variable id - d13c autotrophic resp 
00057 integer(kind=int_kind) :: totd13cid 
00058                                     ! variable id - d13c of heterotrophic resp
00059 !itb_iso 
00060 
00061 integer(kind=int_kind) :: d13ccaid  ! variable id - d13cca
00062 integer(kind=int_kind) :: svegvid   ! variable id - snow_veg
00063 integer(kind=int_kind) :: sagevid   ! variable id - snow_age
00064 integer(kind=int_kind) :: sdepthid  ! variable id - snow_depth
00065 integer(kind=int_kind) :: smassid   ! variable id - snow_mass
00066 integer(kind=int_kind) :: capac1vid ! variable id - capac1  
00067 integer(kind=int_kind) :: capac2vid ! variable id - capac2  
00068 integer(kind=int_kind) :: coszbarid ! variable id - coszbar
00069 integer(kind=int_kind) :: dayflagid ! variable id - dayflag
00070 integer(kind=int_kind) :: totanid   ! variable id - tot_an
00071 integer(kind=int_kind) :: totgppid  ! variable id - tot_gpp
00072 integer(kind=int_kind) :: totrcid   ! variable id - tot_rc
00073 integer(kind=int_kind) :: totfparid ! variable id - tot_fpar
00074 integer(kind=int_kind) :: tkevid    ! variable id - tke     
00075 integer(kind=int_kind) :: tdvid     ! variable id - td (soil/snow temp)
00076 integer(kind=int_kind) :: wwwlvid   ! variable id - www_liq 
00077 integer(kind=int_kind) :: wwwivid   ! variable id - www_ice 
00078 integer(kind=int_kind) :: dzsvid    ! variable id - dz (snow) 
00079 integer(kind=int_kind) :: lzsvid    ! variable id - layer_z (snow)
00080 integer(kind=int_kind) :: nzsvid    ! variable id - node_z (snow)
00081 integer(kind=int_kind) :: rstvid    ! variable id - rst  
00082 integer(kind=int_kind) :: nsecvid   ! variable id - nsecond 
00083 integer(kind=int_kind) :: nsectemp  ! temporary var to hold time%sec_year
00084 integer(kind=int_kind) :: shavid    ! variable id - sha
00085 integer(kind=int_kind) :: totssid   ! variable id - tot_ss
00086 
00087 character*10 curmon  !jlc
00088 
00089 !itb...
00090 character*14 curmon_temp 
00091 
00092 
00093 character*256 rmon  !jk
00094 
00095 integer(kind=int_kind), dimension(nsib) :: nsl
00096 
00097 real(kind=dbl_kind), dimension(nsib) ::  
00098     ta,         
00099     tc,         
00100     pco2ap,     
00101     d13cca,     
00102     snow_veg,   
00103     snow_age,   
00104     snow_depth, 
00105     snow_mass,  
00106     tke,        
00107     sha,        
00108     capac1,     
00109     capac2,     
00110     coszbar,    
00111     dayflag
00112 
00113 !itb_cos
00114 real(kind=dbl_kind), dimension(nsib) ::  
00115     pcosap
00116 
00117 !itb_iso 
00118 real(kind=dbl_kind), dimension(nsib,6) ::   
00119     d13c_auto
00120 real(kind=dbl_kind), dimension(12,nsib) :: tot_d13c
00121 !itb_iso 
00122     
00123 real(kind=dbl_kind), dimension(12,nsib) :: tot_an
00124 real(kind=dbl_kind), dimension(12,nsib) :: tot_gpp
00125 real(kind=dbl_kind), dimension(12,nsib) :: tot_rc
00126 real(kind=dbl_kind), dimension(12,nsib) :: tot_fpar
00127 real(kind=dbl_kind), dimension(12, nsib, nsoil) :: tot_ss
00128 
00129 real(kind=dbl_kind), dimension(nsib,6) :: rst
00130 
00131 real(kind=dbl_kind), dimension(nsib,-nsnow+1:nsoil) ::  
00132     deept,    ! ->sib%prog%deept
00133     www_liq,  ! ->sib%prog%www_liq
00134     www_ice    ! ->sib%prog%www_ice
00135 
00136 real(kind=dbl_kind), dimension(nsib,-nsnow+1:0) ::  
00137     dz_snow,    
00138     nz_snow
00139 
00140 real(kind=dbl_kind), dimension(nsib,-nsnow:0) :: lz_snow
00141 
00142     !jlc...copy data into temporary arrays initialized to 1.e36
00143     ta(:) = 1.e36
00144     tc(:) = 1.e36
00145     nsl(:) = 0
00146     pco2ap(:) = 1.e36
00147 
00148 !itb_cos
00149     pcosap(:) = 1.e36
00150 !itb_iso 
00151     d13c_auto(:,:) = 1.e36
00152     tot_d13c(:,:) = 1.e36
00153 !itb_iso 
00154 
00155     d13cca(:) = 1.e36
00156     snow_veg(:) = 1.e36
00157     snow_age(:) = 1.e36
00158     snow_depth(:) = 1.e36
00159     snow_mass(:) = 1.e36
00160     tke(:) = 1.e36
00161     sha(:) = 1.e36
00162     capac1(:) = 1.e36
00163     capac2(:) = 1.e36
00164     coszbar(:) = 1.e36
00165     tot_an(:,:) = 1.e36
00166     tot_gpp(:,:) = 1.e36
00167     tot_rc(:,:) = 1.e36
00168     tot_fpar(:,:) = 1.e36
00169     tot_ss(:,:,:) = 1.e36
00170     rst(:,:) = 1.e36
00171     deept(:,:) = 1.e36
00172     www_liq(:,:) = 1.e36
00173     www_ice(:,:) = 1.e36
00174     dz_snow(:,:) = 1.e36
00175     nz_snow(:,:) = 1.e36
00176     lz_snow(:,:) = 1.e36
00177     
00178     do i = 1, subcount
00179         ta(subset(i)) = sib(i)%prog%ta
00180         tc(subset(i)) = sib(i)%prog%tc
00181         nsl(subset(i)) = sib(i)%prog%nsl
00182         pco2ap(subset(i)) = sib(i)%prog%pco2ap
00183 
00184 !itb_cos
00185         pcosap(subset(i)) = sib(i)%prog%pcosap
00186 
00187         d13cca(subset(i)) = sib(i)%prog%d13cca
00188         snow_veg(subset(i)) = sib(i)%prog%snow_veg
00189         snow_age(subset(i)) = sib(i)%prog%snow_age
00190         snow_depth(subset(i)) = sib(i)%prog%snow_depth
00191         snow_mass(subset(i)) = sib(i)%prog%snow_mass
00192         tke(subset(i)) = sib(i)%prog%tke
00193         sha(subset(i)) = sib(i)%prog%sha
00194         capac1(subset(i)) = sib(i)%prog%capac(1)
00195         capac2(subset(i)) = sib(i)%prog%capac(2)
00196         coszbar(subset(i)) = sib(i)%stat%coszbar
00197         dayflag(subset(i)) = sib(i)%stat%dayflag
00198         tot_an(:,subset(i)) = sib(i)%param%tot_an(1:12)
00199         tot_gpp(:,subset(i)) = sib(i)%param%tot_gpp(1:12)
00200         tot_rc(:,subset(i)) = sib(i)%param%tot_rc(1:12)
00201         tot_fpar(:,subset(i)) = sib(i)%param%tot_fpar(1:12)
00202 !itb_iso
00203         tot_d13c(:,subset(i)) = sib(i)%param%tot_d13c(1:12)
00204 !itb_iso 
00205 
00206     enddo
00207 
00208     do j = 1, nsoil
00209         do i = 1, subcount
00210             tot_ss(:,subset(i),j) = sib(i)%param%tot_ss(1:12,j)
00211         enddo
00212     enddo
00213 
00214     do j=1,6
00215         do i=1,subcount
00216             rst(subset(i),j) = sib(i)%prog%rst(j)
00217         enddo
00218     enddo
00219 
00220     do j=-nsnow+1,nsoil
00221         do i=1,subcount
00222             deept(subset(i),j)   = sib(i)%prog%td(j)
00223             www_liq(subset(i),j) = sib(i)%prog%www_liq(j)
00224             www_ice(subset(i),j) = sib(i)%prog%www_ice(j)
00225             if (j <= 0) then
00226                 dz_snow(subset(i),j) = sib(i)%prog%dz(j)
00227                 nz_snow(subset(i),j) = sib(i)%prog%node_z(j)
00228             endif
00229         enddo
00230     enddo
00231     !jlc...is this a typo in sibtype? does layer_z have the same
00232     !      indicies as node_z and dz?
00233     !itb...layer_z has one more value, since it holds the edges
00234     !      of the layers
00235 
00236     do j=-nsnow,0
00237         do i=1,subcount
00238             lz_snow(subset(i),j) = sib(i)%prog%layer_z(j)
00239         enddo
00240     enddo
00241 
00242 !itb_iso 
00243 !itb... del 13C of photosynthesized material 
00244     do i= 1,subcount
00245       do j=1,6
00246         d13c_auto(subset(i),j) = sib(i)%param%d13c_auto(j)
00247       enddo     
00248     enddo
00249 !itb_iso 
00250 
00251     !-----------------------------------------------------------------
00252     ! This subroutine writes out a restart file for SiBDRV
00253     !-----------------------------------------------------------------
00254 
00255 
00256     !itb...open netcdf history (restart) file
00257     
00258     write(curmon, '(i4.4,i2.2,a,i3.3)') time%year, time%month, 'p', rank
00259 
00260 !itb...
00261 !    write(curmon_temp, '(i4.4,3i2.2,a,i3.3)') time%year, time%month, time%day, int(time%hour), 'p', rank
00262 
00263 !    rmon = trim(out_path)//"sib_r"//trim(curmon_temp)//".nc" !jk
00264     rmon = trim(out_path)//"sib_r"//trim(curmon)//".nc" !jk
00265 
00266 
00267     print*, 'write restart ', trim(rmon)
00268     status = nf90_create( rmon, nf90_clobber, ncid )
00269     if(status/=nf90_noerr) call handle_err(status,'rtape',1)
00270 
00271     !itb...dimensions...
00272     status = nf90_def_dim( ncid, 'nsib', nsib, nsibdid )
00273     if(status/=nf90_noerr) call handle_err(status,'rtape',2)
00274     status = nf90_def_dim( ncid, 'nsoil', nsoil, nsoildid )
00275     if(status/=nf90_noerr) call handle_err(status,'rtape',3)
00276     status = nf90_def_dim( ncid, 'nsnow', nsnow, nsnowdid )
00277     if(status/=nf90_noerr) call handle_err(status,'rtape',4)
00278     status = nf90_def_dim( ncid, 'nphys', 6, nphysdid )
00279     if(status/=nf90_noerr) call handle_err(status,'rtape',5)
00280     status = nf90_def_dim( ncid, 'ntot', nsoil+nsnow, ntotdid )
00281     if(status/=nf90_noerr) call handle_err(status,'rtape',6)
00282     status = nf90_def_dim( ncid, 'nmonths', 12, monthid )
00283     if(status/=nf90_noerr) call handle_err(status,'rtape',7)
00284 
00285 
00286     !itb... define scalar variables...
00287     vdims(1) = monthid
00288     vdims(2) = nsibdid
00289     vdims(3) = ntotdid
00290 
00291     status = nf90_def_var( ncid, 'nsib', nf90_int, nsibvid )
00292     if(status/=nf90_noerr) call handle_err(status,'rtape',8)
00293     status = nf90_def_var( ncid, 'nsoil', nf90_int, nsoilvid )
00294     if(status/=nf90_noerr) call handle_err(status,'rtape',9)
00295     status = nf90_def_var( ncid, 'nsnow', nf90_int, nsnowvid )
00296     if(status/=nf90_noerr) call handle_err(status,'rtape',10)
00297     status = nf90_def_var( ncid, 'version', nf90_float, vervid )
00298     if(status/=nf90_noerr) call handle_err(status,'rtape',11) 
00299 
00300 !itb...if nsecond = 86400 * 356, set nsecond = 0
00301     nsectemp = time%sec_year
00302     if(nsectemp == 31536000) nsectemp = 0
00303 
00304     status = nf90_def_var( ncid, 'nsecond', nf90_int, nsecvid )
00305     if(status/=nf90_noerr) call handle_err(status,'rtape',12)
00306     status = nf90_def_var( ncid, 'subcount', nf90_int, subcountid )
00307     if(status/=nf90_noerr) call handle_err(status,'rtape',13)
00308 
00309 
00310     !itb...define vector (length=nsib ) variables...
00311     status = nf90_def_var( ncid, 'ta', nf90_double, vdims(2), tavid )
00312     if(status/=nf90_noerr) call handle_err(status,'rtape',14)
00313     status = nf90_def_var( ncid, 'tc', nf90_double, vdims(2), tcvid )
00314     if(status/=nf90_noerr) call handle_err(status,'rtape',15)
00315     status = nf90_def_var( ncid, 'nsl', nf90_int, vdims(2), nslvid )
00316     if(status/=nf90_noerr) call handle_err(status,'rtape',16)
00317     status = nf90_def_var( ncid, 'pco2a', nf90_double, vdims(2), pco2avid )
00318     if(status/=nf90_noerr) call handle_err(status,'rtape',17)
00319 
00320 !itb_cos
00321     status = nf90_def_var( ncid, 'pcosa', nf90_double, vdims(2), pcosavid )
00322     if(status/=nf90_noerr) call handle_err(status,'rtape',175)
00323 
00324 !itb_iso 
00325     status = nf90_def_var( ncid, 'd13cca', nf90_double, vdims(2), d13ccaid )
00326     if(status/=nf90_noerr) call handle_err(status,'rtape',18)
00327 !itb_iso 
00328 
00329     status = nf90_def_var( ncid, 'snow_veg', nf90_double, vdims(2), svegvid )
00330     if(status/=nf90_noerr) call handle_err(status,'rtape',19)
00331     status = nf90_def_var( ncid, 'snow_age', nf90_double, vdims(2), sagevid )
00332     if(status/=nf90_noerr) call handle_err(status,'rtape',20)
00333     status = nf90_def_var( ncid, 'snow_depth', nf90_double, vdims(2), sdepthid )
00334     if(status/=nf90_noerr) call handle_err(status,'rtape',21)
00335     status = nf90_def_var( ncid, 'snow_mass', nf90_double, vdims(2), smassid )
00336     if(status/=nf90_noerr) call handle_err(status,'rtape',22)
00337     status = nf90_def_var( ncid, 'capac1', nf90_double, vdims(2), capac1vid )
00338     if(status/=nf90_noerr) call handle_err(status,'rtape',23)
00339     status = nf90_def_var( ncid, 'capac2', nf90_double, vdims(2), capac2vid )
00340     if(status/=nf90_noerr) call handle_err(status,'rtape',24)
00341     status = nf90_def_var( ncid, 'coszbar', nf90_double, vdims(2), coszbarid )
00342     if(status/=nf90_noerr) call handle_err(status,'rtape',25)
00343     status = nf90_def_var( ncid, 'dayflag', nf90_double, vdims(2), dayflagid )
00344     if(status/=nf90_noerr) call handle_err(status,'rtape',26)
00345     status = nf90_def_var( ncid, 'tke', nf90_double, vdims(2), tkevid )
00346     if(status/=nf90_noerr) call handle_err(status,'rtape',27)
00347     status = nf90_def_var( ncid, 'sha', nf90_double, vdims(2), shavid )
00348     if(status/=nf90_noerr) call handle_err(status,'rtape',28)
00349     status = nf90_def_var( ncid, 'tot_an', nf90_double, vdims(1:2), totanid )
00350     if(status/=nf90_noerr) call handle_err(status,'rtape',29)
00351     status = nf90_def_var( ncid, 'tot_gpp', nf90_double, vdims(1:2), totgppid )
00352     if(status/=nf90_noerr) call handle_err(status,'rtape',29)
00353     status = nf90_def_var( ncid, 'tot_rc', nf90_double, vdims(1:2), totrcid )
00354     if(status/=nf90_noerr) call handle_err(status,'rtape',29)
00355     status = nf90_def_var( ncid, 'tot_fpar', nf90_double, vdims(1:2), totfparid )
00356     if(status/=nf90_noerr) call handle_err(status,'rtape',29)
00357 
00358 !itb_iso
00359     status = nf90_def_var( ncid, 'tot_d13c', nf90_double, vdims(1:2), totd13cid )
00360     if(status/=nf90_noerr) call handle_err(status,'rtape',295)
00361 !itb_iso 
00362 
00363 
00364     !itb...define 2-D variables...
00365     status = nf90_def_var( ncid, 'td', nf90_double, vdims(2:3), tdvid )
00366     if(status/=nf90_noerr) call handle_err(status,'rtape',30)
00367     status = nf90_def_var( ncid, 'www_liq', nf90_double, vdims(2:3), wwwlvid )
00368     if(status/=nf90_noerr) call handle_err(status,'rtape',31)
00369     status = nf90_def_var( ncid, 'www_ice', nf90_double, vdims(2:3), wwwivid )
00370     if(status/=nf90_noerr) call handle_err(status,'rtape',32)
00371 
00372     vdims(3) = nsnowdid
00373     status = nf90_def_var( ncid, 'dzsnow', nf90_double, vdims(2:3), dzsvid )
00374     if(status/=nf90_noerr) call handle_err(status,'rtape',33)
00375     status = nf90_def_var( ncid, 'nzsnow', nf90_double, vdims(2:3), nzsvid )
00376     if(status/=nf90_noerr) call handle_err(status,'rtape',34)
00377     status = nf90_def_var( ncid, 'lzsnow', nf90_double, vdims(2:3), lzsvid )
00378     if(status/=nf90_noerr) call handle_err(status,'rtape',35)
00379 
00380     vdims(3) = nphysdid
00381     status = nf90_def_var( ncid, 'rst', nf90_double, vdims(2:3), rstvid )
00382     if(status/=nf90_noerr) call handle_err(status,'rtape',36)
00383 
00384 !itb_iso
00385     status = nf90_def_var( ncid, 'd13c_auto', nf90_double, vdims(2:3), d13cavid )
00386     if(status/=nf90_noerr) call handle_err(status, 'rtape', 365)
00387 !itb_iso 
00388 
00389     vdims(3) = nsoildid
00390     status = nf90_def_var( ncid, 'tot_ss', nf90_double, vdims, totssid )
00391     if(status/=nf90_noerr) call handle_err(status,'rtape',37)
00392 
00393 
00394 
00395     !itb...take file out of define mode, into data mode
00396     status = nf90_enddef( ncid )
00397     if(status/=nf90_noerr) call handle_err(status,'rtape',38)
00398 
00399     !itb...load the variables...
00400     status = nf90_put_var( ncid, nsibvid, nsib )
00401     if(status/=nf90_noerr) call handle_err(status,'rtape',39)
00402     status = nf90_put_var( ncid, nsoilvid, nsoil )
00403     if(status/=nf90_noerr) call handle_err(status,'rtape',40)
00404     status = nf90_put_var( ncid, nsnowvid, nsnow )
00405     if(status/=nf90_noerr) call handle_err(status,'rtape',41)
00406     status = nf90_put_var( ncid, nsecvid, nsectemp )
00407     if(status/=nf90_noerr) call handle_err(status,'rtape',42)
00408     status = nf90_put_var( ncid, vervid, version )
00409     if(status/=nf90_noerr) call handle_err(status,'rtape',43)
00410     status = nf90_put_var( ncid, subcountid, subcount )
00411     if(status/=nf90_noerr) call handle_err(status,'rtape',44)
00412 
00413     status = nf90_put_var( ncid, tavid, ta )
00414     if(status/=nf90_noerr) call handle_err(status,'rtape',45)
00415     status = nf90_put_var( ncid, tcvid, tc )
00416     if(status/=nf90_noerr) call handle_err(status,'rtape',46)
00417     status = nf90_put_var( ncid, nslvid, nsl )
00418     if(status/=nf90_noerr) call handle_err(status,'rtape',47)
00419     status = nf90_put_var( ncid, pco2avid, pco2ap )
00420     if(status/=nf90_noerr) call handle_err(status,'rtape',48)
00421 
00422 !itb_cos
00423     status = nf90_put_var( ncid, pcosavid, pcosap )
00424     if(status/=nf90_noerr) call handle_err(status,'rtape',485)
00425 
00426     status = nf90_put_var( ncid, d13ccaid, d13cca )
00427     if(status/=nf90_noerr) call handle_err(status,'rtape',49)
00428 
00429 !itb_iso
00430     status = nf90_put_var( ncid, d13cavid, d13c_auto )
00431     if(status/=nf90_noerr) call handle_err(status, 'rtape', 495)
00432 !itb_iso 
00433 
00434 
00435 
00436     status = nf90_put_var( ncid, svegvid, snow_veg )
00437     if(status/=nf90_noerr) call handle_err(status,'rtape',50)
00438     status = nf90_put_var( ncid, sagevid, snow_age )
00439     if(status/=nf90_noerr) call handle_err(status,'rtape',51)
00440     status = nf90_put_var( ncid, sdepthid, snow_depth )
00441     if(status/=nf90_noerr) call handle_err(status,'rtape',52)
00442     status = nf90_put_var( ncid, smassid, snow_mass )
00443     if(status/=nf90_noerr) call handle_err(status,'rtape',53)
00444     status = nf90_put_var( ncid, tkevid, tke )
00445     if(status/=nf90_noerr) call handle_err(status,'rtape',54)
00446     status = nf90_put_var( ncid, shavid, sha )
00447     if(status/=nf90_noerr) call handle_err(status,'rtape',55)
00448     status = nf90_put_var( ncid, coszbarid, coszbar )
00449     if(status/=nf90_noerr) call handle_err(status,'rtape',56)
00450     status = nf90_put_var( ncid, dayflagid, dayflag )
00451     if(status/=nf90_noerr) call handle_err(status,'rtape',57)
00452     status = nf90_put_var( ncid, totanid, tot_an )
00453     if(status/=nf90_noerr) call handle_err(status,'rtape',58)
00454     status = nf90_put_var( ncid, totgppid, tot_gpp )
00455     if(status/=nf90_noerr) call handle_err(status,'rtape',58)
00456     status = nf90_put_var( ncid, totrcid, tot_rc )
00457     if(status/=nf90_noerr) call handle_err(status,'rtape',58)
00458     status = nf90_put_var( ncid, totfparid, tot_fpar )
00459     if(status/=nf90_noerr) call handle_err(status,'rtape',58)
00460 
00461     status = nf90_put_var( ncid, totd13cid, tot_d13c )
00462     if(status/=nf90_noerr) call handle_err(status,'rtape',58)
00463 
00464     !jlc...these are the temporary arrays
00465     status = nf90_put_var( ncid, tdvid, deept )
00466     if(status/=nf90_noerr) call handle_err(status,'rtape',59)
00467     status = nf90_put_var( ncid, wwwlvid, www_liq )
00468     if(status/=nf90_noerr) call handle_err(status,'rtape',60)
00469     status = nf90_put_var( ncid, wwwivid, www_ice )
00470     if(status/=nf90_noerr) call handle_err(status,'rtape',61)
00471     status = nf90_put_var( ncid, rstvid, rst )
00472     if(status/=nf90_noerr) call handle_err(status,'rtape',62)
00473 
00474     !itb...slabs...
00475     status = nf90_put_var( ncid, capac1vid, capac1 )
00476     if(status/=nf90_noerr) call handle_err(status,'rtape',63)
00477     status = nf90_put_var( ncid, capac2vid, capac2 )
00478     if(status/=nf90_noerr) call handle_err(status,'rtape',64)
00479 
00480 
00481     !itb...netcdf does not deal well with negative indices: the 
00482     !itb...dz/node_z/layer_z arrays appear to go from (1:nsoil+nsnow)
00483     !itb...rather than (-nsnow+1:nsoil)
00484     start(1) = 1
00485     start(2) = 1
00486     vcount(1) = nsib
00487     vcount(2) = nsnow
00488 
00489 
00490     status = nf90_put_var( ncid, dzsvid, dz_snow, start, vcount )
00491     if(status/=nf90_noerr) call handle_err(status,'rtape',65)
00492     status = nf90_put_var( ncid, nzsvid, nz_snow, start, vcount )
00493     if(status/=nf90_noerr) call handle_err(status,'rtape',66)
00494     status = nf90_put_var( ncid, lzsvid, lz_snow, start,vcount )
00495     if(status/=nf90_noerr) call handle_err(status,'rtape',67)
00496 
00497 
00498     status = nf90_put_var( ncid, totssid, tot_ss )
00499     if(status/=nf90_noerr) call handle_err(status,'rtape',68)
00500 
00501     !itb...close the file
00502     status = nf90_close( ncid )
00503     if(status/=nf90_noerr) call handle_err(status,'rtape',69)
00504 
00505 end subroutine rtape_sib
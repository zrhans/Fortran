00001 !
00002 !===============================================================================
00003 subroutine respfactor_control( sib, time, rank )
00004 !===============================================================================
00005 ! Controls all calculations associated with the respfactors
00006 !
00007 ! Modifications:
00008 !  Kevin Schaefer added variables for autotrophic respfactor (5/6/05)
00009 !
00010 use kinds
00011 use timetype
00012 use sib_const_module
00013 use sib_io_module
00014 use sibtype
00015 !
00016 implicit none
00017 !
00018 ! parameters
00019 type(sib_t), dimension(subcount), intent(inout) :: sib
00020 type(time_struct), intent(in) :: time
00021 integer(kind=int_kind), intent(in) :: rank
00022 
00023 ! local variables
00024 integer(kind=int_kind) :: i, s, m
00025 real(kind=dbl_kind) test
00026 !
00027 ! add new values to sums of annual totals
00028    do i = 1, subcount
00029       sib(i)%param%tot_an(time%month) = sib(i)%param%tot_an(time%month) +  &
00030             sib(i)%diag%assimn(6) * time%dtsib
00031       sib(i)%param%tot_gpp(time%month) = sib(i)%param%tot_gpp(time%month) +  &
00032             sib(i)%diag%assim(6) * time%dtsib
00033       sib(i)%param%tot_rc(time%month) = sib(i)%param%tot_rc(time%month) +  &
00034             sib(i)%diag%resp_can(6) * time%dtsib
00035       sib(i)%param%tot_fpar(time%month) = sib(i)%param%tot_fpar(time%month) +  &
00036             sib(i)%param%aparc * time%dtsib
00037       sib(i)%param%tot_nee(time%month) = sib(i)%param%tot_nee(time%month) +  &
00038             (sib(i)%diag%resp_tot-sib(i)%diag%assim(6))* time%dtsib
00039       sib(i)%param%tot_het(time%month) = sib(i)%param%tot_het(time%month) +  &
00040             sib(i)%diag%resp_het * time%dtsib
00041       sib(i)%param%tot_auto(time%month) = sib(i)%param%tot_auto(time%month) +  &
00042             sib(i)%diag%resp_auto * time%dtsib
00043 
00044 !itb_iso 
00045       sib(i)%param%tot_d13c(time%month) =  sib(i)%param%tot_d13c(time%month) +   &
00046             sib(i)%diag%assimn(6) * time%dtsib * sib(i)%diag%d13cassimn(6)
00047 !itb_iso 
00048 
00049       Do s = 1, nsoil
00050          sib(i)%param%tot_ss(time%month,s) =  &
00051             sib(i)%param%tot_ss(time%month,s) +   &
00052             sib(i)%diag%soilscale(s) * time%dtsib
00053       enddo
00054    enddo
00055 !
00056 ! if new year, calculate new respfactors
00057    if ( time%calc_respf ) then
00058 !
00059 ! add up 12 monthly totals to get annual total
00060       do i = 1, subcount
00061 !
00062 ! clear out annual totals
00063          sib(i)%param%tot_an(13) = 0.0_dbl_kind
00064          sib(i)%param%tot_gpp(13) = 0.0_dbl_kind
00065          sib(i)%param%tot_rc(13) = 0.0_dbl_kind
00066          sib(i)%param%tot_fpar(13) = 0.0_dbl_kind
00067          sib(i)%param%tot_ss(13,:) = 0.0_dbl_kind
00068          sib(i)%param%tot_nee(13) = 0.0_dbl_kind
00069          sib(i)%param%tot_het(13) = 0.0_dbl_kind
00070          sib(i)%param%tot_auto(13) = 0.0_dbl_kind
00071 
00072 !itb_iso
00073          sib(i)%param%tot_d13c(13) = 0.0_dbl_kind
00074 !itb_iso 
00075 !
00076 ! add up monthly totals
00077          do m = 1, 12
00078             sib(i)%param%tot_an(13) = sib(i)%param%tot_an(13) +  &
00079                sib(i)%param%tot_an(m)
00080             sib(i)%param%tot_gpp(13) = sib(i)%param%tot_gpp(13) +  &
00081                sib(i)%param%tot_gpp(m)
00082             sib(i)%param%tot_rc(13) = sib(i)%param%tot_rc(13) +  &
00083                 sib(i)%param%tot_rc(m)
00084             sib(i)%param%tot_fpar(13) = sib(i)%param%tot_fpar(13) +  &
00085                 sib(i)%param%tot_fpar(m)
00086             sib(i)%param%tot_nee(13) = sib(i)%param%tot_nee(13) +  &
00087                 sib(i)%param%tot_nee(m)
00088             sib(i)%param%tot_het(13) = sib(i)%param%tot_het(13) +  &
00089                 sib(i)%param%tot_het(m)
00090             sib(i)%param%tot_auto(13) = sib(i)%param%tot_auto(13) +  &
00091                 sib(i)%param%tot_auto(m)
00092 
00093 !itb_iso
00094             sib(i)%param%tot_d13c(13) = sib(i)%param%tot_d13c(13) +   &
00095                 sib(i)%param%tot_d13c(m)
00096 !itb_iso 
00097 
00098 
00099             do s = 1, nsoil
00100                sib(i)%param%tot_ss(13,s) = sib(i)%param%tot_ss(13,s) +  &
00101                   sib(i)%param%tot_ss(m,s)
00102             enddo
00103          enddo
00104 
00105       enddo
00106 !
00107 ! test print messages
00108       i=1
00109       do m = 1, 12
00110          print*, m, sib(i)%param%tot_rc(m),sib(i)%param%tot_gpp(m)
00111       enddo
00112       do m = 1, 12
00113          print*, m, sib(i)%param%tot_gpp(m)-sib(i)%param%tot_rc(m)-sib(i)%param%tot_an(m)
00114       enddo
00115       print*, 'ann nee=',sib(i)%param%tot_nee(13)
00116       print*, 'test nee=',sib(i)%param%tot_het(13) + sib(i)%param%tot_auto(13) + sib(i)%param%tot_rc(13)- sib(i)%param%tot_gpp(13)
00117       print*, 'ann an=',sib(i)%param%tot_an(13)
00118       print*, 'ann gpp=',sib(i)%param%tot_gpp(13)
00119       print*, 'ann het=',sib(i)%param%tot_het(13)
00120       print*, 'ann aut=',sib(i)%param%tot_auto(13)
00121       print*, 'ann can=',sib(i)%param%tot_rc(13),sib(i)%param%tot_gpp(13)-sib(i)%param%tot_an(13)
00122       print*, 'ann fpar=',sib(i)%param%tot_fpar(13)
00123       print*, 'het ratio=',sib(i)%param%tot_het(13)/sib(i)%param%tot_gpp(13)
00124       print*, 'aut ratio=',(sib(i)%param%tot_auto(13) + sib(i)%param%tot_rc(13))/sib(i)%param%tot_gpp(13)
00125 !
00126 ! calculate new respfactors
00127       call calc_respfactor( sib )
00128 !
00129 ! write respfactors to file
00130       if ( time%write_respf ) then
00131         if(drvr_type=='single' .OR. drvr_type == 'ncp_sngl') then
00132           call write_single_respfactor(sib,time)
00133         else
00134           call write_global_respfactor ( sib, time, rank )
00135         endif
00136     endif
00137 !
00138 ! clear out totals
00139       if(roll_respf) then ! clear out next month's totals only
00140          m = mod( time%month, 12 ) + 1
00141          do i = 1, subcount
00142             sib(i)%param%tot_an(m) = 0.0_dbl_kind
00143             sib(i)%param%tot_gpp(m) = 0.0_dbl_kind
00144             sib(i)%param%tot_rc(m) = 0.0_dbl_kind
00145             sib(i)%param%tot_fpar(m) = 0.0_dbl_kind
00146             sib(i)%param%tot_nee(m) = 0.0_dbl_kind
00147             sib(i)%param%tot_het(m) = 0.0_dbl_kind
00148             sib(i)%param%tot_auto(m) = 0.0_dbl_kind
00149             sib(i)%param%tot_ss(m,:) = 0.0_dbl_kind
00150 !itb_iso 
00151             sib(i)%param%tot_d13c(m) = 0.0_dbl_kind
00152 !itb_iso 
00153          enddo
00154       else ! clear out all totals
00155          do i = 1, subcount
00156             sib(i)%param%tot_an(:) = 0.0_dbl_kind
00157             sib(i)%param%tot_gpp(:) = 0.0_dbl_kind
00158             sib(i)%param%tot_rc(:) = 0.0_dbl_kind
00159             sib(i)%param%tot_fpar(:) = 0.0_dbl_kind
00160             sib(i)%param%tot_nee(:) = 0.0_dbl_kind
00161             sib(i)%param%tot_het(:) = 0.0_dbl_kind
00162             sib(i)%param%tot_auto(:) = 0.0_dbl_kind
00163             sib(i)%param%tot_ss(:,:) = 0.0_dbl_kind
00164 !itb_iso 
00165             sib(i)%param%tot_d13c(:) = 0.0_dbl_kind
00166 !itb_iso 
00167          enddo
00168       endif
00169     endif
00170 !
00171 end subroutine respfactor_control
00172 !
00173 !===============================================================================
00174 subroutine calc_respfactor( sib )
00175 !===============================================================================
00176 !  calculate the annual respiration rate "respfactor" for each of 7
00177 !   soil layer at each grid cell in the model, given monthly mean
00178 !   maps of net carbon assimilation and "soilscale" at each level.
00179 
00180 !  references:
00181 !  denning et al., 1996a, tellus 48b, 521-542
00182 !  denning at al., 1996b, tellus 48b, 543-567 
00183 
00184 !  soilscale is the product of a temperature response and a moisture
00185 !   response function, evaluated in each soil layer. it is also called
00186 !   r* in denning et al (1996a), equations 6 through 9, and in
00187 !   denning et al (1996b), equations 8 and 9
00188 
00189 !  note: the soilscale qp3 from bugs has only 6 layers, so 
00190 !  *** before calling respire, you must "fill in" the bottom (first)
00191 !      layer of soilscale with zeros ****
00192 !
00193 ! Modifications:
00194 !  Kevin Schaefer added autotrophic respfactor (5/6/05)
00195 !------------------------------------------------------------------------------
00196 !
00197 use kinds
00198 use sibtype
00199 use sib_const_module
00200 !
00201 implicit none
00202 !
00203 ! parameters
00204 type(sib_t), dimension(subcount), intent(inout) :: sib
00205 !                      
00206 ! local variables
00207 integer :: n,l               ! looping indices
00208 real(kind=dbl_kind) xag       ! above ground biomass fraction (litter)
00209 real(kind=dbl_kind) xbg       ! below ground biomass fraction
00210 real(kind=dbl_kind) xagmin    ! min above-grnd biomass fraction (low GPP ecosystems)
00211 real(kind=dbl_kind) xagmax    ! max above-grnd biomass fraction (high GPP ecosystems)  
00212 real(kind=dbl_kind) anainflec ! tot_an at inflection point for xag function
00213 real(kind=dbl_kind) kxag      ! exponential constant for xag function    
00214 real(kind=dbl_kind) tot_het   ! total annual heterotrophic respiration
00215 real(kind=dbl_kind) tot_auto  ! total annual autotrophic respiration
00216 real(kind=dbl_kind) rcfrac    ! fraction of GPP to canopy autotrophic respiration
00217 !
00218 parameter(xagmin = 0.10, xagmax = 0.75, anainflec = 1000.,  &
00219     kxag=5.e-3)
00220 !
00221 ! loop through applicable grid points
00222     do n = 1, subcount
00223 !
00224 ! divide total annual GPP into heterotrophic and autotrophic respiration
00225         rcfrac=sib(n)%param%tot_rc(13)/sib(n)%param%tot_gpp(13)
00226         if(rcfrac<=autofrac) then
00227           tot_het=(1.-autofrac)*sib(n)%param%tot_gpp(13)
00228           tot_auto=autofrac*sib(n)%param%tot_gpp(13)-sib(n)%param%tot_rc(13)
00229         else
00230           tot_het=sib(n)%param%tot_gpp(13)-sib(n)%param%tot_rc(13)
00231           if(tot_het<0.) print*, n, sib(n)%param%tot_gpp(13)-sib(n)%param%tot_rc(13), sib(n)%param%tot_an(13)
00232           tot_auto=0.
00233         endif
00234 !
00235 ! split biomass into above ground (litter) and below ground
00236         ! above-ground biomass fraction
00237         xag = xagmin + (xagmax - xagmin) /  &
00238           ( 1.0 + exp( -kxag * ( tot_het * 12. - anainflec ) ) )
00239 
00240         ! below-ground biomass fraction
00241         xbg = 1.0 - xag
00242 !
00243 ! vertical distribution of biomass proportional to root distribution
00244         ! top two soil layers include litter
00245         sib(n)%param%het_respfac(1) = tot_het * ( 0.5 * xag  +  &
00246             xbg * sib(n)%param%rootf(1))
00247 
00248         sib(n)%param%het_respfac(2) = tot_het * ( 0.5 * xag  +  &
00249             xbg * sib(n)%param%rootf(2))
00250 
00251         ! rooting layers (3..10)
00252         do l = 3, nsoil
00253             sib(n)%param%het_respfac(l) = tot_het * xbg  &
00254                 * sib(n)%param%rootf(l)
00255         end do
00256 !
00257 ! heterotrophic respiration factor: divide by annual soilscale
00258         do l = 1, nsoil
00259             sib(n)%param%het_respfac(l) = sib(n)%param%het_respfac(l) /  &
00260                 sib(n)%param%tot_ss(13,l)
00261         end do ! (next layer)
00262 !
00263 ! autotrophic respiration factor
00264         sib(n)%param%auto_respfac=tot_auto/sib(n)%param%tot_fpar(13)
00265 
00266 !
00267 !itb_iso 
00268 ! del13C of heterotrophic respiration
00269         sib(n)%param%d13c_het = sib(n)%param%tot_d13c(13) /      &
00270                                         sib(n)%param%tot_an(13)
00271 !itb_iso 
00272  
00273     End do ! (next grid cell)
00274 
00275 end subroutine calc_respfactor
00276 !
00277 !==================================================================
00278 subroutine write_global_respfactor ( sib, time, rank )
00279 !==================================================================
00280 ! This subroutine creates a netcdf respfactor file.
00281 !
00282 ! Modifications:
00283 !  Kevin Schaefer created routine (5/16/05)
00284 !------------------------------------------------------------------
00285 !
00286 use kinds
00287 #ifdef PGF
00288 use netcdf
00289 use typeSizes
00290 #endif
00291 use sibtype
00292 use timetype
00293 use sib_io_module
00294 use sib_const_module
00295 !
00296 Implicit none
00297 !
00298 ! inputs
00299 type(sib_t), dimension(subcount), intent(in) :: sib  ! SiB variable tree
00300 type(time_struct), intent(in) :: time       ! time data
00301 integer(kind=int_kind), intent(in) :: rank  ! processor number
00302 !
00303 ! netcdf id variables
00304 integer(kind=int_kind) :: status      ! netcdf error number
00305 integer(kind=int_kind) :: ncid        ! netcdf file id number
00306 integer(kind=int_kind) :: did_time    ! dimension id - time
00307 integer(kind=int_kind) :: did_nsib    ! dimension id - nsib
00308 integer(kind=int_kind) :: did_nsoil   ! dimension id - nsoil
00309 integer(kind=int_kind) :: did_lat     ! dimension id - latitude
00310 integer(kind=int_kind) :: did_lon     ! dimension id - longitude
00311 integer(kind=int_kind) :: did_subcount ! dimension id - subcount
00312 integer(kind=int_kind) :: did_char    ! dimension id - character
00313 integer(kind=int_kind) :: vid_time    ! variable id - time
00314 integer(kind=int_kind) :: vid_start   ! variable id - start time
00315 integer(kind=int_kind) :: vid_end     ! variable id - end time
00316 integer(kind=int_kind) :: vid_period  ! variable id - period of time
00317 integer(kind=int_kind) :: vid_lon     ! variable id - longitude
00318 integer(kind=int_kind) :: vid_lat     ! variable id - latitude
00319 integer(kind=int_kind) :: vid_lonindx ! variable id - longitude index
00320 integer(kind=int_kind) :: vid_latindx ! variable id - latitude index
00321 integer(kind=int_kind) :: vid_sibindx ! variable id - sib point index
00322 integer(kind=int_kind) :: vid_subc    ! variable id - subcount
00323 integer(kind=int_kind) :: vid_het     ! variable id - heterotrophic respfac
00324 integer(kind=int_kind) :: vid_auto    ! variable id - autotrophic respfac
00325 integer(kind=int_kind) :: vid_nsec    ! variable ID - time
00326 !itb_iso 
00327 integer(kind=int_kind) :: vid_d13cr   ! variable ID - del13C of het respiration
00328 !itb_iso 
00329 !
00330 !
00331 ! local variables
00332 integer(kind=int_kind) :: i,j,k,l,m,n       ! indeces
00333 character*8 txt                         ! year and processor in character form
00334 character*256 filename                      ! filename
00335 integer(kind=int_kind) :: nsectemp          ! local time
00336 real(kind=dbl_kind) het_respfac(subcount,nsoil) ! local heterotrophic respfactor
00337 real(kind=dbl_kind) auto_respfac(subcount)      ! local autotrophic respfactor
00338 !
00339 
00340 !itb_iso
00341 real(kind=dbl_kind) d13c_het_resp(subcount)     ! local del13C or het resp
00342 !itb_iso 
00343 
00344 
00345 
00346 ! transfer data to local arrays 
00347     do i = 1, subcount
00348        auto_respfac(i) = sib(i)%param%auto_respfac
00349 
00350 !itb_iso 
00351        d13c_het_resp(i) = sib(i)%param%d13c_het
00352 !itb_iso 
00353 
00354        do j = 1, nsoil
00355           het_respfac(i,j) = sib(i)%param%het_respfac(j)
00356        enddo
00357     enddo
00358 !
00359 ! local time variable
00360     nsectemp = time%year
00361 !
00362 ! open respfactor file
00363     write(txt, '(i4.4,a,i3.3)') time%year, 'p', rank
00364     filename = trim(out_path)//"CO2_respf_"//trim(txt)//".nc" !jk
00365     print*, 'write global respfactor ', trim(filename)
00366     status = nf90_create( filename, nf90_clobber, ncid )
00367 !
00368 ! define global attributes
00369     call global_atts( ncid, 'sib3', 'lat/lon', '1.0', drvr_type,  &
00370         biome_source, soil_source, soref_source, ndvi_source, c4_source,  &
00371         d13cresp_source, rank )
00372 !
00373 ! define dimensions
00374     status = nf90_def_dim( ncid, 'time', nf90_unlimited, did_time )
00375     if(status/=nf90_noerr) call handle_err(status,'rf_write',1)
00376     status = nf90_def_dim( ncid, 'nsib', nsib, did_nsib )
00377     if(status/=nf90_noerr) call handle_err(status,'rf_write',2)
00378     status = nf90_def_dim( ncid, 'level', nsoil, did_nsoil )
00379     if(status/=nf90_noerr) call handle_err(status,'rf_write',3)
00380     status = nf90_def_dim( ncid, 'char_len', 10, did_char )
00381     if(status/=nf90_noerr) call handle_err(status,'rf_write',4)
00382     status = nf90_def_dim( ncid, 'latitude', jhr, did_lat )
00383     if(status/=nf90_noerr) call handle_err(status,'rf_write',5)
00384     status = nf90_def_dim( ncid, 'longitude', ihr, did_lon )
00385     if(status/=nf90_noerr) call handle_err(status,'rf_write',6)
00386     status = nf90_def_dim( ncid, 'landpoints', subcount, did_subcount )
00387     if(status/=nf90_noerr) call handle_err(status,'rf_write',7)
00388 !
00389 ! define latitude/longitude index variables
00390     status = nf90_def_var( ncid, 'time', nf90_double, (/did_time/), vid_time )
00391     if(status/=nf90_noerr) call handle_err(status,'rf_write',8)
00392     status = nf90_put_att( ncid, vid_time, 'quantity', 'time' )
00393     if(status/=nf90_noerr) call handle_err(status,'rf_write',9)
00394     status = nf90_put_att( ncid, vid_time, 'units', 'year' )
00395     if(status/=nf90_noerr) call handle_err(status,'rf_write',10)
00396     status = nf90_put_att( ncid, vid_time, 'calender', 'noleap' )
00397     if(status/=nf90_noerr) call handle_err(status,'rf_write',11)
00398     
00399     status = nf90_def_var( ncid, 'start_period', nf90_int, (/did_time/), vid_start )
00400     if(status/=nf90_noerr) call handle_err(status,'rf_write',12)
00401     status = nf90_put_att( ncid, vid_start, 'long_name', 'start of respfactor period' )
00402     if(status/=nf90_noerr) call handle_err(status,'rf_write',13)
00403     status = nf90_put_att( ncid, vid_start, 'units', 'day of year' )
00404     if(status/=nf90_noerr) call handle_err(status,'rf_write',14)
00405     
00406     status = nf90_def_var( ncid, 'end_period', nf90_int, (/did_time/), vid_end )
00407     if(status/=nf90_noerr) call handle_err(status,'rf_write',15)
00408     status = nf90_put_att( ncid, vid_end, 'long_name', 'end of respfactor period' )
00409     if(status/=nf90_noerr) call handle_err(status,'rf_write',16)
00410     status = nf90_put_att( ncid, vid_end, 'units', 'day of year' )
00411     if(status/=nf90_noerr) call handle_err(status,'rf_write',17)
00412     
00413     status = nf90_def_var( ncid, 'period_length', nf90_double, (/did_time/), vid_period )
00414     if(status/=nf90_noerr) call handle_err(status,'rf_write',18)
00415     status = nf90_put_att( ncid, vid_period, 'long_name', 'length of respfactor period' )
00416     if(status/=nf90_noerr) call handle_err(status,'rf_write',19)
00417     status = nf90_put_att( ncid, vid_period, 'units', 'days' )
00418     if(status/=nf90_noerr) call handle_err(status,'rf_write',20)
00419 
00420     status = nf90_def_var( ncid, 'latitude', nf90_float, (/did_lat/), vid_lat )
00421     if(status/=nf90_noerr) call handle_err(status,'rf_write',21)
00422     status = nf90_put_att( ncid, vid_lat, 'units', 'degrees_north' )
00423     if(status/=nf90_noerr) call handle_err(status,'rf_write',22)
00424     status = nf90_put_att( ncid, vid_lat, 'quantity', 'latitude' )
00425     if(status/=nf90_noerr) call handle_err(status,'rf_write',23)
00426     
00427     status = nf90_def_var( ncid, 'longitude', nf90_float, (/did_lon/), vid_lon )
00428     if(status/=nf90_noerr) call handle_err(status,'rf_write',24)
00429     status = nf90_put_att( ncid, vid_lon, 'units', 'degrees_east' )
00430     if(status/=nf90_noerr) call handle_err(status,'rf_write',25)
00431     status = nf90_put_att( ncid, vid_lon, 'quantity', 'longitude' )
00432     if(status/=nf90_noerr) call handle_err(status,'rf_write',26)
00433 
00434     status = nf90_def_var( ncid, 'lonindex', nf90_int, (/did_subcount/), vid_lonindx )
00435     if(status/=nf90_noerr) call handle_err(status,'rf_write',27)
00436     status = nf90_put_att( ncid, vid_lonindx, 'long_name', 'Longitude array index' )
00437     if(status/=nf90_noerr) call handle_err(status,'rf_write',28)
00438     status = nf90_put_att( ncid, vid_lonindx, 'units', 'index-integer' )
00439     if(status/=nf90_noerr) call handle_err(status,'rf_write',29)
00440 
00441     status = nf90_def_var( ncid, 'latindex', nf90_int, (/did_subcount/), vid_latindx )
00442     if(status/=nf90_noerr) call handle_err(status,'rf_write',30)
00443     status = nf90_put_att( ncid, vid_latindx, 'long_name', 'Latitude array index' )
00444     if(status/=nf90_noerr) call handle_err(status,'rf_write',31)
00445     status = nf90_put_att( ncid, vid_latindx, 'units', 'index-integer' )
00446     if(status/=nf90_noerr) call handle_err(status,'rf_write',32)
00447 
00448     status = nf90_def_var( ncid, 'sibindex', nf90_int, (/did_subcount/), vid_sibindx )
00449     if(status/=nf90_noerr) call handle_err(status,'rf_write',30)
00450     status = nf90_put_att( ncid, vid_sibindx, 'long_name', 'subset to nsib array index' )
00451     if(status/=nf90_noerr) call handle_err(status,'rf_write',31)
00452     status = nf90_put_att( ncid, vid_sibindx, 'units', 'index-integer' )
00453     if(status/=nf90_noerr) call handle_err(status,'rf_write',32)
00454 !
00455 ! define heterotrophic respiration factor variable
00456     status = nf90_def_var( ncid, 'het_respfac', nf90_double, (/did_subcount,did_nsoil/), vid_het)
00457     if(status/=nf90_noerr) call handle_err(status,'rf_write',33)
00458     status = nf90_put_att( ncid, vid_het, 'long_name', 'Heterotrophic Respiration Factor' )
00459     if(status/=nf90_noerr) call handle_err(status,'rf_write',34)
00460     status = nf90_put_att( ncid, vid_het, 'title', 'Heterotrophic Respiration Factor' )
00461     if(status/=nf90_noerr) call handle_err(status,'rf_write',35)
00462     status = nf90_put_att( ncid, vid_het, 'units', 'moles/m2/s' )
00463     if(status/=nf90_noerr) call handle_err(status,'rf_write',36)
00464     status = nf90_put_att( ncid, vid_het, 'missing_value', 1.e36 )
00465     if(status/=nf90_noerr) call handle_err(status,'rf_write',37)
00466 !
00467 ! define autotrophic respiration factor variable
00468     status = nf90_def_var( ncid, 'auto_respfac', nf90_double, (/did_subcount/), vid_auto)
00469     if(status/=nf90_noerr) call handle_err(status,'rf_write',38)
00470     status = nf90_put_att( ncid, vid_auto, 'long_name', 'Autotrophic Respiration Factor' )
00471     if(status/=nf90_noerr) call handle_err(status,'rf_write',39)
00472     status = nf90_put_att( ncid, vid_auto, 'title', 'Autotrophic Respiration Factor' )
00473     if(status/=nf90_noerr) call handle_err(status,'rf_write',40)
00474     status = nf90_put_att( ncid, vid_auto, 'units', 'moles/m2/s' )
00475     if(status/=nf90_noerr) call handle_err(status,'rf_write',41)
00476     status = nf90_put_att( ncid, vid_auto, 'missing_value', 1.e36 )
00477     if(status/=nf90_noerr) call handle_err(status,'rf_write',42)
00478 
00479 !
00480 !itb_iso 
00481 ! define del13C of autotrophic respiration factor variable
00482     status = nf90_def_var( ncid, 'del13c_resp_het', nf90_double,      &
00483                                 (/did_subcount/), vid_d13cr)
00484     if(status/=nf90_noerr) call handle_err(status,'rf_write',421)
00485     status = nf90_put_att( ncid, vid_d13cr, 'long_name',      &
00486                                 'del13C of Heterotrophic Respiration' )
00487     if(status/=nf90_noerr) call handle_err(status,'rf_write',422)
00488     status = nf90_put_att( ncid, vid_d13cr, 'title',      &
00489                                 'del13C of Heterotrophic Respiration' )
00490     if(status/=nf90_noerr) call handle_err(status,'rf_write',423)
00491     status = nf90_put_att( ncid, vid_d13cr, 'units', 'none' )
00492     if(status/=nf90_noerr) call handle_err(status,'rf_write',424)
00493     status = nf90_put_att( ncid, vid_d13cr, 'missing_value', 1.e36 )
00494     if(status/=nf90_noerr) call handle_err(status,'rf_write',425)
00495 !itb_iso 
00496 
00497 !
00498 ! end variable definition
00499     status = nf90_enddef( ncid )
00500     if(status/=nf90_noerr) call handle_err(status,'rf_write',43)
00501 !
00502 ! assign values to grid and time related variables
00503     status = nf90_put_var( ncid, vid_time, nsectemp )
00504     if(status/=nf90_noerr) call handle_err(status,'rf_write',44)
00505     status = nf90_put_var( ncid, vid_start, 1 )
00506     if(status/=nf90_noerr) call handle_err(status,'rf_write',45)
00507     status = nf90_put_var( ncid, vid_end, 365 )
00508     if(status/=nf90_noerr) call handle_err(status,'rf_write',46)
00509     status = nf90_put_var( ncid, vid_period, 365 )
00510     if(status/=nf90_noerr) call handle_err(status,'rf_write',47)
00511     status = nf90_put_var( ncid, vid_lat, latitude )
00512     if(status/=nf90_noerr) call handle_err(status,'rf_write',48)
00513     status = nf90_put_var( ncid, vid_lon, longitude )
00514     if(status/=nf90_noerr) call handle_err(status,'rf_write',49)
00515     status = nf90_put_var( ncid, vid_lonindx, sublon )
00516     if(status/=nf90_noerr) call handle_err(status,'rf_write',50)
00517     status = nf90_put_var( ncid, vid_latindx, sublat )
00518     if(status/=nf90_noerr) call handle_err(status,'rf_write',51)
00519     status = nf90_put_var( ncid, vid_sibindx, subset )
00520     if(status/=nf90_noerr) call handle_err(status,'rf_write',51)
00521 !
00522 ! load respfactor variables
00523     status = nf90_put_var( ncid, vid_het, het_respfac )
00524     if(status/=nf90_noerr) call handle_err(status,'rf_write',52)
00525     status = nf90_put_var( ncid, vid_auto, auto_respfac )
00526     if(status/=nf90_noerr) call handle_err(status,'rf_write',53)
00527 
00528 !itb_iso 
00529     status = nf90_put_var( ncid, vid_d13cr, d13c_het_resp )
00530     if(status/=nf90_noerr) call handle_err(status,'rf_write',531)
00531 !itb_iso 
00532 !
00533 ! close respfactor file
00534     status = nf90_close( ncid )
00535     if(status/=nf90_noerr) call handle_err(status,'rf_write',54)
00536 !
00537 end subroutine write_global_respfactor
00538 !
00539 !===============================================================================
00540 subroutine write_single_respfactor(sib,time)
00541 !===============================================================================
00542 ! writes respfactor to ascii file for single point
00543 !
00544 ! Modifications:
00545 !  Kevin Schaefer created routine (7/22/05)
00546 !-------------------------------------------------------------------------------
00547 !
00548 use kinds
00549 use sibtype
00550 use timetype
00551 use sib_const_module
00552 use sib_io_module
00553 !
00554 implicit none
00555 !
00556 ! input/output variables
00557 type(sib_t), dimension(subcount), intent(inout) :: sib
00558 type(time_struct), intent(in) :: time   ! time data
00559 !
00560 ! local variables
00561 integer(kind=int_kind) :: i,j, k, l,n,m ! indeces
00562 character*8 txt                         ! year and processor in character form
00563 character*256 filename                  ! filename
00564 
00565 logical good  ! flag indicating good data
00566 real(kind=dbl_kind), allocatable :: loc_data(:,:) ! temp var to read in data
00567 !
00568 ! Open SiB-CO2 respiration factor 
00569     write(txt, '(i4.4)') time%year
00570     filename = trim(out_path)//'CO2_respf_'//trim(txt) !jk
00571     open( unit=3, file=trim(filename), form='formatted')
00572 !
00573 ! print message to screen
00574     print*, '\t write single respfactor ', trim(filename)
00575 !
00576 ! write heterotrophic respfactor
00577     do i = 1,nsoil
00578       write(3,*) sib(1)%param%het_respfac(i),' het_respfac lev',i
00579     enddo
00580 !
00581 ! write autotrophic respfactor
00582     write(3,*) sib(1)%param%auto_respfac,' auto_respfac'
00583 
00584 !
00585 !itb_iso 
00586 ! write del13C of heterotrophic respiration
00587     write(3,*) sib(1)%param%d13c_het,' d13C_het_resp'
00588 !itb_iso 
00589 
00590 !
00591 ! close file
00592     close (3)
00593 !
00594 end subroutine write_single_respfactor
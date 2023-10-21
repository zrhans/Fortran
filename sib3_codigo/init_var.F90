00001 !---------------------------------------------------------------------
00002 subroutine init_var( sib)
00003 !---------------------------------------------------------------------
00004 
00005 !sets all variable, both global and local, initially to 0;
00006 !called from SiBDRV.f90 after sib has been allocated.
00007 
00008 use sibtype
00009 use timetype
00010 use sib_const_module
00011 use sib_io_module
00012 implicit none
00013 
00014 ! parameters
00015 type(sib_t), dimension(subcount), intent(inout) :: sib
00016 integer i
00017 integer j
00018 integer k
00019 
00020 !set all diagnostic variables to 0.0;
00021 
00022         sib%diag%eastar = 0.0
00023         sib%diag%rha = 0.0
00024         sib%diag%psy = 0.0
00025         sib%diag%cas_cap_heat = 0.0
00026         sib%diag%cas_cap_vap = 0.0
00027         sib%diag%cas_cap_co2 = 0.0
00028         sib%diag%cas_e_storage = 0.0
00029         sib%diag%cas_w_storage = 0.0
00030         sib%diag%canex  = 0.0
00031         sib%diag%wc = 0.0
00032         sib%diag%wg  = 0.0
00033         
00034         sib%diag%areas  = 0.0
00035         sib%diag%a_areas = 0.0
00036         sib%diag%snowmelt = 0.0
00037         sib%diag%www_tot_soil = 0.0
00038         sib%diag%roff = 0.0
00039         sib%diag%roffo  = 0.0
00040         sib%diag%qqq  = 0.0
00041         sib%diag%hr  = 0.0
00042         sib%diag%hrr  = 0.0
00043         sib%diag%resp_grnd = 0.0
00044         sib%diag%resp_tot = 0.0
00045         sib%diag%resp_het = 0.0
00046         sib%diag%resp_auto = 0.0
00047         sib%diag%www_inflow = 0.0
00048         sib%diag%cu = 0.0
00049         sib%diag%ct = 0.0
00050         sib%diag%ustar = 0.0
00051         sib%diag%ventmf  = 0.0
00052         sib%diag%thvgm = 0.0
00053         sib%diag%ecmass  = 0.0
00054         sib%diag%egmass = 0.0
00055         sib%diag%chf = 0.0
00056         sib%diag%shf = 0.0
00057         sib%diag%ra  = 0.0
00058         sib%diag%rb = 0.0
00059         sib%diag%rc     = 0.0
00060         sib%diag%rd = 0.0
00061         sib%diag%rsoil  = 0.0
00062         sib%diag%rds  = 0.0
00063         sib%diag%thermk  = 0.0
00064         sib%diag%tgeff = 0.0
00065         sib%diag%thgeff = 0.0
00066         sib%diag%shgeff  = 0.0
00067         sib%diag%p0  = 0.0
00068         sib%diag%pcpg_rain = 0.0
00069         sib%diag%pcpg_snow = 0.0
00070         sib%diag%cuprt = 0.0
00071         sib%diag%lsprt = 0.0
00072         sib%diag%hg  = 0.0
00073         sib%diag%hc   = 0.0
00074         sib%diag%hs  = 0.0
00075         sib%diag%fss = 0.0
00076         sib%diag%fws = 0.0
00077         sib%diag%ec  = 0.0
00078         sib%diag%eg = 0.0
00079         sib%diag%es  = 0.0
00080         sib%diag%egi   = 0.0
00081         sib%diag%eci  = 0.0
00082         sib%diag%egs  = 0.0
00083         sib%diag%ess = 0.0
00084         sib%diag%ect  = 0.0
00085         sib%diag%aparkk  = 0.0
00086         sib%diag%pfd     = 0.0
00087         sib%diag%cflux = 0.0
00088         sib%diag%flux13c = 0.0
00089         sib%diag%flux12c  = 0.0
00090         sib%diag%flux_turb  = 0.0
00091 
00092         do j=1,nsoil
00093                 
00094                 sib%diag%soilscale(j) = 0.0
00095                 sib%diag%soilq10(j) = 0.0
00096         enddo
00097 
00098         do i=1,6
00099                 sib%diag%assimnp(i) = 0.0
00100                 sib%diag%antemp(i) = 0.0
00101                 sib%diag%ansqr(i) = 0.0
00102                 sib%diag%omepot(i) = 0.0
00103                 sib%diag%assimpot(i) = 0.0
00104                 sib%diag%assimci(i) = 0.0
00105                 sib%diag%wsfws(i) = 0.0
00106                 sib%diag%wsfht(i) = 0.0
00107                 sib%diag%wsflt(i) = 0.0
00108                 sib%diag%wci(i) = 0.0
00109                 sib%diag%whs(i) = 0.0
00110                 sib%diag%wags(i) = 0.0
00111                 sib%diag%wegs(i) = 0.0
00112                 sib%diag%kiecps(i) = 0.0
00113                 sib%diag%d13cassimn(i) = 0.0
00114                 sib%diag%c13assimn(i) = 0.0
00115                 sib%diag%c12assimn(i) = 0.0
00116                 sib%diag%rcassimn(i) = 0.0
00117                 sib%diag%ggl(i) = 0.0
00118                 sib%diag%pco2i(i) = 0.0
00119                 sib%diag%pco2c(i) = 0.0
00120                 sib%diag%pco2s(i) = 0.0
00121                 sib%diag%resp_can(i) = 0.0
00122                 sib%diag%assim(i) = 0.0
00123                 sib%diag%assimn(i) = 0.0
00124 
00125 !itb_iso 
00126 !itb_d13c_psn
00127                 sib%param%d13c_psn(i)  = 0.0
00128                 sib%param%psn_accum(i) = 0.0
00129                 sib%param%d13c_auto(i) = 0.0
00130 !itb_iso 
00131 
00132 
00133         enddo
00134 
00135         do i=1,4
00136                 sib%diag%rstfac(i) = 0.0
00137         enddo
00138 
00139         do i=1,3
00140                 sib%diag%snow_end(i) = 0.0
00141                 sib%diag%radt(i) = 0.0
00142                 sib%diag%radtt(i) = 0.0
00143         enddo
00144 
00145         do i=1,2
00146                 sib%diag%drag(i) = 0.0
00147                 sib%diag%radc3(i) = 0.0
00148                 do j=1,2
00149                         sib%diag%salb(i,j) = 0.0
00150                         do k=1,2
00151                                 sib%diag%radfac(i,j,k) = 0.0
00152                         enddo
00153                 enddo
00154         enddo
00155 
00156    sib%diag%fac1 = 0.0
00157    sib%diag%closs = 0.0
00158    sib%diag%gloss = 0.0
00159    sib%diag%sloss = 0.0
00160    sib%diag%infil = 0.0
00161 
00162    sib%diag%capac_old(1)=0.
00163    sib%diag%capac_old(2)=0.
00164    sib%diag%snow_veg_old=0.
00165    sib%diag%snow_mass_old=0.
00166 
00167 
00168    sib%diag%abal=0.
00169    sib%diag%gbal=0.
00170    sib%diag%cbal=0.
00171    sib%diag%wbal=0.
00172    sib%diag%ebal=0.
00173 !
00174 ! set netcdf id numbers to zero
00175     param_id = 0
00176 !itb_modis
00177 !    ndvi_id  =0
00178 !    ndvi_time_id =0
00179     mlai_id = 0
00180     mfpar_id = 0
00181     modis_time_id = 0
00182 !itb_modis
00183 
00184 
00185     d13_id =0
00186     phys_id =0
00187 
00188 !------------------------------------------------------------------
00189 !                   BOUNDARY CONDITION VARIABLES
00190 !------------------------------------------------------------------
00191 
00192         sib%param%biome = 0.0
00193         sib%param%chil = 0.0
00194         sib%param%phc  = 0.0
00195         sib%param%z1  = 0.0
00196         sib%param%z2 = 0.0
00197         sib%param%poros = 0.0
00198         sib%param%satco  = 0.0
00199         sib%param%bee  = 0.0
00200         sib%param%phsat = 0.0
00201         sib%param%slope = 0.0
00202         sib%param%vcover  = 0.0
00203         sib%param%zm = 0.0
00204         sib%param%wopt  = 0.0
00205         sib%param%woptzm  = 0.0
00206         sib%param%wsat  = 0.0
00207         sib%param%sandfrac = 0.0
00208         sib%param%clayfrac = 0.0
00209         sib%param%vwcmin = 0.0
00210         sib%param%czc = 0.0
00211         sib%param%fieldcap  = 0.0
00212 !itb_modis
00213 !       sib%param%NDVI = 0.0
00214 !       sib%param%NDVI1 = 0.0
00215 !       sib%param%NDVI2 = 0.0
00216 !       sib%param%NDVI3 = 0.0
00217 !       sib%param%NDVI_time1 = 0.0
00218 !       sib%param%NDVI_time2 = 0.0
00219 !       sib%param%NDVI_time3 = 0.0
00220         sib%param%mlai = 0.0
00221         sib%param%mlai1 = 0.0
00222         sib%param%mlai2 = 0.0
00223         sib%param%mlai3 = 0.0
00224         sib%param%mfpar = 0.0
00225         sib%param%mfpar1 = 0.0
00226         sib%param%mfpar2 = 0.0
00227         sib%param%mfpar3 = 0.0
00228         sib%param%modis_time1 = 0.0
00229         sib%param%modis_time2 = 0.0
00230         sib%param%modis_time3 = 0.0
00231 !itb_modis
00232 
00233         sib%param%aparc = 0.0
00234         sib%param%aparc1 = 0.0
00235         sib%param%aparc2 = 0.0
00236         sib%param%zlt = 0.0
00237         sib%param%zlt1 = 0.0
00238         sib%param%zlt2  = 0.0
00239         sib%param%green  = 0.0
00240         sib%param%green1 = 0.0
00241         sib%param%green2 = 0.0
00242         sib%param%z0d  = 0.0
00243         sib%param%z0d1 = 0.0
00244         sib%param%z0d2  = 0.0
00245         sib%param%z0 = 0.0
00246         sib%param%z01 = 0.0
00247         sib%param%z02  = 0.0
00248         sib%param%zp_disp = 0.0
00249         sib%param%zp_disp1  = 0.0
00250         sib%param%zp_disp2 = 0.0
00251         sib%param%zpd_adj1 = 0.0
00252         sib%param%zpd_adj  = 0.0
00253         sib%param%zpd_adj2 = 0.0
00254         sib%param%cc1 = 0.0
00255         sib%param%cc2 = 0.0
00256         sib%param%rbc = 0.0
00257         sib%param%rbc1 = 0.0
00258         sib%param%rbc2 = 0.0
00259         sib%param%rdc = 0.0
00260         sib%param%rdc1 = 0.0
00261         sib%param%rdc2 = 0.0
00262         sib%param%gmudmu = 0.0
00263         sib%param%gmudmu1 = 0.0
00264         sib%param%gmudmu2 = 0.0
00265         sib%param%d13cresp = 0.0
00266         sib%param%d13cresp1 = 0.0
00267         sib%param%d13cresp2 = 0.0
00268         sib%param%d13cresp3 = 0.0
00269 
00270         do i=1,13
00271                 sib%param%tot_an(i) = 0.0
00272                 sib%param%tot_gpp(i) = 0.0
00273                 sib%param%tot_rc(i) = 0.0
00274                 sib%param%tot_fpar(i) = 0.0
00275                 sib%param%tot_nee(i) = 0.0
00276                 sib%param%tot_het(i) = 0.0
00277                 sib%param%tot_auto(i) = 0.0
00278                 do j=1,nsoil
00279                         sib%param%tot_ss(i,j) = 0.0
00280                 enddo
00281         enddo
00282 
00283         do i=1,2
00284                 sib%param%satcap(i) = 0.0
00285                 sib%param%soref(i) = 0.0
00286                 do j=1,2
00287                         sib%param%tran(i,j) = 0.0
00288                         sib%param%ref(i,j)  = 0.0
00289                 enddo
00290         enddo
00291         
00292         do i=1,5
00293                 sib%param%vmax0(i)  = 0.0
00294                 sib%param%trop(i) = 0.0
00295                 sib%param%trda(i) = 0.0
00296                 sib%param%trdm(i) = 0.0
00297                 sib%param%respcp(i) = 0.0
00298                 sib%param%slti(i) = 0.0
00299                 sib%param%shti(i) = 0.0
00300                 sib%param%hltii(i) = 0.0
00301                 sib%param%hhti(i)  = 0.0
00302                 sib%param%effcon(i) = 0.0
00303                 sib%param%binter(i) = 0.0
00304                 sib%param%gradm(i) = 0.0
00305                 sib%param%atheta(i) = 0.0
00306                 sib%param%btheta(i) = 0.0
00307                 sib%param%physfrac(i) = 0.0
00308                 sib%param%physfrac1(i) = 0.0
00309                 sib%param%physfrac2(i) = 0.0 
00310                 sib%param%physfrac3(i) = 0.0 
00311                 sib%param%phystype(i) = 0.0
00312 
00313 !itb_frost
00314         sib%param%sfti(i) = 0.6
00315         sib%param%hfti(i) = 269.15
00316 !itb_frost
00317 
00318         enddo
00319 
00320 !itb_frost
00321     sib%param%tcmin = 273.15
00322 !itb_frost
00323 
00324         do i=1,nsoil
00325                 sib%param%rootf(i) = 0.0
00326                 sib%param%rootr(i) = 0.0
00327                 sib%param%het_respfac(i) = 0.0
00328                 sib%param%tkmg(i) = 0.0
00329                 sib%param%tksatu(i) = 0.0
00330                 sib%param%tkdry(i) = 0.0
00331                 sib%param%csolid(i) = 0.0
00332         enddo
00333         
00334                 !sib%param%tksoil(-nsnow+1:nsoil) = 0.0
00335                 !sib%param%slamda(-nsnow+1:nsoil) = 0.0
00336                 !sib%param%shcap(-nsnow+1:nsoil) = 0.0
00337 
00338 !itb...location info
00339      sib%param%pt_1x1 = 0
00340 
00341 
00342 
00343 !------------------------------------------------------------------
00344 !                   PROGNOSTIC VARIABLES
00345 !------------------------------------------------------------------
00346         sib%prog%ta = 0.0
00347         sib%prog%tc = 0.0
00348         sib%prog%tha = 0.0
00349         sib%prog%sha = 0.0
00350         sib%prog%ea = 0.0
00351         sib%prog%snow_veg = 0.0
00352         sib%prog%tke = 0.0
00353         sib%prog%snow_mass  = 0.0
00354         sib%prog%snow_depth  = 0.0
00355         sib%prog%snow_age = 0.0
00356         sib%prog%nsl   = 0.0
00357         sib%prog%pco2ap = 0.0
00358         sib%prog%pco2ap_old = 0.0
00359         sib%prog%cas = 0.0
00360         sib%prog%cas_old  = 0.0
00361         sib%prog%expand  = 0.0
00362         sib%prog%pco2m = 0.0
00363 
00364 !itb_cos
00365         sib%prog%pcosm = 0.0
00366         sib%prog%pcosap = 0.0
00367         sib%diag%cosflux = 0.0
00368         sib%diag%cos_flux_pbl = 0.0
00369         sib%diag%cos_grnd = 0.0
00370         sib%diag%cos_temp = 0.0
00371         sib%diag%coss    = 0.0
00372         sib%diag%cosi    = 0.0
00373         sib%diag%cosc    = 0.0
00374 
00375         sib%prog%sw_dwn = 0.0
00376         sib%prog%sw_dwn1  = 0.0
00377         sib%prog%sw_dwn2  = 0.0
00378         sib%prog%radvbc  = 0.0
00379         sib%prog%radvdc          = 0.0
00380         sib%prog%radnbc = 0.0
00381         sib%prog%radndc  = 0.0
00382         sib%prog%dlwbot = 0.0
00383         sib%prog%dlwbot1 = 0.0
00384         sib%prog%dlwbot2 = 0.0
00385         sib%prog%vdcsav = 0.0
00386         sib%prog%tm = 0.0
00387         sib%prog%tm1 = 0.0
00388         sib%prog%tm2  = 0.0
00389         sib%prog%thm = 0.0
00390         sib%prog%sh = 0.0
00391         sib%prog%sh1  = 0.0
00392         sib%prog%sh2 = 0.0
00393         sib%prog%em = 0.0
00394         sib%prog%ps  = 0.0
00395         sib%prog%ps1 = 0.0
00396         sib%prog%ps2 = 0.0
00397         sib%prog%psb = 0.0
00398         sib%prog%zb = 0.0
00399         sib%prog%ros = 0.0
00400         sib%prog%cupr = 0.0
00401         sib%prog%cupr1 = 0.0
00402         sib%prog%cupr2 = 0.0
00403         sib%prog%lspr = 0.0
00404         sib%prog%lspr1 = 0.0
00405         sib%prog%lspr2 = 0.0
00406         sib%prog%spdm  = 0.0
00407         sib%prog%spdm1 = 0.0
00408         sib%prog%spdm2 = 0.0
00409         sib%prog%tcc1  = 0.0
00410         sib%prog%tcc2 = 0.0
00411         sib%prog%d13cca = 0.0
00412         sib%prog%d13cm = 0.0
00413 
00414         do i=1,2
00415                 sib%prog%bps(i) = 0.0
00416                 sib%prog%capac(i) = 0.0
00417         enddo
00418         
00419         do i=1,6
00420                 sib%prog%rst(i) = 0.0
00421         enddo
00422         
00423         
00424                 !sib%prog%dz(-nsnow+1:nsoil) = 0.0
00425                 !sib%prog%layer_z(-nsnow:nsoil) = 0.0
00426                 !sib%prog%node_z(-nsnow+1:nsoil) = 0.0
00427                 !sib%prog%vol_ice(-nsnow+1:nsoil) = 0.0
00428                 !sib%prog%vol_liq(-nsnow+1:nsoil) = 0.0
00429                 !sib%prog%www_ice(-nsnow+1:nsoil) = 0.0
00430                 !sib%prog%www_liq(-nsnow+1:nsoil) = 0.0
00431                 !sib%prog%td(-nsnow+1:nsoil = 0.0       
00432         
00433 !------------------------------------------------------------------
00434 !                   STATUS VARIABLES
00435 !------------------------------------------------------------------
00436 
00437         sib%stat%coszbar = 0.0
00438         sib%stat%cosz = 0.0
00439         sib%stat%dayflag = 0.0
00440         sib%stat%julday = 0
00441         sib%stat%pt_num = 0
00442         
00443 
00444 
00445 
00446 
00447 
00448 end subroutine init_var
00001 subroutine vntlat(sib,sib_loc)
00002 
00003 
00004     use kinds
00005     use sibtype
00006 
00007     use sib_const_module, only:  &
00008         vkrmn,  &
00009         snofac, &
00010         dtt,    &
00011         zwind,  &
00012         ztemp
00013 
00014     implicit none
00015 
00016     !----------------------------------------------------------------------
00017 
00018     type(sib_t), intent(inout) :: sib
00019     type(sib_local_vars)     ,intent(inout) :: sib_loc
00020                                        ! variables local to SiB
00021 
00022     !----------------------------------------------------------------------  
00023 
00024     !...LOCAL VARIABLES
00025 
00026     real(kind=dbl_kind) :: ps
00027     real(kind=dbl_kind) :: u2 
00028     real(kind=dbl_kind) :: cuni 
00029     real(kind=dbl_kind) :: temv 
00030     real(kind=dbl_kind) :: zzwind
00031     real(kind=dbl_kind) :: zztemp
00032     real(kind=dbl_kind) :: eps
00033     real(kind=dbl_kind) :: epsc
00034     real(kind=dbl_kind) :: epsg 
00035 
00036     eps    = 1. / snofac                                   
00037 
00038     !
00039     !   calculate ventilation mass flux
00040     !
00041 
00042     zzwind = sib%param%z2 - sib%param%zpd_adj + zwind
00043     zztemp = sib%param%z2 - sib%param%zpd_adj + ztemp
00044 
00045 
00046     call vmfcalz(sib,zzwind,zztemp,cuni)
00047 
00048     !                                                                       
00049     !     AERODYNAMIC RESISTANCE                                            
00050     !                             
00051 
00052     sib%diag%ra    = sib%prog%ros / sib%diag%ventmf 
00053     temv = (sib%param%z2 - sib%param%zpd_adj) / sib%param%z0
00054 !    print*,'vntlat,temv:',sib%param%z2,sib%param%zpd_adj,sib%param%z0
00055     !itb...PATCH...make sure that temv is not negative
00056     temv = max(temv,1.00_dbl_kind)
00057     temv = log(temv) 
00058     u2     = sib%prog%spdm / (cuni * vkrmn)
00059     u2 = u2 * temv
00060 
00061     !itb...HARDWIRE PATCH...keeping u2 from being zero. That's bad...
00062 !    u2 = MAX(u2,1.0_dbl_kind)
00063 
00064     sib%diag%drag(1) = sib%prog%ros * sib%diag%cu * sib%diag%ustar
00065 
00066     sib_loc%fc = 1.0
00067     sib_loc%fg = 1.0                                  
00068 
00069     !...calculate leaf surface-CAS and ground-CAS resistance
00070     call rbrd(sib,u2)
00071 
00072     epsc = 1.
00073     epsg = 1. 
00074     !   this only makes sense for canopy leaves, since
00075     !   there can only be water OR snow, not both. switching epsc
00076     !   epsc to eps makes the hltm adapt to freezing/fusion.
00077     if(sib%prog%snow_veg > 0.0) epsc = eps
00078     if(sib%prog%nsl      < 0)   epsg = eps
00079     !
00080     !   compute resistance terms
00081     !
00082     sib%diag%rc = sib%prog%rst(6) + sib%diag%rb + sib%diag%rb
00083 
00084     sib%diag%rds = sib%diag%rsoil * sib_loc%fg + sib%diag%rd
00085 
00086     sib_loc%gect =  (1. - sib%diag%wc) / sib%diag%rc
00087     sib_loc%geci = epsc * sib%diag%wc / (sib%diag%rb + sib%diag%rb)
00088 
00089     sib_loc%gegs =  (1. - sib%diag%wg) / sib%diag%rds
00090     sib_loc%gegi = epsg * sib%diag%wg / sib%diag%rd
00091 
00092     sib_loc%coc = sib_loc%gect + sib_loc%geci
00093 
00094     !...calculate ecmass -- canopy evapotranspiration
00095     !...temporary value to be passed into phosib
00096     sib%diag%ecmass = (sib_loc%etc - sib%prog%ea) * sib_loc%coc *  &
00097         sib%prog%ros  * 0.622e0_dbl_kind /sib%prog%ps * dtt
00098 
00099     !
00100     !   calculate soil respiration
00101     !
00102     call respsib(sib)
00103     !
00104     !   calculation of canopy conductance and photosynthesis
00105     !
00106 
00107     call phosib(sib,sib_loc)
00108 
00109     if(sib%prog%ea > sib_loc%etc) sib_loc%fc = 0.0
00110     if(sib%prog%ea > sib_loc%etg) sib_loc%fg = 0.0
00111     sib%diag%hrr = sib%diag%hr                                                
00112     if (sib_loc%fg < 0.5) sib%diag%hrr = 1.0                           
00113 
00114 end subroutine vntlat
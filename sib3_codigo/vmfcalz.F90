00001 subroutine vmfcalz(sib,zzwind,zztemp,cuni)
00002 
00003     use kinds
00004     use sibtype
00005     use sib_const_module, only:  &
00006         vkrmn
00007     use physical_parameters, only:  &
00008         grav,  &
00009         delta
00010 
00011 
00012     implicit none
00013 
00014     !----------------------------------------------------------------------
00015 
00016     type(sib_t), intent(inout) :: sib
00017 
00018     !----------------------------------------------------------------------  
00019 
00020 !*****************************************************************************                                                                       
00021 !     VENTILATION MASS FLUX,Ustar, and transfer coefficients for momentum 
00022 !     and heat fluxes, based on by Louis (1979, 1982), and revised by Holtslag
00023 !     and Boville(1993), and by Beljaars and Holtslag (1991).              
00024 !  
00025 !     Rerences:
00026 !       Beljars and Holtslag (1991): Flux parameterization over land surfaces
00027 !              for atmospheric models. J. Appl. Meteo., 30, 327-341.
00028 !       Holtslag and Boville (1993): Local versus nonlocal boundary-layer 
00029 !              diffusion in a global climate model. J. of Climate, 6, 1825-
00030 !              1842.
00031 !       Louis, J. F., (1979):A parametric model of vertical eddy fluxes in
00032 !              atmosphere. Boundary-Layer Meteo., 17, 187-202.
00033 !       Louis, Tiedke, and Geleyn, (1982): A short history of the PBL
00034 !              parameterization at ECMWF. Proc. ECMWF Workshop on Boundary-
00035 !              Layer parameterization, ECMWF, 59-79.
00036 !
00037 !     General formulation:
00038 !        surface_flux = transfer_coef.*U1*(mean_in_regerence - mean_at_sfc.) 
00039 !     Transfer coefficients for mommentum and heat fluxes are:
00040 !        CU = CUN*Fm, and
00041 !        CT = CTN*Fh
00042 !        where  CUN and CTN are nutral values of momentum and heat transfers,
00043 !           and Fm and Fh are stability functions derived from surface
00044 !           similarity relationships.     
00045 !*****************************************************************************
00046 
00047 
00048 
00049     real(kind=dbl_kind) ,intent(in) ::  
00050         zzwind,  ! adjusted wind measurement height (m)       
00051         zztemp    ! adjusted temp measurement height (m)
00052 
00053     !...OUTPUT VARIABLES
00054     real(kind=dbl_kind) ,intent(out) :: 
00055         cuni    ! 1/ momentum transfer coefficient
00056 
00057     !...PATCH WARNING: an unjustified patch has been put in the code, 
00058     !...whereupon when cuni=1/cun is calculated, the square root is taken.
00059     !...this is a patch that makes the results better, but it is 
00060     !...unjustified scientifically.
00061 
00062 
00063     !...LOCAL VARIABLES
00064     integer ::   i
00065 
00066     !  constants for surface flux functions, according to Holtslag and
00067     !      Boville (1993, J. Climate)
00068     real(kind=dbl_kind), parameter ::  
00069         bunstablM = 10.0,  !  
00070         bunstablT = 15.0,  !
00071         cunstablM = 75.0,  ! 
00072         cunstablT = 75.0,  !
00073         bstabl =  8.0,     !
00074         cstabl = 10.0       ! 
00075 
00076     real(kind=dbl_kind) ::   
00077         wgm,      ! moisture mixing ratio deficit, 
00078                    !  CAS to reference layer (kg/kg)
00079         thgm,    & ! temperature difference (theta) CAS-ref level (K)
00080         z1z0u,   & ! ratio of reference height to roughness length
00081         z1z0urt, & ! square root of z1z0u
00082         z1z0t,   & ! ratio of reference height to roughness length
00083         z1z0trt, & ! square root of z1z0t
00084         !...currently, z1z0u and z1z0t are identical. theoretically, they 
00085         !...can be changed for different wind/temp measurement heights. 
00086         !...they both use zzwind right now.
00087         cun,     & ! momentum transfer coefficient (?)
00088         ctn,     & ! thermal transfer coefficient (?)
00089         temv,    & ! part of Richardson No. calculation
00090         zrib,    & ! part of Richardson No. calculation
00091         rib,     & ! Richardson Number
00092         fmomn,   & !
00093         fheat      !
00094 
00095     real(kind=dbl_kind) ::  
00096         ribtemp,  !
00097         dm,       !
00098         dh         !
00099 
00100 
00101 
00102 
00103 
00104     zrib = zzwind **2 / zztemp                                                    
00105     wgm  = sib%prog%sha - sib%prog%sh   
00106                                    
00107     !                                                                       
00108     ! SFC-AIR DEFICITS OF MOISTURE AND POTENTIAL TEMPERATURE         
00109     ! WGM IS THE EFFECTIVE SFC-AIR TOTAL MIXING RATIO DIFFERENCE.    
00110     !                                                                       
00111     thgm  = sib%prog%tha  - sib%prog%thm                                   
00112     sib%diag%thvgm = thgm + sib%prog%tha * delta * wgm       
00113 
00114 
00115     !   Ratio of reference height (zwind/ztemp) and roughness length:
00116     z1z0u = zzwind/sib%param%z0
00117     z1z0urt = sqrt( z1z0U )
00118     z1z0u = log( z1z0U )
00119     z1z0t = zzwind/sib%param%z0
00120     z1z0trt = sqrt( z1z0T )
00121     z1z0t = log( z1z0T )
00122 
00123     !   Neutral surface transfers for momentum CUN and for heat/moisture CTN:
00124 
00125     cun = vkrmn*vkrmn / (z1z0u*z1z0u )   !neutral Cm & Ct
00126     ctn = vkrmn*vkrmn / (z1z0t*z1z0t )
00127 
00128     !...PATCH-when 1/cun is calculated, the square root is taken.
00129     cuni = z1z0u / vkrmn
00130 
00131     !                                                                       
00132     !   SURFACE TO AIR DIFFERENCE OF POTENTIAL TEMPERATURE.            
00133     !   RIB IS THE BULK RICHARDSON NUMBER, between reference
00134     !   height and surface.
00135 
00136     temv = sib%prog%tha * sib%prog%spdm * sib%prog%spdm   
00137     temv = max(0.000001_dbl_kind,temv)
00138     rib = -sib%diag%thvgm * grav * zrib / temv 
00139 
00140     !   The stability functions for momentum and heat/moisture fluxes as
00141     !   derived from the surface-similarity theory by Luis (1079, 1982), and
00142     !   revised by Holtslag and Boville(1993), and by Beljaars and Holtslag 
00143     !   (1991).
00144 
00145     if(rib .ge. 0.0) then                                           
00146 
00147         !  THE STABLE CASE. RIB IS USED WITH AN UPPER LIMIT              
00148 
00149         rib   = min( rib, 0.5_dbl_kind)                   
00150         fmomn = (1. + cstabl * rib * (1.+ bstabl * rib))
00151         fmomn = 1. / fmomn
00152         fmomn = max(0.0001_dbl_kind,fmomn)
00153         fheat = fmomn
00154 
00155     else                                  
00156 
00157         !  THE UNSTABLE CASE.    
00158 
00159         ribtemp = abs(rib)
00160         ribtemp = sqrt( ribtemp )
00161         dm      = 1. + cunstablM * cun * z1z0Urt * ribtemp
00162         dh      = 1. + cunstablT * ctn * z1z0Trt * ribtemp
00163         fmomn   = 1. - (bunstablM * rib ) / dm
00164         fheat   = 1. - (bunstablT * rib ) / dh
00165 
00166     endif    
00167 
00168     !   surface-air transfer coefficients for momentum CU, for heat and 
00169     !   moisture CT. The CUI and CTI are inversion of CU and CT respectively.
00170 
00171     sib%diag%cu = cun * fmomn 
00172     sib%diag%ct = ctn * fheat
00173 
00174     !   Ustar and ventlation mass flux: note that the ustar and ventlation 
00175     !   are calculated differently from the Deardoff's methods due to their
00176     !   differences in define the CU and CT.
00177 
00178     sib%diag%ustar  = sib%prog%spdm * sib%prog%spdm * sib%diag%cu 
00179     sib%diag%ustar  = sqrt( sib%diag%ustar ) 
00180     sib%diag%ventmf = sib%prog%ros * sib%diag%ct * sib%prog%spdm  
00181 !print *, 'vmfcalz', sib%prog%ros, sib%diag%ct, sib%prog%spdm, sib%diag%ventmf
00182     !                                                                       
00183     ! Note there is no CHECK FOR VENTMF EXCEEDS TOWNSENDS(1962) FREE CONVECTION  
00184     ! VALUE, like DEARDORFF EQ(40B), because the above CU and CT included
00185     ! free convection conditions.                                            
00186 
00187 
00188 end subroutine vmfcalz
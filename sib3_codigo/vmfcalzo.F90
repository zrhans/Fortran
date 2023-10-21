00001 
00002 subroutine vmfcalzo(sib,zzwind,zztemp) 
00003 
00004 
00005 !*****************************************************************************
00006 !     VENTILATION MASS FLUX,Ustar, and transfer coefficients for momentum 
00007 !     and heat fluxes, based on by Louis (1979, 1982), and revised by Holtslag
00008 !     and Boville(1993), and by Beljaars and Holtslag (1991).              
00009 !  
00010 !     Rerences:
00011 !       Beljars and Holtslag (1991): Flux parameterization over land surfaces
00012 !              for atmospheric models. J. Appl. Meteo., 30, 327-341.
00013 !       Holtslag and Boville (1993): Local versus nonlocal boundary-layer 
00014 !              diffusion in a global climate model. J. of Climate, 6, 1825-
00015 !              1842.
00016 !       Louis, J. F., (1979):A parametric model of vertical eddy fluxes in
00017 !              atmosphere. Boundary-Layer Meteo., 17, 187-202.
00018 !       Louis, Tiedke, and Geleyn, (1982): A short history of the PBL
00019 !              parameterization at ECMWF. Proc. ECMWF Workshop on Boundary-
00020 !              Layer parameterization, ECMWF, 59-79.
00021 !
00022 !     General formulation:
00023 !        surface_flux = transfer_coef.*U1*(mean_in_regerence - mean_at_sfc.) 
00024 !     Transfer coefficients for mommentum and heat fluxes are:
00025 !        CU = CUN*Fm, and
00026 !        CT = CTN*Fh
00027 !        where  CUN and CTN are nutral values of momentum and heat transfers,
00028 !           and Fm and Fh are stability functions derived from surface
00029 !           similarity relationships.     
00030 !*****************************************************************************
00031 
00032     use kinds
00033     use sibtype
00034 
00035     use physical_parameters, only:  &
00036         grav
00037 
00038     use sib_const_module, only:  &
00039         vkrmn
00040 
00041     implicit none
00042 
00043 
00044     !----------------------------------------------------------------------
00045 
00046     type(sib_t), intent(inout) :: sib
00047 
00048     !----------------------------------------------------------------------  
00049 
00050 
00051 
00052     real(kind=dbl_kind),intent(in) :: zzwind
00053     real(kind=dbl_kind),intent(in) :: zztemp
00054 
00055 
00056     !...LOCAL VARIABLES...
00057     real(kind=dbl_kind) :: zrib
00058     real(kind=dbl_kind) :: z1z0u
00059     real(kind=dbl_kind) :: z1z0urt
00060     real(kind=dbl_kind) :: cun
00061     real(kind=dbl_kind) :: temv
00062     real(kind=dbl_kind) :: rib
00063     real(kind=dbl_kind) :: fmomn
00064     real(kind=dbl_kind) :: ribtemp
00065     real(kind=dbl_kind) :: dm
00066 
00067     ! constants for surface flux functions, according to Holtslag and
00068     ! Boville (1993, J. Climate)
00069     ! constants for unstable function
00070     real(kind=dbl_kind),parameter:: bunstablM = 10.  
00071     real(kind=dbl_kind),parameter:: cunstablM = 75.
00072 
00073                 ! constants for stable function
00074     real(kind=dbl_kind),parameter:: bstabl = 8.   
00075     real(kind=dbl_kind),parameter:: cstabl = 10.
00076 
00077 
00078     zrib = zzwind**2.0 / zztemp
00079 
00080     !   Ratio of reference height (zwind/ztemp) and roughness length:
00081     z1z0u = zzwind/ 0.0002   ! oceanic roughness length
00082     z1z0urt = sqrt( z1z0U )
00083     z1z0u = log( z1z0U )
00084 
00085     !   Neutral surface transfers for momentum CUN and for heat/moisture CTN:
00086 
00087     cun = vkrmn*vkrmn / (z1z0u*z1z0u )   !neutral Cm & Ct
00088 
00089     !   SURFACE TO AIR DIFFERENCE OF POTENTIAL TEMPERATURE.            
00090     !   RIB IS THE BULK RICHARDSON NUMBER, between reference height and surface.
00091 
00092     temv = sib%prog%tha * sib%prog%spdm * sib%prog%spdm                          
00093     temv = max(1.0e-6_dbl_kind,temv)
00094     rib = -sib%diag%thvgm * grav * zrib / temv 
00095 
00096     !   The stability functions for momentum and heat/moisture fluxes as
00097     !   derived from the surface-similarity theory by Luis (1079, 1982), and
00098     !   revised by Holtslag and Boville(1993), and by Beljaars and Holtslag 
00099     !   (1991).
00100 
00101     if(rib >= 0.0_dbl_kind) then                                           
00102 
00103         !  THE STABLE CASE. RIB IS USED WITH AN UPPER LIMIT              
00104 
00105         rib = min( rib, 0.5_dbl_kind)                   
00106         fmomn = (1. + cstabl * rib * (1.+ bstabl * rib))
00107         fmomn = 1. / fmomn
00108         fmomn = max(1.0e-4_dbl_kind,fmomn)
00109 
00110     else                                  
00111 
00112         !  THE UNSTABLE CASE.    
00113 
00114         ribtemp = abs(rib)
00115         ribtemp = sqrt( ribtemp )
00116         dm = 1. + cunstablm * cun * z1z0urt * ribtemp
00117         fmomn = 1. - (bunstablm * rib ) / dm
00118 
00119     end if    
00120 
00121     !   surface-air transfer coefficients for momentum CU, for heat and 
00122     !   moisture CT.
00123 
00124     sib%diag%cu = cun * fmomn 
00125 
00126     !   Ustar and ventlation mass flux: note that the ustar and ventlation 
00127     !   are calculated differently from the Deardoff's methods due to their
00128     !   differences in define the CU and CT.
00129 
00130     sib%diag%ustar   = sib%prog%spdm * sib%prog%spdm * sib%diag%cu 
00131     sib%diag%ustar   = sqrt( sib%diag%ustar ) 
00132     sib%diag%drag(2) = sib%prog%ros * sib%diag%cu * sib%diag%ustar
00133 
00134     ! Note there is no CHECK FOR VENTMF EXCEEDS TOWNSENDS(1962) FREE CONVECTION  
00135     ! VALUE, like DEARDORFF EQ(40B), because the above CU and CT included
00136     ! free convection conditions. 
00137 
00138 
00139 
00140 end subroutine vmfcalzo
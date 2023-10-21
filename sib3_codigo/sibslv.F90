00001 
00002 !======================SUBROUTINE SIBSLV=================================
00003 
00004 subroutine sibslv(sib,sib_loc)   
00005 
00006 !========================================================================
00007 !
00008 !     Calculation of time increments in Tc, Tgs, Theta-m and Qm using an
00009 !        implicit backwards method with explicit coefficients.  
00010 !pl   Similar to equations (10-15), SA-92B. 
00011 !
00012 !     Longwave feedbacks are now really included
00013 !
00014 !======================================================================== 
00015 
00016 !++++++++++++++++++++++++++++++OUTPUT+++++++++++++++++++++++++++++++++++
00017 !
00018 !       DTC            CANOPY TEMPERATURE INCREMENT (K)
00019 !       DTG            GROUND SURFACE TEMPERATURE INCREMENT (K)
00020 !       DTH            MIXED LAYER POTENTIAL TEMPERATURE INCREMENT (K)
00021 !       DQM            MIXED LAYER MIXING RATIO INCREMENT (KG KG-1)
00022 !       ETMASS (FWS)   EVAPOTRANSPIRATION (MM)
00023 !       HFLUX (FSS)    SENSIBLE HEAT FLUX (W M-2)
00024 !
00025 !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
00026 
00027 use kinds
00028 use sibtype
00029 
00030 !itb...used for diagnostic print...
00031 use sib_const_module, only:  &
00032     nsoil,  &
00033     nsnow,  &
00034     dtt,    &
00035     dti
00036 
00037 use physical_parameters, only:  &
00038     grav,                &
00039     cp => spec_heat_cp,  &
00040     hltm
00041 IMPLICIT none
00042 
00043 !----------------------------------------------------------------------
00044 
00045 type(sib_t), intent(inout) :: sib
00046 
00047 type(sib_local_vars)     ,intent(inout) :: sib_loc
00048                                    ! variables local to SiB
00049 
00050 !----------------------------------------------------------------------  
00051 
00052 
00053 
00054 real(kind=dbl_kind),parameter :: grav2 = grav * 0.01_dbl_kind
00055 
00056 integer(kind=int_kind) :: error_flag,i,j,k
00057 
00058 real(kind=dbl_kind):: aag
00059 real(kind=dbl_kind):: aac
00060 real(kind=dbl_kind):: aam
00061 real(kind=dbl_kind):: bbg
00062 real(kind=dbl_kind):: bbc
00063 real(kind=dbl_kind):: bbm
00064 real(kind=dbl_kind):: temv 
00065 
00066 !...matrix arrays - variable, due to potential snow layers
00067 
00068 real(kind=dbl_kind),dimension((nsoil + nsnow) -    
00069       sib%prog%nsl,(nsoil + nsnow) - sib%prog%nsl) :: avec
00070 
00071 real(kind=dbl_kind),dimension((nsoil + nsnow) -    
00072                                    sib%prog%nsl)   :: bvec,bv_copy
00073 
00074 integer(kind=int_kind),dimension((nsoil + nsnow) -  
00075                                    sib%prog%nsl)   :: cvec   
00076 
00077 !...this routine sets up the coupled system of partial 
00078 !...differential equations described in Sato et al., 
00079 !...with the exception that now Ta and ea are prognostic
00080 !...variables, and so we have two more equations, reflected
00081 !...in two more lines and two more columns as compared 
00082 !...to the old sibslv.F (used for no prognistic CAS calculations)
00083 
00084 !...this matrix is expandable, size determined by the number of 
00085 !...snow layers (0 to 5). All energy is passed through the snow layer
00086 !...if snow exists. Partial snowcover is not dealt with. We know
00087 !...that this is physically incorrect, it is an issue that we hope
00088 !...to deal with in the future...
00089 
00090 
00091 !itb...VARIABLES
00092 !...1 : TREF (lowest model layer/driver data temperature)
00093 !...2 : EREF (lowest model layer/driver data water vapor)
00094 !...3 : TA   (CAS temperature) 
00095 !...4 : EA   (CAS water vapor)
00096 !...5 : TC   (vegetation temperature)
00097 !...6 : TG   (Ground or snow surface temperature)
00098 !...7-14 to 19 : TD (interior soil layers temperature)
00099 !...last member: TD (bottom soil level)
00100 
00101 
00102 !*****************************************************
00103 !
00104 !   THESE FIRST TWO EQUATIONS (TREF AND EREF) ARE
00105 !   FOR THE 'OFFLINE SiB' SITUATION, WHERE SiB IS
00106 !   DRIVEN BY TOWER DATA. FOR COUPLING TO A MODEL
00107 !   (GCM OR MESOSCALE MODEL), USE THE SECOND SET
00108 !   OF EQUATIONS. REFERENCE IS TO KALNAY AND KANAMITSU
00109 !   (1988) RIGHT NOW, BUT I'LL WRITE IT UP LATER...
00110 !
00111 !*****************************************************
00112 
00113 !itb...zero all of 'em out first, then fill 'em in...
00114     avec(:,:) = 0.0
00115 
00116     !1     TREF EQUATION       
00117 
00118     avec(1,1)             =  1.0                                 
00119     avec(1,2:(nsoil + nsnow) - sib%prog%nsl)  =  0.0            
00120     bvec(1)               =  0.0  
00121 
00122     !2     EREF EQUATION  
00123 
00124     avec(2,1)             =  0.0                                 
00125     avec(2,2)             =  1.0                                 
00126     avec(2,3:(nsoil + nsnow) - sib%prog%nsl)  =  0.0            
00127     bvec(2)               =  0.0        
00128 
00129 !********************************************************************
00130 !     TREF EQUATION - COUPLED
00131 !        avec(1,1)             =  (sib%cp * sib%prog%psb)/(grav*    &
00132 !                                  (0.5_dbl_kind * dtt)) + sib_loc%hadta
00133 !        avec(1,2)             = 0.0
00134 !        avec(1,3)             = -sib_loc%hadta
00135 !        avec(1,4:15-sib%prog%nsl)  = 0.0
00136 !        bvec(2)               = sib%diag%fss * dti
00137 !
00138 !     EREF EQUATION - COUPLED
00139 !        avec(2,1)             = 0.0
00140 !        avec(2,2)             = (sib%cp * sib%prog%psb)/    &
00141 !                                (grav * (0.5_dbl_kind * dtt)    &
00142 !                                 * sib%diag%psy) + sib_loc%eadea
00143 !        avec(2,3)             = sib_loc%eadem
00144 !        avec(2,4:15-sib%prog%nsl)  = 0.0
00145 !        bvec(3)               = sib%diag%fws * dti
00146 !*********************************************************************        
00147 
00148     !3     TA EQUATION 
00149     if (sib%prog%nsl == 0) then
00150        avec(3,1)  =  sib_loc%hadth                                    
00151        avec(3,2)  =  0.0     
00152        avec(3,3)  =  sib%diag%cas_cap_heat   * dti  & 
00153             + sib_loc%hadta  - sib_loc%hcdta - sib_loc%hgdta 
00154        avec(3,4)  =  0.0         
00155        avec(3,5)  =  - sib_loc%hcdtc     
00156        avec(3,6) = - sib_loc%hgdtg
00157        bvec(3)    =  sib%diag%hc * dti - sib%diag%fss * dti + sib%diag%hg * dti 
00158     else
00159        avec(3,1)  =  sib_loc%hadth                                    
00160        avec(3,2)  =  0.0     
00161        avec(3,3)  =  sib%diag%cas_cap_heat   * dti  & 
00162             + sib_loc%hadta  - sib_loc%hcdta - sib_loc%hsdta
00163        avec(3,4)  =  0.0         
00164        avec(3,5)  =  - sib_loc%hcdtc     
00165        avec(3,6) = - sib_loc%hsdts
00166        bvec(3)    =  sib%diag%hc * dti - sib%diag%fss * dti + sib%diag%hs * dti    
00167     endif
00168 
00169     !4    EA EQUATION  
00170     if (sib%prog%nsl == 0) then
00171        avec(4,1)  =  0.0                                  
00172        avec(4,2)  =  sib_loc%eadem                                  
00173        avec(4,3)  =  0.0                                  
00174        avec(4,4)  =  sib%diag%cas_cap_vap   * dti  &
00175             + sib_loc%eadea  - sib_loc%ecdea - sib_loc%egdea
00176        avec(4,5)  =  - sib_loc%ecdtc               
00177        avec(4,6)  =  - sib_loc%egdtg         
00178        bvec(4)    =  sib%diag%ec * dti - sib%diag%fws * dti + sib%diag%eg * dti 
00179     else
00180        avec(4,1)  =  0.0                                  
00181        avec(4,2)  =  sib_loc%eadem                                  
00182        avec(4,3)  =  0.0                                  
00183        avec(4,4)  =  sib%diag%cas_cap_vap   * dti  &
00184             + sib_loc%eadea  - sib_loc%ecdea - sib_loc%esdea  
00185        avec(4,5)  =  - sib_loc%ecdtc               
00186        avec(4,6)  =  - sib_loc%esdts 
00187        bvec(4)    =  sib%diag%ec * dti - sib%diag%fws * dti + sib%diag%es * dti   
00188     endif
00189 
00190     !5    TC EQUATION 
00191     avec(5,1)  =  0.0                                   
00192     avec(5,2)  =  0.0                               
00193     avec(5,3)  =  sib_loc%hcdta                                    
00194     avec(5,4)  =  sib_loc%ecdea          
00195     avec(5,5)  =  sib%param%czc * dti + sib_loc%hcdtc    &
00196         + sib_loc%ecdtc + sib_loc%lcdtc
00197     avec(5,6)  =  sib_loc%lcdtg
00198     bvec(5)    =  sib%diag%radt(1) - sib%diag%hc * dti - sib%diag%ec * dti      
00199 
00200     !6    TOP SOIL LAYER (TD1 OR TG) EQUATION
00201     if (sib%prog%nsl == 0) then !NO SNOW CASE
00202         avec(6,1)  =  0.0                                  
00203         avec(6,2)  =  0.0
00204         avec(6,3)  =  sib_loc%hgdta                                  
00205         avec(6,4)  =  sib_loc%egdea          
00206         avec(6,5)  =  sib_loc%lgdtc               
00207         avec(6,6)  =  sib%param%slamda(1) +  sib%param%shcap(1) *  dti    &
00208             + sib_loc%hgdtg + sib_loc%egdtg + sib_loc%lgdtg          
00209         avec(6,7)  =  -sib%param%slamda(1) 
00210         bvec(6)    =  sib%diag%radt(2) - sib%diag%hg * dti - sib%diag%eg * dti  &
00211             - sib%param%slamda(1) * (sib%prog%td(1) - sib%prog%td(2))
00212     else   ! SNOW CASE
00213         avec(6,1)  =  0.0                                  
00214         avec(6,2)  =  0.0
00215         avec(6,3)  =  sib_loc%hsdta                            
00216         avec(6,4)  =  sib_loc%esdea       
00217         avec(6,5)  =  sib_loc%lsdtc              
00218         avec(6,6)  =  sib%param%slamda(sib%prog%nsl+1) +  &
00219              sib%param%shcap(sib%prog%nsl+1) * dti + sib_loc%hsdts + sib_loc%esdts + sib_loc%lsdts 
00220         avec(6,7)  =  -sib%param%slamda(sib%prog%nsl+1)
00221         bvec(6)    =  sib%diag%radt(3) - &
00222              sib%diag%hs * dti - sib%diag%es * dti - &
00223              sib%param%slamda(sib%prog%nsl+1) * (sib%prog%td(sib%prog%nsl+1) - &
00224              sib%prog%td(sib%prog%nsl+2))
00225     endif
00226 
00227     !7-??    INTERIOR SOIL LAYERS
00228 
00229     !itb...need to scale this for snow/no snow (sib%prog%nsl)
00230 
00231     do i = 7, (nsoil + nsnow - 1) - sib%prog%nsl   ! matrix indices
00232         k = i - nsnow + sib%prog%nsl    ! soil layer indices
00233 
00234     !itb...fill all matrix components with zero, re-fill the 3 necessary
00235     !   ...positions 
00236         do j=1, (nsoil + nsnow)  - sib%prog%nsl
00237             avec(i,j) = 0.0
00238         enddo
00239 
00240         avec(i,i-1) = -sib%param%slamda(k-1)
00241 
00242         avec(i,i)   = sib%param%shcap(k)*dti + sib%param%slamda(k)  &
00243             + sib%param%slamda(k-1)
00244 
00245         avec(i,i+1) = -sib%param%slamda(k)
00246 
00247         bvec(i)     = sib%param%slamda(k)*(sib%prog%td(k+1) - sib%prog%td(k))  &
00248             - sib%param%slamda(k-1) * (sib%prog%td(k) - sib%prog%td(k-1))
00249 
00250 
00251     enddo
00252 
00253 
00254 
00255 
00256     !    BOTTOM SOIL LAYER
00257     i = (nsoil + nsnow) - sib%prog%nsl
00258     do j=1,i-2
00259         avec(i,j)  =  0.0 
00260     enddo                                
00261     avec(i,i-1) =  sib%param%slamda(nsoil - 1) * (sib%prog%td(nsoil)    &
00262                                           - sib%prog%td(nsoil - 1))             
00263     avec(i,i) =  sib%param%shcap(nsoil) * dti  &
00264         + sib%param%slamda(nsoil - 1 ) 
00265     bvec(i)    =  - sib%param%slamda(nsoil - 1) * (sib%prog%td(nsoil)   &
00266         - sib%prog%td(nsoil - 1))
00267 
00268 
00269     !     SOLVE MATRIX EQUATION   
00270     !call dsimul(avec,bvec,cvec,1,15-sib%prog%nsl,15-sib%prog%nsl,error_flag)
00271     !jlc...use the lapack version of this call, instead of our home-brewed
00272     !FIXME: cvec is temporarily in the place of IPIV (the pivot array)
00273 
00274     call dgesv( (nsoil + nsnow) - sib%prog%nsl, 1, avec,      &
00275                 (nsoil + nsnow) - sib%prog%nsl, cvec, bvec, &
00276                 (nsoil + nsnow) - sib%prog%nsl, error_flag )
00277  
00278     sib_loc%dth = bvec(1)           
00279     sib_loc%dqm = bvec(2)
00280     sib_loc%dta = bvec(3)
00281     sib_loc%dea = bvec(4)
00282     sib_loc%dtc = bvec(5)        
00283 !    sib_loc%dtg = bvec(6)   
00284 !    sib_loc%dts = 0.0 
00285 
00286 
00287 !    sib_loc%dtd(sib%prog%nsl+1) = sib_loc%dtg
00288     do i=6,(nsoil +nsnow) - sib%prog%nsl
00289         sib_loc%dtd(i - 5 + sib%prog%nsl) = bvec(i)
00290  
00291     enddo
00292 
00293 
00294 end subroutine sibslv
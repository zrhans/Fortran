00001 
00002 subroutine soilwater( sib, infil, hk, dhkdw, dw_liq, dzmm, zmm)
00003 
00004 !----------------------------------------------------------------------
00005 !
00006 !     subroutine based on  CLM_SOILWATER
00007 !
00008 !     CLM web info: http://clm.gsfc.nasa.gov
00009 !
00010 !     Description: (taken directly from CLM_SOILWATER.F90)
00011 !     Soil moisture is predicted from a 10-layer model (as with soil
00012 !     temperature), in which the vertical soil moisture transport is 
00013 !     governed by infiltration, runoff, gradient diffusion, gravity 
00014 !     and root extraction through canopy transpiration.  The net water
00015 !     applied to the surface layer is the snowmelt plus precipitation 
00016 !     plus the throughfall of canopy dew minus surface runoff and 
00017 !     evaporation. 
00018 !
00019 !     The vertical water flow in an unsaturated porous media is 
00020 !     described by Darcy's Law, and the hydraulic conductivity and the
00021 !     soil negative potential vary with soil water content and soil 
00022 !     texture based on the work of Clapp and Hornberger (1978) and Cosby 
00023 !     et al. (1984). The equation is integrated over the layer thickness,
00024 !     in which the time rate of change in water mass must equal the net 
00025 !     flow across the bounding interface, plus the rate of internal source
00026 !     or sink.  The terms of water flow across the layer interfaces are 
00027 !     linearly expanded by using first-order Taylor expansion.  The 
00028 !     equations result in a tridiagonal system of equations.
00029 
00030 !     Note: lentgh units here are all millimeter
00031 !
00032 !     Richards Equation
00033 !
00034 !     d wat     d      d wat   d psi
00035 !     ----- = - -- [ k(-----   ----- - 1) ] + S
00036 !      dt       dz       dz    d wat
00037 !
00038 !     where:
00039 !     wat = volumetric water content (m^3/m^3)
00040 !     psi = soil matric potential (mm)
00041 !     dt  = time step (sec)
00042 !     z   = depth (mm)
00043 !     qin = inflow at top of layer (mm H2O/sec)
00044 !     qout= outflow at bottom of layer (mm H2O/sec)
00045 !     s   = source/sink flux (mm H2O/sec)
00046 !     k   = hydraulic conductivity (mm H2O/sec)
00047 !
00048 !     Solution:
00049 !     linearize k and psi about d wat and use tridiagonal system of 
00050 !     equations to solve for d wat, where for layer j
00051 !
00052 !     r_j = a_j[d wat_j-1] + b_j [d wat_j] + c_j [d wat_j+1]
00053 !
00054 !     Revision History
00055 !     15 September 1999: Yongjiu Dai; Initial code
00056 !     15 December  1999: Paul Houser and Jon Radakovich; F90 Revision
00057 !     25 January   2002: Ian Baker; SiB integration
00058 !----------------------------------------------------------------------
00059 
00060 use sibtype
00061 
00062 use physical_parameters, only:  &
00063     grav,  &
00064     tice,  &
00065     hltm
00066 
00067 use sib_const_module, only:  &
00068     nsoil,  &
00069     wimp,   &
00070     phmin,  &
00071     dti,    &
00072     dtt
00073 
00074 implicit none
00075 
00076 !----------------------------------------------------------------------
00077 
00078 type(sib_t), intent(inout) :: sib
00079 
00080 !----------------------------------------------------------------------  
00081 
00082 !...INPUT VARIABLES
00083 real(kind=dbl_kind),intent(in)  :: infil   ! infiltration into soil
00084                                              !  (kg m^-2 sec^-1) 
00085 real(kind=dbl_kind),intent(in)  ::dzmm(1:nsoil)     
00086 real(kind=dbl_kind),intent(in)  ::zmm(1:nsoil)
00087 
00088 
00089 !...OUTPUT VARIABLES
00090 real(kind=dbl_kind),intent(out) :: hk(1:nsoil)
00091                                              ! hydraulic conductivity
00092                                              !  (mm H2O sec^-1)    
00093 real(kind=dbl_kind),intent(out) :: dhkdw(1:nsoil)   
00094                                              ! d(hk)/d(water)     
00095 real(kind=dbl_kind),intent(out) :: dw_liq(1:nsoil)
00096                                              ! change in layer liquid
00097                                              !  (m^3/m^3)(volumetric)
00098 
00099 
00100 
00101 !...local variables...
00102 integer ::   j     ! loop variables
00103 
00104 real(kind=dbl_kind)  :: hltmi     ! 1/hltm
00105 real(kind=dbl_kind)  :: s_node    ! volumetric wetness of node
00106 real(kind=dbl_kind)  :: s1        ! wetness at interface of layer
00107 real(kind=dbl_kind)  :: s2        ! conductivity*wetness**(2b+2)
00108 real(kind=dbl_kind)  :: smp(1:nsoil)
00109                                     ! soil matric potential (mm)
00110 real(kind=dbl_kind)  :: dsmpdw(1:nsoil)
00111                                     ! d(smp)/d(wetness)
00112 real(kind=dbl_kind)  :: qin       ! flux of water into layer 
00113                                     !  (mm H2O sec^-1)
00114 real(kind=dbl_kind)  :: qout      ! flux of water out of layer 
00115                                     !  (mm H2O sec^-1)  
00116 real(kind=dbl_kind)  :: den       ! used in calculating qin,qout   
00117 real(kind=dbl_kind)  :: num       ! used in calculating qin,qout
00118 real(kind=dbl_kind)  :: dqidw0    ! d(qin)/d(vol_liq(j-1)) 
00119 real(kind=dbl_kind)  :: dqidw1    ! d(qin)/d(vol_liq(j))
00120 real(kind=dbl_kind)  :: dqodw1    ! d(qout)/d(vol_liq(j))
00121 real(kind=dbl_kind)  :: dqodw2    ! d(qout)/d(vol_liq(j+1))
00122 real(kind=dbl_kind)  :: rmx(1:nsoil)
00123                                     ! "r" forcing term of tridiag matrix
00124 real(kind=dbl_kind)  :: amx(1:nsoil)
00125                                     ! "a" left off diagonal of tridiag mat
00126 real(kind=dbl_kind)  :: bmx(1:nsoil)
00127                                     ! "b" diagonal column for tridiag mat
00128 real(kind=dbl_kind)  :: cmx(1:nsoil)
00129                                     ! "c" right off diagonal of tridiag mat
00130 
00131 
00132 
00133 !----------------------------------------------------------------------
00134 
00135     hltmi = 1.0/hltm
00136 
00137     !...set hydraulic conductivity to zero if effective porosity 5% in 
00138     !...any two adjoining layers, or if volumetric water (liquid) content
00139     !...less than 0.001
00140 
00141     do j=1,nsoil
00142         if( ((sib%diag%eff_poros(j) < wimp)            .or.   &
00143             (sib%diag%eff_poros(min(nsoil,j+1)) < wimp)) .or. &
00144             (sib%prog%vol_liq(j) <= 1.E-3)) then
00145             hk(j)    = 0.0
00146             dhkdw(j) = 0.0
00147         else
00148             s1 = 0.5*(sib%prog%vol_liq(j)+sib%prog%vol_liq(min(nsoil,j+1)))/ &
00149                 sib%param%poros
00150 
00151             s2 = sib%param%satco*1000.0*s1**(2.0*sib%param%bee+2.0)
00152             hk(j) = s1*s2
00153 
00154             dhkdw(j) = (2.0*sib%param%bee+3.0)*s2*0.5/sib%param%poros
00155             !itb...I don't like the 0.5 factor in the CLM code-don't understand.
00156             !itb...it's in the CLM manual-CLM does not follow Bonan exactly.
00157 
00158 
00159             if(j==nsoil) dhkdw(j) = dhkdw(j)*2.0
00160         endif
00161 
00162         !...evaluate hydraulic conductivity, soil matric potential,
00163         !...d(smp)/d(vol_liq) and d(hk)/d(vol_liq)
00164         if(sib%prog%td(j) > tice) then
00165 
00166             s_node = max(sib%prog%vol_liq(j)/sib%param%poros,0.01_dbl_kind)
00167             s_node = min(1.0_dbl_kind,s_node)
00168 
00169             smp(j) = sib%param%phsat*1000.0*s_node**(-sib%param%bee)
00170 
00171             smp(j) = max(phmin,smp(j))
00172 
00173             dsmpdw(j) = -sib%param%bee*smp(j)/(s_node*sib%param%poros)
00174 
00175         else
00176 
00177         !...when ice is present, the matric potential is only related to
00178         !...temperature by (Fuchs et. al., 1978; Soil Sci. Soc. of Amer. J.
00179         !...42(3); 379-385) 
00180         !...Unit 1 joule = 1kg m2/sec2 j/kg/(m/s2) ==> m ==>1e3mm
00181 
00182             smp(j) = 1.e3_dbl_kind * 0.3336e6_dbl_kind/grav *  &
00183                 (sib%prog%td(j) - tice)/sib%prog%td(j)
00184             smp(j) = max(phmin,smp(j))
00185             dsmpdw(j) = 0.0
00186 
00187         endif
00188     enddo
00189 
00190 
00191     !...set up a, b, c and r vectors for tridiagonal solver
00192     !...node j=1
00193 
00194     j = 1
00195     qin    = infil
00196     den    = zmm(j+1) - zmm(j)
00197     num    = (smp(j+1)-smp(j)) - den
00198     qout   = -hk(j)*num/den
00199     dqodw1 = -(-hk(j)*dsmpdw(j)   + num*dhkdw(j))/den
00200     dqodw2 = -( hk(j)*dsmpdw(j+1) + num*dhkdw(j))/den
00201     rmx(j) = qin - qout  - (((sib%diag%ect * sib%param%rootr(j)) + &
00202         sib%diag%egs) * dti * hltmi)
00203     amx(j) = 0.0
00204     bmx(j) = dzmm(j) * (dti) + dqodw1
00205     cmx(j) = dqodw2
00206 
00207     !...nodes 2 through nsoil-1
00208     do j = 2,nsoil-1 
00209         den    = zmm(j) - zmm(j-1)
00210         num    = (smp(j)-smp(j-1)) - den
00211         qin    = -hk(j-1)*num/den
00212         dqidw0 = -(-hk(j-1)*dsmpdw(j-1) + num*dhkdw(j-1))/den
00213         dqidw1 = -( hk(j-1)*dsmpdw(j)   + num*dhkdw(j-1))/den  
00214         den    = zmm(j+1)-zmm(j)
00215         num    = smp(j+1)-smp(j) - den
00216         qout   = -hk(j)*num/den
00217         dqodw1 = -(-hk(j)*dsmpdw(j)  +  num*dhkdw(j))/den
00218         dqodw2 = -( hk(j)*dsmpdw(j+1) + num*dhkdw(j))/den
00219         rmx(j) = qin - qout  - (sib%diag%ect * dti * sib%param%rootr(j) * hltmi)
00220         amx(j) = -dqidw0
00221         bmx(j) = dzmm(j)*dti - dqidw1 + dqodw1
00222         cmx(j) = dqodw2
00223     enddo
00224 
00225     !...node j=nsoil
00226     j = nsoil
00227     den    = zmm(j) - zmm(j-1)
00228     num    = smp(j) - smp(j-1) - den
00229     qin    = -hk(j-1) * num/den
00230     dqidw0 = -(-hk(j-1)*dsmpdw(j-1) + num*dhkdw(j-1))/den
00231     dqidw1 = -( hk(j-1)*dsmpdw(j)   + num*dhkdw(j-1))/den
00232     qout   = hk(j)
00233     dqodw1 = dhkdw(j)
00234     rmx(j) = qin - qout  - (sib%diag%ect * dti * sib%param%rootr(j) * hltmi)
00235     amx(j) = -dqidw0
00236     bmx(j) = dzmm(j)*dti - dqidw1 + dqodw1
00237     cmx(j) = 0.0
00238 
00239     ! Add qout out of the bottom layer to runoff
00240     sib%diag%roff = sib%diag%roff + qout*dtt
00241     
00242     !...solve
00243     call  clm_tridia (nsoil, amx, bmx, cmx, rmx, dw_liq)
00244 
00245 end subroutine soilwater
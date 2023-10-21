00001 !----------------------------------------------------------------------
00002 subroutine combine_snow(sib)
00003 !----------------------------------------------------------------------
00004 !
00005 !   Based on CLM subroutine CLM_COMBIN
00006 !
00007 !   CLM web info: http://clm.gsfc.nasa.gov
00008 !
00009 !   Description:
00010 !   This subroutine checks for elements which are below the prescribed 
00011 !   minimum for thickness or mass.  If the snow element thickness or 
00012 !   mass is less than a prescribed minimum, then it is combined with a 
00013 !   neighboring element.  The subroutine subdivide_snow then executes 
00014 !   the combination of mass and energy.
00015 !
00016 !   Revision History:
00017 !   15 September 1999: Yongjiu Dai; initial code
00018 !   15 December  1999: Paul Houser and Jon Radakovich; F90 revision
00019 !   30 January   2002: Ian Baker; SiB integration
00020 !
00021 !----------------------------------------------------------------------
00022 
00023 use kinds
00024 use sibtype
00025 
00026 use sib_const_module, only: &
00027     nsnow,  &
00028     denice, &
00029     denh2o, &
00030     dti
00031 
00032 
00033 implicit none
00034 
00035 !----------------------------------------------------------------------
00036 
00037 type(sib_t), intent(inout) :: sib
00038 
00039 !----------------------------------------------------------------------  
00040 
00041 
00042 !   local variables
00043 integer(kind=int_kind) :: i,i0,j,k,l,m  ! loop variables
00044 integer(kind=int_kind) :: nsnow_old ! copy of number of snow layers
00045 integer(kind=int_kind) :: mssi
00046 integer(kind=int_kind) :: neibor
00047 
00048 real(kind=dbl_kind) :: zwice  ! snow-total ice (kg m^-2)
00049 real(kind=dbl_kind) :: zwliq  ! snow-total liquid (kg m^-2)
00050 real(kind=dbl_kind) :: dzmin(5) 
00051 ! minimum depth for snow layers (m)
00052 !----------------------------------------------------------------------
00053 
00054 
00055 
00056 data dzmin/0.010,0.015,0.025,0.055,0.115/
00057 
00058 
00059 !    if ( sib%prog%nsl == 0 ) return
00060 
00061     nsnow_old = sib%prog%nsl
00062 
00063 
00064     do j=nsnow_old+1,0
00065 
00066 
00067         !itb...Im still not comfortable with the threshold amount to maintain
00068         !itb...a snow layer. Its been 0.1 (kg/m^2 water) in the past...
00069 
00070         if(sib%prog%www_ice(j) <= 0.05 ) then
00071 
00072             !itb...need to prevent supersaturating the soil column.
00073             if (  (sib%prog%www_ice(j)/(sib%prog%dz(j)*denice)) + &
00074                 (sib%prog%www_liq(j)/(sib%prog%dz(j)*denh2o)) &
00075                 .gt. sib%param%poros) then
00076                 sib%diag%roffo = sib%diag%roffo + &
00077                     (sib%prog%www_ice(j) + sib%prog%www_liq(j)) ! CSR kg/m2, not kg/m2/s
00078 
00079             else
00080                 sib%prog%www_liq(j+1) = sib%prog%www_liq(j+1) + &
00081                     sib%prog%www_liq(j)
00082                 sib%prog%www_ice(j+1) = sib%prog%www_ice(j+1) + &
00083                     sib%prog%www_ice(j)
00084             endif
00085 
00086             !...shift all elements above this down one.
00087 
00088             if( j > sib%prog%nsl+1 .and. sib%prog%nsl < -1) then
00089                 do k = j,sib%prog%nsl+2, -1
00090                     sib%prog%td(j)      = sib%prog%td(j-1)
00091                     sib%prog%www_liq(j) = sib%prog%www_liq(j-1)
00092                     sib%prog%www_ice(j) = sib%prog%www_ice(j-1)
00093                     sib%prog%dz(j)      = sib%prog%dz(j-1)
00094                 enddo
00095             endif
00096 
00097             sib%prog%nsl = sib%prog%nsl + 1
00098 
00099 
00100             !itb...zero out the layer that just disappeared
00101             sib%prog%td(sib%prog%nsl) = 0.0_dbl_kind
00102             sib%prog%dz(sib%prog%nsl) = 0.0_dbl_kind
00103             sib%prog%layer_z(sib%prog%nsl-1) = 0.0_dbl_kind
00104             sib%prog%node_z(sib%prog%nsl) = 0.0_dbl_kind
00105             sib%prog%www_liq(sib%prog%nsl) = 0.0_dbl_kind
00106             sib%prog%www_ice(sib%prog%nsl) = 0.0_dbl_kind
00107 
00108         endif
00109     enddo
00110 
00111 
00112     if(sib%prog%nsl == 0) then
00113 
00114         sib%prog%snow_depth = 0.0_dbl_kind
00115         sib%prog%snow_mass  = 0.0_dbl_kind
00116 
00117         sib%diag%snow_end(1) = min(sib%diag%snow_end(3),   &
00118                                      (sib%stat%julday))
00119 
00120         !...set layer values to zero
00121         do j=-nsnow+1,0
00122             sib%prog%td(sib%prog%nsl) = 0.0_dbl_kind
00123             sib%prog%dz(j)      = 0.0_dbl_kind
00124             sib%prog%layer_z(j) = 0.0_dbl_kind
00125             sib%prog%node_z(j)  = 0.0_dbl_kind
00126             sib%prog%www_liq(sib%prog%nsl) = 0.0_dbl_kind
00127             sib%prog%www_ice(sib%prog%nsl) = 0.0_dbl_kind
00128         enddo
00129 
00130         !...make sure top layer is zero-ed out also
00131         sib%prog%layer_z(-nsnow) = 0.0_dbl_kind
00132 
00133         return
00134 
00135     else
00136 
00137 
00138         sib%prog%snow_depth = 0.0_dbl_kind
00139         sib%prog%snow_mass  = 0.0_dbl_kind
00140         zwice      = 0.0_dbl_kind
00141         zwliq      = 0.0_dbl_kind
00142 
00143         do j=sib%prog%nsl+1,0
00144             sib%prog%snow_mass  = sib%prog%snow_mass + sib%prog%www_ice(j) + &
00145                 sib%prog%www_liq(j)
00146             sib%prog%snow_depth = sib%prog%snow_depth + sib%prog%dz(j)
00147             zwice      = zwice + sib%prog%www_ice(j)
00148             zwliq      = zwliq + sib%prog%www_liq(j)
00149         enddo
00150 
00151         if(sib%prog%snow_mass < 1.0) then
00152            sib%diag%snow_end(3) = min(sib%diag%snow_end(3),   &
00153                                      (sib%stat%julday))
00154         endif
00155 
00156     endif
00157 
00158 
00159     !...check the snow depth
00160 
00161     if(sib%prog%snow_depth < 1.d-6 ) then   ! all snow gone!
00162 
00163        ! CSR: now we have cleaned up the snow layers that vanished
00164        ! and put the water from these layers in capac(2)
00165         sib%prog%nsl = 0
00166         sib%prog%snow_mass = 0.0_dbl_kind
00167         sib%prog%dz(0) = 0.0_dbl_kind
00168         sib%prog%node_z(0) = 0.0_dbl_kind
00169         sib%prog%layer_z(-1) = 0.0_dbl_kind
00170         sib%prog%snow_depth = 0.0_dbl_kind
00171         sib%prog%www_liq(0) = 0.0_dbl_kind
00172         sib%prog%www_ice(0) = 0.0_dbl_kind
00173         sib%prog%td(0) = 0.0_dbl_kind
00174 
00175         !...the liquid water assumed ponding on soil surface
00176 
00177         sib%prog%capac(2) = sib%prog%capac(2) + zwliq + zwice
00178 
00179         zwliq=0.
00180         zwice=0.
00181 
00182         return
00183     else
00184 
00185         !...two or more layers
00186 
00187         if(sib%prog%nsl < -1) then
00188             nsnow_old = sib%prog%nsl
00189             mssi = 1
00190             do k = nsnow_old+1,0
00191 
00192                 !...if top node is removed, combine with bottom neighbor
00193 
00194                 if(sib%prog%dz(k) < dzmin(mssi)) then
00195                     if(k == sib%prog%nsl+1)then
00196                         neibor = k + 1
00197 
00198                         !...if the bottom neighbor is not snow, 
00199                         !...combine with the top neighbor
00200 
00201                     elseif(k == 0) then
00202                         neibor = k - 1
00203 
00204                         !...if neither of the above apply, 
00205                         !...combine with thinnest neighbor
00206 
00207                     else
00208                         neibor = k + 1
00209                         if((sib%prog%dz(k-1) + sib%prog%dz(k)) < &
00210                             (sib%prog%dz(k+1) + sib%prog%dz(k))) neibor = k-1
00211                     endif
00212 
00213                     !...node l and j are combined and stored as node j
00214 
00215                     if(neibor > k) then
00216                         j = neibor
00217                         l = k
00218                     else
00219                         j = k
00220                         l = neibor
00221                     endif
00222 
00223                     call clm_combo(sib%prog%dz(j), sib%prog%www_liq(j), &
00224                         sib%prog%www_ice(j), sib%prog%td(j), sib%prog%dz(l), &
00225                         sib%prog%www_liq(l), sib%prog%www_ice(l), sib%prog%td(l) )
00226 
00227 
00228                     !...now shift all elements above this down one
00229                     if(j-1 > sib%prog%nsl+1) then
00230                         do m= j-1, sib%prog%nsl+2, -1
00231                             sib%prog%td(m)      = sib%prog%td(m-1)
00232                             sib%prog%www_ice(m) = sib%prog%www_ice(m-1)
00233                             sib%prog%www_liq(m) = sib%prog%www_liq(m-1)
00234                             sib%prog%dz(m)      = sib%prog%dz(m-1)
00235                         enddo
00236                     endif
00237 
00238                     sib%prog%nsl = sib%prog%nsl + 1
00239                     if(sib%prog%nsl >= 1 ) cycle
00240 
00241                 else
00242 
00243                     mssi = mssi + 1
00244 
00245                 endif ! if thickness greater than minimum
00246 
00247             enddo  ! (k) snow layer loop
00248 
00249         endif ! if two or more layers condition
00250 
00251         !...reset the node depth and the depth of layer interface
00252         do j=0,sib%prog%nsl+1,-1
00253 
00254             sib%prog%node_z(j)    = sib%prog%layer_z(j) - 0.5 * sib%prog%dz(j)
00255             sib%prog%layer_z(j-1) = sib%prog%layer_z(j) - sib%prog%dz(j)
00256 
00257         enddo
00258 
00259     endif  ! what's the total depth? condition     
00260 
00261 
00262 end subroutine combine_snow
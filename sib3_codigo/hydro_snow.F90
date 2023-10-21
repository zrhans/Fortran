00001 
00002 subroutine hydro_snow(sib)
00003 
00004 !----------------------------------------------------------------------
00005 !
00006 !     Evaluate and update snow mass/snow water.
00007 !                 based on code in CLM_HYDRO_SNOW.F90
00008 !
00009 !
00010 !     the following is taken verbatim from the comments in CLM_HYDRO_SNOW
00011 !
00012 !     Description:
00013 !       Evaluate the change of snow mass and the snow water onto soil. 
00014 !     Water flow within snow is computed by an expicit and non-physical
00015 !     capacity (a tentative value is used, i.e. equal to 0.33*porosity)
00016 !     to percolate into the underlying layer.  Except for cases where 
00017 !     the porosity of one of the two neighboring layers is less than 0.05,
00018 !     zero flow is assumed. The water flow out of the bottom of the snow
00019 !     pack will participate as the input of the soil water and runoff.
00020 !
00021 !     Revision History:
00022 !      15 September 1999: Yonjiu Dai; original code
00023 !      15 December  1999: Paul Houser and Jon Radakovich; F90 revision
00024 !      15 November  2000: Mariana Vertenstein
00025 !      22 January   2002: Ian Baker - integration into SiB
00026 !----------------------------------------------------------------------
00027 
00028 use kinds
00029 use sibtype
00030 
00031 use physical_parameters, only: &
00032     tice, &
00033     hltm
00034 
00035 use sib_const_module, only: &
00036     cww,    &
00037     denice, &
00038     denh2o, &
00039     ssi,    &
00040     snomel, &
00041     dtt,    &
00042     dti
00043 
00044 
00045 implicit none
00046 
00047 !----------------------------------------------------------------------
00048 
00049 type(sib_t), intent(inout) :: sib
00050 
00051 !----------------------------------------------------------------------  
00052 
00053 
00054 !...LOCAL VARIABLES...
00055 integer(kind=int_kind) :: j,i
00056 real(kind=dbl_kind)    :: wgdif
00057 real(kind=dbl_kind)    :: qin
00058 real(kind=dbl_kind)    :: qout
00059 real(kind=dbl_kind)    :: qout_snow
00060 real(kind=dbl_kind)    :: hfus
00061 
00062 !----------------------------------------------------------------------
00063 
00064 
00065     hfus = snomel/1000.0   ! heat of fusion, units J/kg
00066 
00067 
00068     !...porosity and partial volume - recalculated
00069     if(sib%prog%nsl < 0) then
00070         do i = sib%prog%nsl + 1, 0
00071             sib%prog%vol_ice(i) = min(sib%param%poros, &
00072                 sib%prog%www_ice(i)/(sib%prog%dz(i)*denice))
00073             sib%diag%eff_poros(i) = 1.0 - sib%prog%vol_ice(i)
00074             sib%prog%vol_liq(i) = min(sib%diag%eff_poros(i), &
00075                 sib%prog%www_liq(i)/(sib%prog%dz(i)*denh2o))
00076         enddo
00077     endif
00078 
00079     if (sib%prog%nsl == 0) then  !no snow case
00080 
00081         sib%diag%www_inflow =  sib%diag%pcpg_rain
00082         ! units kg/m^2/sec 
00083 
00084     else  !snow present
00085 
00086         j = sib%prog%nsl + 1
00087 
00088 
00089         !itb...wgdif is the top snow layer ice amount, plus deposition (dew/frost) 
00090         !itb...minus sublimation. Keeping it zero for now, as a PATCH
00091         wgdif = 0.0
00092 
00093         if(wgdif < 0.0) then
00094             sib%prog%www_ice(j) = 0.0
00095             sib%prog%www_liq(j) = sib%prog%www_liq(j) + wgdif
00096         endif
00097 
00098         !itb...add rain
00099         if(sib%prog%tm >= tice) sib%prog%www_liq(j) = sib%prog%www_liq(j) &
00100             + sib%diag%pcpg_rain*dtt
00101 
00102         !itb...for the moment, not adjusting top snow layer liquid by fluxes.
00103         !        sib%prog%www_liq(j) =  sib%prog%www_liq(j) + precip + dew - evap/sublimation
00104 
00105 
00106         !itb---this directly from the comments in CLM_HYDRO_SNOW---
00107         !
00108         !...Capillary forces within snow are usually two or more orders of 
00109         !...magnitude less than those of gravity. Only gravity terms are
00110         !...considered.  The general expression for water flow is "K" * ss**3",
00111         !...however, no effective parameterization for "K".  Thus, a very
00112         !...simple consideration (not physically based) is introduced: when
00113         !...the liquid water of layer exceeds the layer's holding capacity,
00114         !...the excess meltwater adds to the underlying neighbor water.
00115         !
00116 
00117         !itb...CLM sets 0.033 as irreducible water saturation for snow (ssi)
00118         qin = 0.0
00119         do j = sib%prog%nsl+1,0
00120             sib%prog%www_liq(j) = sib%prog%www_liq(j) + qin
00121             if(j <= -1 ) then
00122                 !...no runoff over snow surface, just ponding on surface
00123                 !itb...USE SATCAP(2) HERE?
00124                 if(sib%diag%eff_poros(j)<0.05 .or. &
00125                     sib%diag%eff_poros(j+1)< 0.05) then
00126                     qout = 0.0
00127                 else
00128                     qout = max(0.0_dbl_kind,(sib%prog%vol_liq(j)- &
00129                         ssi * sib%diag%eff_poros(j)) * sib%prog%dz(j))
00130                     qout = min(qout,(1.0-sib%prog%vol_ice(j+1)- &
00131                         sib%prog%vol_liq(j+1))*sib%prog%dz(j+1))
00132                 endif
00133 
00134             else
00135                 qout = max(0.0_dbl_kind,(sib%prog%vol_liq(j)-ssi* &
00136                     sib%diag%eff_poros(j))*sib%prog%dz(j))
00137             endif
00138 
00139             qout           = qout * 1000.0
00140             sib%prog%www_liq(j) = sib%prog%www_liq(j) - qout
00141             qin            = qout
00142         enddo
00143 
00144         !itb...liquid out of the bottom of the snow into the top soil layer...
00145         qout_snow = qout*dti
00146         sib%diag%www_inflow = qout_snow
00147     endif   ! snow present/not present condition
00148 
00149     !itb...put sib%diag%www_inflow into sib%prog%capac(2) - ground interception
00150     !itb...before it is infiltrated.
00151 
00152     sib%prog%capac(2) = sib%prog%capac(2) + sib%diag%www_inflow * dtt
00153 
00154 
00155 end subroutine hydro_snow

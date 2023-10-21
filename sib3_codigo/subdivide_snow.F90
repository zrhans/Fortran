00001 !----------------------------------------------------------------------
00002 subroutine subdivide_snow(sib)
00003 !----------------------------------------------------------------------
00004 !
00005 !   Based on CLM subroutine CLM_SUBDIV
00006 !
00007 !   Description
00008 !   Subdivides snow layers if they exceed their prescribed 
00009 !   maximum thickness
00010 !
00011 !   Revision History:
00012 !   15 September 1999: Yongjiu Dai, initial code
00013 !   15 December  1999: Paul Houser and Jon Radakovich, F90 revision
00014 !   30 January   2002: Ian Baker, SiB integration
00015 !----------------------------------------------------------------------
00016 
00017 use kinds
00018 use sibtype
00019 
00020 implicit none
00021 
00022 !----------------------------------------------------------------------
00023 
00024 type(sib_t), intent(inout) :: sib
00025 
00026 !----------------------------------------------------------------------  
00027 
00028 
00029 !...local variables
00030 integer(kind=int_kind) :: msno,j
00031 real(kind=dbl_kind) :: dzsno(5)
00032 real(kind=dbl_kind) :: swice(5)
00033 real(kind=dbl_kind) :: swliq(5)
00034 real(kind=dbl_kind) :: tsnow(5)
00035 real(kind=dbl_kind) :: drr
00036 real(kind=dbl_kind) :: propor
00037 real(kind=dbl_kind) :: zwice
00038 real(kind=dbl_kind) :: zwliq
00039 
00040     !if ( sib%prog%nsl == 0 ) return
00041 
00042     msno = abs(sib%prog%nsl)
00043 
00044     do j=1,msno
00045         dzsno(j) = sib%prog%dz(j+sib%prog%nsl)
00046         swice(j) = sib%prog%www_ice(j+sib%prog%nsl)
00047         swliq(j) = sib%prog%www_liq(j+sib%prog%nsl)
00048         tsnow(j) = sib%prog%td(j+sib%prog%nsl)
00049     enddo
00050 
00051     if(msno == 1) then
00052         if(dzsno(1) > 0.03) then
00053             msno = 2
00054 
00055             !...specify a new snow layer
00056             dzsno(1) = dzsno(1)/2.0
00057             swice(1) = swice(1)/2.0
00058             swliq(1) = swliq(1)/2.0
00059 
00060             dzsno(2) = dzsno(1)
00061             swice(2) = swice(1)
00062             swliq(2) = swliq(1)
00063             tsnow(2) = tsnow(1)
00064         endif
00065 
00066     endif   ! if msno == 1 condition
00067 
00068 
00069     if(msno > 1) then
00070 
00071         if(dzsno(1) > 0.02 ) then
00072             drr      = dzsno(1) - 0.02
00073             propor   = drr/dzsno(1)
00074             zwice    = propor*swice(1)
00075             zwliq    = propor*swliq(1)
00076             propor   = 0.02/dzsno(1)
00077             swice(1) = propor*swice(1)
00078             swliq(1) = propor*swliq(1)
00079             dzsno(1) = 0.02
00080 
00081 
00082             call clm_combo(dzsno(2),swliq(2),swice(2),tsnow(2),         &
00083                 drr,zwliq,zwice,tsnow(1))
00084 
00085 
00086             if(msno <= 2  .AND. dzsno(2) > 0.07 ) then
00087 
00088                 !...subdivide a new layer
00089                 msno = 3
00090                 dzsno(2) = dzsno(2)/2.0
00091                 swice(2) = swice(2)/2.0
00092                 swliq(2) = swliq(2)/2.0
00093                 dzsno(3) = dzsno(2)
00094                 swice(3) = swice(2)
00095                 swliq(3) = swliq(2)
00096                 tsnow(3) = tsnow(2)
00097             endif
00098         endif     ! if dzsno(1) > 0.02 condition
00099     endif       ! if msno > 1 condition
00100 
00101 
00102     if(msno > 2) then
00103         if(dzsno(2) > 0.05) then
00104 
00105             drr      = dzsno(2) - 0.05
00106             propor   = drr/dzsno(2)
00107             zwice    = propor*swice(2)
00108             zwliq    = propor*swliq(2)
00109             propor   = 0.05/dzsno(2)
00110             swice(2) = propor*swice(2)
00111             swliq(2) = propor*swliq(2)
00112             dzsno(2) = 0.05
00113 
00114             call clm_combo(dzsno(3),swliq(3),swice(3),tsnow(3),         &
00115                 drr,zwliq,zwice,tsnow(2))
00116 
00117 
00118 
00119             if(msno <= 3  .AND.  dzsno(3) > 0.18) then
00120 
00121                 !...subdivide a new layer
00122                 msno = 4
00123                 dzsno(3) = dzsno(3)/2.0
00124                 swice(3) = swice(3)/2.0
00125                 swliq(3) = swliq(3)/2.0
00126                 dzsno(4) = dzsno(3)
00127                 swice(4) = swice(3)
00128                 swliq(4) = swliq(3)
00129                 tsnow(4) = tsnow(3) 
00130             endif
00131         endif    ! if dzsno(2) > 0.05 condition
00132     endif      ! if msno > 2 condition
00133 
00134 
00135     if(msno > 3) then
00136         if(dzsno(3) > 0.11) then
00137 
00138             drr      = dzsno(3) - 0.11
00139             propor   = drr/dzsno(3)
00140             zwice    = propor*swice(3)
00141             zwliq    = propor*swliq(3)
00142             propor   = 0.11/dzsno(3)
00143             swice(3) = propor*swice(3)
00144             swliq(3) = propor*swliq(3)
00145             dzsno(3) = 0.11
00146 
00147             call clm_combo(dzsno(4),swliq(4),swice(4),tsnow(4),         &
00148                 drr,zwliq,zwice,tsnow(3))
00149 
00150 
00151             if(msno <= 4  .AND.  dzsno(4) > 0.41) then
00152 
00153                 !...subdivide a new layer
00154                 msno = 5
00155                 dzsno(4) = dzsno(4)/2.0
00156                 swice(4) = swice(4)/2.0
00157                 swliq(4) = swliq(4)/2.0
00158                 dzsno(5) = dzsno(4)
00159                 swice(5) = swice(4)
00160                 swliq(5) = swliq(4)
00161                 tsnow(5) = tsnow(4)
00162             endif
00163         endif    ! if dzsno(3) > 0.11 condition
00164     endif      ! if msno > 3 condition
00165 
00166 
00167     if(msno > 4) then
00168         if(dzsno(4) > 0.23) then
00169             drr      = dzsno(4) - 0.23
00170             propor   = drr/dzsno(4)
00171             zwice    = propor*swice(4)
00172             zwliq    = propor*swliq(4)
00173             propor   = 0.23/dzsno(4)
00174             swice(4) = propor*swice(4)
00175             swliq(4) = propor*swliq(4)
00176             dzsno(4) = 0.23
00177 
00178             call clm_combo(dzsno(5),swliq(5),swice(5),tsnow(5),         &
00179                 drr,zwliq,zwice,tsnow(4))
00180 
00181 
00182 
00183         endif    ! if dzsno(4) > 0.23 condition
00184     endif      ! if msno > 4 condition
00185 
00186     sib%prog%nsl = -msno
00187 
00188     do j=sib%prog%nsl+1,0
00189         sib%prog%dz(j) = dzsno(j - sib%prog%nsl)
00190         sib%prog%www_ice(j) = swice(j - sib%prog%nsl)
00191         sib%prog%www_liq(j) = swliq(j - sib%prog%nsl)
00192         sib%prog%td(j)      = tsnow(j - sib%prog%nsl)
00193     enddo
00194 
00195     do j=0,sib%prog%nsl+1,-1
00196 
00197         sib%prog%node_z(j) = sib%prog%layer_z(j) - 0.5 * sib%prog%dz(j)
00198         sib%prog%layer_z(j-1) = sib%prog%node_z(j) - 0.5*sib%prog%dz(j)
00199 
00200     enddo
00201 
00202 
00203 end subroutine subdivide_snow
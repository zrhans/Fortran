00001 !==================SUBROUTINE NETRAD====================================
00002 subroutine netrad (sib,sib_loc)
00003 
00004     use kinds
00005     use sibtype
00006     use physical_parameters, only: &
00007         stefan, &
00008         tice
00009 
00010     implicit none
00011 
00012     !----------------------------------------------------------------------
00013 
00014     type(sib_t), intent(inout) :: sib
00015 
00016     type(sib_local_vars)     ,intent(inout) :: sib_loc
00017     ! variables local to SiB
00018 
00019     !----------------------------------------------------------------------  
00020     !
00021     !=======================================================================
00022     !
00023     !                                                                       
00024     !        CALCULATE RADT USING RADIATION FROM PHYSICS AND CURRENT        
00025     !        LOSSES FROM CANOPY AND GROUND                                  
00026     !
00027     !
00028     !=======================================================================
00029     !
00030     !... bands in sib: 0.2 to 0.7 microns are VIS, then 
00031     !...               0.7 to 4.0 is NIR, above 4.0 it is thermal
00032 
00033     !++++++++++++++++++++++++++++++OUTPUT+++++++++++++++++++++++++++++++++++
00034     !
00035     !       RADt (2)       SUM OF ABSORBED RADIATIVE FLUXES (W M-2) 
00036     !
00037     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
00038 
00039 
00040     !Bio...LOCAL VARIABLES
00041     real(kind=dbl_kind) :: 
00042         tc4,      ! canopy temp **4
00043         tg4,      ! ground temp **4
00044         ts4,      ! snow temp **4
00045         radtbar,  ! bulk weighted net rad
00046         feedfac,  ! feedback factor
00047         zlwup      ! total thermal rad up from sfc (W/m^2)
00048 
00049     real(kind=dbl_kind),dimension(1) :: ppl,ttl,qsatst
00050     ! holder arrays to make SGI compiler happy
00051     ! for calls 
00052 
00053     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
00054 
00055 
00056     data feedfac/1.0/
00057 
00058 !    if(sib%prog%nsl < 0) then
00059 !        sib%diag%tsnow = sib%prog%td(sib%prog%nsl+1)
00060 !    else
00061 !        sib%diag%tsnow = min(sib%prog%td(1),tice)
00062 !    endif
00063 
00064     tc4 = sib%prog%tc**4
00065     tg4 = sib%prog%td(sib%prog%nsl+1)**4
00066     ts4 = sib%prog%td(sib%prog%nsl+1)**4
00067 
00068     !...effective ground cove3r for thermal radiation
00069     sib_loc%fac1 = sib%param%vcover * ( 1.-sib%diag%thermk )
00070 
00071     !...derivatives
00072     sib_loc%dtc4 = 4*stefan * sib%prog%tc**3
00073     sib_loc%dtg4 = 4*stefan * sib%prog%td(sib%prog%nsl+1)**3
00074     sib_loc%dts4 = 4*stefan * sib%prog%td(sib%prog%nsl+1)**3
00075 
00076     !...canopy leaves thermal radiation loss
00077     sib_loc%closs =  2. * sib_loc%fac1 * stefan * tc4
00078 
00079     if (sib%prog%nsl == 0) then
00080        sib_loc%closs =  sib_loc%closs - sib_loc%fac1 * stefan * tg4
00081     else
00082        sib_loc%closs =  sib_loc%closs - sib_loc%fac1 * stefan * ts4
00083     endif
00084 
00085     !...ground thermal radiation loss 
00086     sib_loc%gloss =  stefan * tg4 - sib_loc%fac1 * stefan * tc4
00087 
00088     !...snow thermal radiation loss 
00089     sib_loc%sloss =  stefan * ts4 - sib_loc%fac1 * stefan * tc4
00090 
00091     !...canopy leaves net radiation
00092     sib%diag%radt(1) = sib%diag%radc3(1) - sib_loc%closs
00093 
00094     !...ground net radiation
00095     sib%diag%radt(2) = sib%diag%radc3(2) - sib_loc%gloss
00096 
00097     !...snow net radiation 
00098     sib%diag%radt(3) = sib%diag%radc3(2) - sib_loc%sloss
00099 
00100     !...bulk, weighted net radiation from combined ground and snow
00101     if (sib%prog%nsl == 0) then
00102        radtbar = sib%diag%radt(2)
00103     else
00104        radtbar = sib%diag%radt(3)
00105     endif
00106 
00107     !...this is the exchange meant to help out exchanges between
00108     !...ground and snow
00109     sib%diag%radt(2) = radtbar + (1.+feedfac)*(sib%diag%radt(2)-radtbar)
00110     sib%diag%radt(3) = radtbar + (1.+feedfac)*(sib%diag%radt(3)-radtbar)
00111 
00112     !...total thermal radiation up from surface
00113     if (sib%prog%nsl == 0) then
00114        zlwup = sib_loc%fac1 * tc4 + (1.-sib_loc%fac1) * tg4
00115     else
00116        zlwup = sib_loc%fac1 * tc4 + (1.-sib_loc%fac1) * ts4
00117     endif
00118 
00119 
00120     !...effective (combined) skin temperature from surface thermal radiation
00121     sib%diag%tgeff =  zlwup ** 0.25
00122 
00123     !...potential skin temp 
00124     sib%diag%thgeff = sib%diag%tgeff / sib%prog%bps(1)
00125 
00126     !itb...use a call to eau_sat instead of vnqsat
00127     !...have to make SGI compiler happy with array arguments, not scalars...
00128 
00129     ppl(1) = sib%prog%ps*100.0
00130     ttl(1) = sib%diag%thgeff
00131 
00132     call qsat_eau(1,ppl,ttl,qsatst)
00133 
00134     sib%diag%shgeff = qsatst(1)
00135 
00136 
00137 end subroutine netrad
00001 !----------------------------------------------------------------------
00002 subroutine respsib(sib)
00003 !----------------------------------------------------------------------
00004 ! respsib calculates ground respiration based on annual balance between 
00005 ! canopy net assimilation and respiration [Denning et al., 1996].
00006 ! Instantaneous ground respiration using the "R-star" approach of
00007 ! Denning et al (1996) scaled by soil temperature and moisture [Raich et al., 1991]
00008 ! and adapted for use with the Bonan/CLM 10-layer soil thermodynamics module.
00009 !
00010 ! References:
00011 !   Raich et al. [1991) Ecological Applications 1(4) pp 399-429 
00012 !                     appendix I - Terrestrial Ecosystem Model
00013 !   Denning, A.S., G.J. Collatz, C. Zhang,D.A. Randall, J.A. Berry, 
00014 !                     P.J. Sellers, G.D. Colello D.A. Dazlich, 1996: 
00015 !                     Simulations of Terrestrial Carbon Metabolism and 
00016 !                     Atmospheric CO2 in  a General Circulation Model. 
00017 !                     Part 1: Surace Carbon Fluxes. Tellus, 48B, 521-542.
00018 ! Modifications:
00019 !   Scott Denning (9/14/95) changed soil Q10 value for soil respiration from 2.0 to 2.4
00020 !      following Raich and Schelsinger (1992, Tellus 44B, 81-89)
00021 !   Kevin Schaefer (8/4/2004) changed wetness exponent to use soil moisture 
00022 !      fraction rather than percent to minimize cpu time
00023 !   Kevin Schaefer (8/4/2004) changed wetness exp to use wfrac rather than www_liq
00024 !   Lara Prihodko changed moist to use wet_exp for each layer (10/28/04)
00025 !   Kevin Schaefer added autotrophic respiration (5/6/05)
00026 !   Kevin Schaefer moved woptzm to read_ti routine (5/6/05)
00027 
00028     use kinds
00029     use sibtype
00030     use sib_const_module, only : denh2o,denice,ca_q10
00031 
00032     implicit none    
00033 
00034     type(sib_t), intent(inout) :: sib
00035 
00036 ! LOCAL VARIABLES
00037     integer:: j
00038     real(kind=dbl_kind),dimension(nsoil) :: 
00039         wet_exp,  ! wetness exponent (b in eqn 8 from Denning et al [1996])
00040         wfrac,    ! soil moisture fraction of saturation for soil layer
00041         moist      ! soil moisture scaling factor for resp (eqn 1.14b from Raich et al [1991])
00042 
00043 !itb_COS
00044     real(kind=dbl_kind) :: ca_slope = -1.0_dbl_kind
00045     real(kind=dbl_kind) :: ca_moist
00046 
00047 
00048 !
00049 ! initialize heterotrophic respiration (resp_het) to zero
00050     sib%diag%resp_het = 0.0
00051 !
00052 ! loop through soil layers to calculate heterotrophic resp
00053     do j=1,nsoil
00054 !
00055 ! calculate soil moisture fraction of saturation
00056         wfrac(j) = sib%prog%www_liq(j) / (sib%prog%dz(j) * sib%param%poros * denh2o) !  &
00057 !                 + sib%prog%www_ice(j) / (sib%prog%dz(j) * sib%param%poros * denice)
00058 !        print*,'wfrac=',j,wfrac(j)
00059 !
00060 ! calculate wetness exponent
00061         wet_exp(j) = ((wfrac(j)**sib%param%zm-sib%param%woptzm)/(1.-sib%param%woptzm))**2
00062         wet_exp(j) = min(wet_exp(j),10.0_dbl_kind)
00063 !
00064 ! calculate soil moisture respiration scaling factor
00065         moist(j) = 0.8*sib%param%wsat**wet_exp(j) + 0.2
00066 !
00067 ! calculate soil temperature respiration scaling factor
00068         sib%diag%soilQ10(j) = exp(0.087547 * (sib%prog%td(j) - 298.15))
00069 !
00070 ! calculate total soil respiration scaling factor
00071         sib%diag%soilscale(j) = sib%diag%soilQ10(j) * moist(j)
00072 
00073 !itb_COS
00074         if(j < 4) then   !only look at top 3 layers
00075            ca_moist = 1.0 - wfrac(j)
00076            sib%diag%ca_soilscale(j) = exp(ca_q10 *    &
00077                      (sib%diag%ts3_mean(j) - 298.15)) * ca_moist
00078 
00079 !           print*,'ca_ss:',j,sib%diag%ca_soilscale(j)
00080         endif
00081 !itb_cos...end
00082 
00083 
00084 !
00085 ! calculate soil layer respiration; add to total ground respiration
00086         sib%diag%resp_het = sib%diag%resp_het + sib%param%het_respfac(j) *   &
00087             sib%diag%soilscale(j)
00088 
00089     enddo  ! soil layers
00090 !
00091 ! autotrophic respiration
00092     sib%diag%resp_auto = sib%param%auto_respfac * sib%param%aparc
00093 !
00094 ! ground respiration
00095     sib%diag%resp_grnd = sib%diag%resp_auto + sib%diag%resp_het
00096 !
00097 ! total respiration
00098     sib%diag%resp_tot = sib%diag%resp_grnd + sib%diag%resp_can(6)
00099 
00100 end subroutine respsib
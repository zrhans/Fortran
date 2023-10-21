00001 !------------------------------------------------------------------------------
00002 
00003 module eau_params
00004       
00005 use kinds
00006 use physical_parameters,  cpelq   => spec_heat_cp, &
00007                           gravelq => grav,         &
00008                           lcond   => hltm
00009 
00010 implicit none
00011 save
00012       
00013 !------------------------------------------------------------------------------
00014 !contains the constants needed for the eau_cup and eau_liq parameterizations of
00015 !convection and cloud microphysics.
00016 !Laura D. Fowler/slikrock (10-01-99).
00017 
00018 !send comments to laura@atmos.colostate.edu.
00019 
00020 !references:
00021 !Fowler, L.D, D.A. Randall, and S.A. Rutledge, 1996: Liquid and Ice Cloud Micro
00022 !physics in the CSU General Circulation Model: Model description and simulated
00023 !cloud microphysical processes.
00024 !J. Climate, 9, 489-529.
00025 
00026 !Randall, D.A., and L.D. Fowler, 1999: EAUliq: The next generation. Dept. of
00027 !Atmospheric Science Paper 673, Dept. of Atmospheric Science. Colorado State
00028 !University, Fort Collins, Colorado, 65 pp.
00029 !------------------------------------------------------------------------------
00030 
00031 !eau_cup parameters:
00032 
00033 integer (kind=int_kind), parameter:: 
00034    ltpcup = 1,                      !index of highest convective cloud top.
00035    ncb    = 2                        !index of highest convective cloud base.
00036 
00037 real (kind=dbl_kind), parameter:: 
00038    alpham = 1.e+08_dbl_kind,        !
00039    ckemin = 5.0_dbl_kind,           !minimum value of cke.
00040    taudis = 600.0_dbl_kind,         !cke dissipation time scale.
00041    amiu   = 1.0_dbl_kind             !cloud-top entrainment.
00042      
00043 !eau_liq parameters:
00044 
00045 integer (kind=int_kind), parameter:: 
00046    nc1elq = 3                        !number of microphysics time-steps per
00047                                      !dynamical time-step.
00048 
00049 real (kind=dbl_kind), parameter:: 
00050    a0elq    = -.267_dbl_kind,       !
00051    a1elq    = 5.15e+03_dbl_kind,    !
00052    a2elq    = -1.0225e+06_dbl_kind, !
00053    a3elq    = 7.55e+07_dbl_kind,    !
00054    alphaelq = 0.001_dbl_kind,       !
00055    aprime   = 3.e+03_dbl_kind,      !
00056    asecond  = 1.139_dbl_kind,       !
00057    belq     = 0.11_dbl_kind,        !
00058    betaelq  = 0.001_dbl_kind,       !
00059    diffelq  = 2.26e-05_dbl_kind,    !
00060    erccoef  = 1._dbl_kind,          !
00061    esccoef  = 1._dbl_kind,          !
00062    esicoef  = 0.1_dbl_kind,         !
00063    gam3     = 2._dbl_kind,          !
00064    gams1    = 2.21891_dbl_kind,     !
00065    gams2    = 1.38784_dbl_kind,     !
00066    gams3    = 6.90080_dbl_kind,     !
00067    kap      = .2861328125_dbl_kind, !
00068    muelq    = 1.718e-05_dbl_kind,   !
00069    nzeror   = 8.e+06_dbl_kind,      !
00070    nzeros   = 2.e+07_dbl_kind,      !
00071    pielq    = 3.14159265,           !
00072    pzero    = 1.e+05_dbl_kind,      !
00073    qci0     = 0.01e-03_dbl_kind,    !
00074 !  qci0     = 0.1e-03_dbl_kind,     &!
00075    qcw0     = 0.25e-03_dbl_kind,    &!
00076 !  qcw0     = 0.7e-03_dbl_kind,     &!
00077    rhor     = 1.e03_dbl_kind,       &!
00078    rhos     = 1.e02_dbl_kind,       &!
00079    taul     = 100._dbl_kind,        &!
00080    tauf     = 100._dbl_kind,        &!
00081    therco   = 2.43e-02_dbl_kind      !
00082       
00083 !shared eau_liq and eau_cup parameters:
00084 
00085 real (kind=dbl_kind), parameter:: 
00086    eauc0    = 0.0_dbl_kind,         !
00087    eauc1    = 1.0_dbl_kind           !
00088 
00089 real (kind=dbl_kind), parameter:: 
00090    lfus     = 0.3336e+06_dbl_kind,  !
00091    lsub     = lcond+lfus,           !
00092    t00      = 273.15_dbl_kind,      !
00093    tbgmin   = 253.15_dbl_kind,      !
00094    tbgmax   = 273.15_dbl_kind        !
00095 
00096 !#ifdef eau_ng
00097 !eau_ng parameters:
00098 !real (kind=dbl_kind), parameter:: &
00099 !   cClrCld  = eauc1,                &!exchange terms between cld and clr.
00100 !   cCldClr  = eauc1                  !exchange terms between cld and clr.
00101 !#endif
00102 
00103 
00104 end module eau_params
00105 
00106 !------------------------------------------------------------------------------
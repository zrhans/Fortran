00001 !==================SUBROUTINE RBRD=======================================
00002 subroutine rbrd(sib,u2)
00003 
00004 use kinds
00005 use sibtype
00006 use physical_parameters, only: grav    
00007 
00008 implicit none
00009 
00010 !----------------------------------------------------------------------
00011 
00012 type(sib_t), intent(inout) :: sib
00013 
00014 !----------------------------------------------------------------------  
00015 
00016 !      Reference
00017 
00018 !      Sellers, P.J. and Mintz, Y., Y.C. Sud, A. Dalcher, 1986: A Simple 
00019 !                     Biospher Model (SiB) for use Within General 
00020 !                     Circulation Models. JAS, 43(6),505-531.
00021 
00022 !========================================================================
00023 !
00024 !      CALCULATION OF RB AND RD AS FUNCTIONS OF U2 AND TEMPERATURES
00025 !
00026 !======================================================================== 
00027 
00028 
00029 
00030 !++++++++++++++++++++++++++++++OUTPUT+++++++++++++++++++++++++++++++++++
00031 !
00032 !       RB (GRB)       CANOPY TO CAS AERODYNAMIC RESISTANCE (Sec M-1)
00033 !       RD (GRD)       GROUND TO CAS AERODYNAMIC RESISTANCE (Sec M-1)
00034 !
00035 !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
00036 
00037 !...input variable
00038 
00039 real(kind=dbl_kind) :: u2
00040 
00041 integer(kind=int_kind) :: i0
00042 
00043 !Bio...LOCAL VARIABLES
00044 real(kind=dbl_kind) :: temdif    
00045 ! vegetation-CAS temperature difference (K)
00046 
00047 real(kind=dbl_kind) :: fac    ! factor for vegetation-CAS resistance calc
00048 real(kind=dbl_kind) :: fih    ! factor for ground-CAS resistance calc
00049 !real(kind=dbl_kind) :: tgs    ! composite soil/snow sfc temperature
00050 
00051 !    tgs = (1.0_dbl_kind - sib%diag%areas) * sib%prog%td(1) +  &
00052 !        sib%diag%areas * sib%prog%td(sib%prog%nsl+1)
00053 
00054     !-----------------------------------------------------------------------
00055     !      RB       (RB)       : EQUATION (A9), SE-86
00056     !-----------------------------------------------------------------------
00057 
00058     temdif  = MAX( 0.1_dbl_kind,  sib%prog%tc - sib%prog%ta)
00059     fac     = sib%param%zlt / 890.* (temdif * 20.0)**0.25
00060 
00061     sib%diag%rb  = 1.0 / (SQRT(u2) / sib%param%rbc+fac)
00062 
00063     !-----------------------------------------------------------------------
00064     !      RD       (RD)       : EQUATION (A15), SE-86
00065     !-----------------------------------------------------------------------
00066 
00067     !itb...for rd, we use the composite soil/snow temperature
00068 !    tgs = sib%diag%areas*sib%prog%td(sib%prog%nsl+1) +   &
00069 !        (1.0 - sib%diag%areas)*sib%prog%td(1)
00070     temdif = MAX( 0.1_dbl_kind, sib%prog%td(sib%prog%nsl+1)-sib%prog%ta )
00071 
00072     fih = SQRT( 1.+9.* grav * temdif * sib%param%z2 / (sib%prog%td(sib%prog%nsl+1)*u2*u2) )
00073     
00074     sib%diag%rd  = sib%param%rdc / (u2 * fih) 
00075 
00076 end subroutine rbrd
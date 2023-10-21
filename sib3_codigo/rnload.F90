00001 !==================SUBROUTINE RNLOAD====================================
00002 subroutine rnload(sib)
00003 
00004     use kinds
00005     use sibtype
00006 
00007     implicit none
00008 
00009     !----------------------------------------------------------------------
00010 
00011     type(sib_t), intent(inout) :: sib
00012 
00013     !----------------------------------------------------------------------  
00014 
00015     !
00016     !=======================================================================
00017     !
00018     !    calculation of absorption of radiation by surface.  Note that
00019     !       output from this calculation (radc3) only accounts for the 
00020     !       absorption of incident longwave and shortwave fluxes.  The
00021     !       total net radiation calculation is performed in subroutine
00022     !       netrad.
00023     !
00024     !=======================================================================
00025     !
00026 
00027     !++++++++++++++++++++++++++++++OUTPUT+++++++++++++++++++++++++++++++++++
00028     !
00029     !       RADN(2,3)      INCIDENT RADIATION FLUXES (W M-2)
00030     !       RADC3(2)       SUM OF ABSORBED RADIATIVE FLUXES (W M-2) 
00031     !
00032     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
00033 
00034 
00035 
00036     integer(kind=int_kind) :: i, iveg, iwave, irad
00037     real(kind=dbl_kind)    :: radn(2,2)
00038 
00039     !-----------------------------------------------------------------------
00040     !     CALCULATION OF SOIL MOISTURE STRESS FACTOR.
00041     !     AVERAGE SOIL MOISTURE POTENTIAL IN ROOT ZONE (LAYER-2) USED AS
00042     !     SOURCE FOR TRANSPIRATION.
00043     !
00044     !      RADN        (F(IW,IMU,O)) : EQUATION (19-22) , SE-86
00045     !      RADC3       (FC,FGS)      : EQUATION (21,22) , SE-86
00046     !-----------------------------------------------------------------------
00047 
00048 
00049     sib%diag%radc3(1) = 0.
00050     sib%diag%radc3(2) = 0.
00051     radn(1,1) = sib%prog%radvbc
00052     radn(1,2) = sib%prog%radvdc
00053     radn(2,1) = sib%prog%radnbc
00054     radn(2,2) = sib%prog%radndc
00055 
00056     do iveg=1,2
00057         do iwave=1,2
00058             do irad=1,2
00059                 sib%diag%radc3(iveg) = sib%diag%radc3(iveg) +  &
00060                     sib%diag%radfac(iveg,iwave,irad) *         &
00061                     radn(iwave,irad)
00062             enddo
00063         enddo
00064     enddo
00065 
00066     !...absorb downwelling radiation 
00067 
00068     sib%diag%radc3(1) = sib%diag%radc3(1) + sib%prog%dlwbot *   &
00069         sib%param%vcover * (1.- sib%diag%thermk)
00070     sib%diag%radc3(2) = sib%diag%radc3(2) + sib%prog%dlwbot *   &
00071         (1.-sib%param%vcover * (1.-sib%diag%thermk))
00072 
00073 end subroutine rnload
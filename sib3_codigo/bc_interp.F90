 !-------------------------------------------------------------------------------
 subroutine bc_interp( sib, time )
 !-------------------------------------------------------------------------------
 ! interpolates between bc data points
 !
 ! Modifications:
 !  Kevin Schaefer created subrotine from bc_update_sib code (3/14/03)
 !  Kevin Schaefer removed print statements (3/17/03)
 !  Kevin Schaefer moved isotope interpolation to separate routine (2/21/03)
 !  Kevin Schaefer deleted LAI underflow patch, already done in laigrn (4/4/03)
 !  Kevin Schaefer added ndvi interpolation (7/9/03)
 !  Kevin Schaefer changed ndvi variable names to match bc convention (3/15/05)
 !         prevndvi to ndvi1  curndvi to ndvi2 
 !  Kevin Schaefer changed to generic interpolation (8/2/05)
 !-----------------------------------------------------------------------
 !
 use sibtype
 use timetype
 use sib_const_module
 use sib_bc_module
 !
 implicit none
 !
 ! parameters
 type(sib_t), dimension(subcount), intent(inout) :: sib
 type(time_struct), intent(in) :: time
 
 ! local variables
 integer i, k  ! indices
 real fac1    ! scaling factor between ndvi1 and current time
 real fac2    ! scaling factor between ndvi2 and current time
 real time1   ! (day) local version of ndvi1 time
 real time2   ! (day) local version of ndvi2 time
 logical(kind=log_kind) :: switch_param      ! switch parameters for single point
 !
 ! loop through sib points
     do i = 1, subcount
 !
 !itb_modis
 ! switch to local ndvi times
 !      time1=sib(i)%param%ndvi_time1
 !      time2=sib(i)%param%ndvi_time2
 
       time1=sib(i)%param%modis_time1
       time2=sib(i)%param%modis_time2
 
 ! interpolation times in different years
 
 !      if (sib(i)%param%ndvi_time1>sib(i)%param%ndvi_time2) then 
       if (sib(i)%param%modis_time1>sib(i)%param%modis_time2) then 
         ! time1 in previous year
 !        if(time%real_doy<=time%ndvi_stop(1)) then
         if(time%real_doy<=time%modis_stop(1)) then
 !          time1=sib(i)%param%ndvi_time1-real(time%days_per_year)
           time1=sib(i)%param%modis_time1-real(time%days_per_year)
         endif
         ! time2 in next year
 !        if(time%real_doy>=time%ndvi_start(nper))then
         if(time%real_doy>=time%modis_start(nper))then
 !          time2=sib(i)%param%ndvi_time2+real(time%days_per_year)
           time2=sib(i)%param%modis_time2+real(time%days_per_year)
         endif
       endif
       if(time1==time2) then
         print'(a,i5,2f10.3)', '(bc_interp)Seatbelts on, we gonna crash!', i,time1,time2
       endif
 !
 ! Calculate scaling factors for parameter interpolation
       fac2=(time%real_doy-time1)/(time2-time1)
       fac1=1.0-fac2
 !
 ! interpolate parameters
 !        sib(i)%param%ndvi     = fac1*sib(i)%param%ndvi1     + fac2*sib(i)%param%ndvi2
         sib(i)%param%mlai     = fac1*sib(i)%param%mlai1     + fac2*sib(i)%param%mlai2
 !print*,'MODIS:',sib(i)%param%mlai3,sib(i)%param%mfpar3
         sib(i)%param%mfpar    = fac1*sib(i)%param%mfpar1    + fac2*sib(i)%param%mfpar2
         sib(i)%param%aparc    = fac1*sib(i)%param%aparc1    + fac2*sib(i)%param%aparc2
         sib(i)%param%zlt      = fac1*sib(i)%param%zlt1      + fac2*sib(i)%param%zlt2
         sib(i)%param%green    = fac1*sib(i)%param%green1    + fac2*sib(i)%param%green2
         sib(i)%param%z0d      = fac1*sib(i)%param%z0d1      + fac2*sib(i)%param%z0d2
         sib(i)%param%zp_disp  = fac1*sib(i)%param%zp_disp1  + fac2*sib(i)%param%zp_disp2
         sib(i)%param%cc1      = fac1*sib(i)%param%rbc1      + fac2*sib(i)%param%rbc2
         sib(i)%param%cc2      = fac1*sib(i)%param%rdc1      + fac2*sib(i)%param%rdc2
         sib(i)%param%gmudmu   = fac1*sib(i)%param%gmudmu1   + fac2*sib(i)%param%gmudmu2
         sib(i)%param%d13cresp = fac1*sib(i)%param%d13cresp1 + fac2*sib(i)%param%d13cresp2
 
  
         do k=1,physmax
            sib(i)%param%physfrac(k) = fac1*sib(i)%param%physfrac1(k) +    &
                                    fac2*sib(i)%param%physfrac2(k)
         enddo
 
 !if(i==840) then
 !      print*, time1                   , time%real_doy          , time2
 !      print*, sib(i)%param%ndvi1      , sib(i)%param%ndvi      , sib(i)%param%ndvi2
 !      print*, sib(i)%param%aparc1     , sib(i)%param%aparc     , sib(i)%param%aparc2
 !      print*, sib(i)%param%zlt1       , sib(i)%param%zlt       , sib(i)%param%zlt2
 !      print*, sib(i)%param%green1     , sib(i)%param%green     , sib(i)%param%green2
 !      print*, sib(i)%param%z0d1       , sib(i)%param%z0d       , sib(i)%param%z0d2
 !      print*, sib(i)%param%zp_disp1   , sib(i)%param%zp_disp   , sib(i)%param%zp_disp2
 !      print*, sib(i)%param%rbc1       , sib(i)%param%cc1       , sib(i)%param%rbc2
 !      print*, sib(i)%param%rdc1       , sib(i)%param%cc2       , sib(i)%param%rdc2
 !      print*, sib(i)%param%gmudmu1   , sib(i)%param%gmudmu    , sib(i)%param%gmudmu2
 !      print*, sib(i)%param%d13cresp1 , sib(i)%param%d13cresp  , sib(i)%param%d13cresp2
 !endif
 
     enddo
 !
 end subroutine bc_interp
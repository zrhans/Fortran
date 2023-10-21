00001 subroutine sibdrv_read_single( sib, time )
00002 !
00003 ! Modifications:
00004 !  Kevin Schaefer moved conversion from pascals to millibars from sibdrv_interp to here (8/16/04)
00005 
00006 use kinds
00007 use sibtype
00008 use timetype
00009 use sib_const_module
00010 use sib_io_module
00011 implicit none
00012 
00013 ! parameters
00014 type(sib_t), dimension(subcount), intent(inout) :: sib ! NOTE: subcount should = 1
00015 type(time_struct), intent(in) :: time
00016 
00017 ! local variables
00018 integer(kind=int_kind) :: i
00019 integer(kind=int_kind) :: status
00020 real(kind=dbl_kind) :: yr, doy, hr,dy
00021 real(kind=dbl_kind) :: temp_dpt  ! dew point
00022 character(len=256) :: filename
00023 character(len=13) :: subname
00024 character(len=1025) :: record
00025 real, parameter :: nodata = -9999.
00026 
00027 data subname/'sibdrv_read '/
00028 
00029    !*** Storing previous time steps data
00030     do i=1,subcount
00031         sib(i)%prog%ps1       = sib(i)%prog%ps2
00032         sib(i)%prog%tm1       = sib(i)%prog%tm2
00033         sib(i)%prog%tcc1      = sib(i)%prog%tcc2
00034         sib(i)%prog%sh1       = sib(i)%prog%sh2
00035         sib(i)%prog%spdm1     = sib(i)%prog%spdm2
00036         sib(i)%prog%lspr1     = sib(i)%prog%lspr2
00037         sib(i)%prog%cupr1     = sib(i)%prog%cupr2
00038         sib(i)%prog%dlwbot1   = sib(i)%prog%dlwbot2
00039         sib(i)%prog%sw_dwn1   = sib(i)%prog%sw_dwn2
00040     enddo
00041 
00042     ! switch files if needed
00043     if ( time%switch_driver ) then
00044         close( 87, iostat = status )
00045               
00046 
00047         write(unit=filename,fmt=dr_format)time%driver_year, time%driver_month
00048 
00049         open( unit=87, file=trim(filename), form='formatted', iostat=status)
00050         if ( status > 0 ) then
00051             print *, 'SiBDRV_read_single'
00052             print *, 'Error opening file'
00053             stop
00054         endif
00055     endif
00056     
00057     ! read one line of driver data from file
00058 !    print *,'SiBDRV_init_std'
00059 !    print *,'opening drive files for ',time%sec_day
00060 !    print *, 'dr_form=',trim(dr_format)
00061 !    print *, 'filename=',trim(filename)
00062     do i = 1, time%driver_recnum
00063         do  ! Read until not a comment.
00064             read( 87,'(a)', iostat=status ) record
00065             if ( status > 0 ) then
00066                 print *, 'SiBDRV_read_single'
00067                 print *, 'Error reading file'
00068                 stop
00069             endif
00070             if ( record(1:1) .ne. '#' ) exit
00071         enddo
00072         read(unit=record,fmt=*)yr,doy,hr,sib(1)%prog%tm2,temp_dpt, &
00073             sib(1)%prog%spdm2,sib(1)%prog%ps2,sib(1)%prog%dlwbot2,   &
00074             sib(1)%prog%sw_dwn2,sib(1)%prog%lspr2,sib(1)%prog%cupr2
00075 
00076 
00077 
00078 ! calculate large scale precipitation
00079         sib(1)%prog%lspr2 = sib(1)%prog%lspr2 - sib(1)%prog%cupr2
00080 !
00081 ! KS comvert from pascals to millibars
00082         sib(1)%prog%ps2=sib(1)%prog%ps2*0.01
00083 !
00084 ! KS convert dew point to specific humidity
00085         call qsat_eau(1,sib%prog%ps2*100.0,temp_dpt,sib%prog%sh2)
00086 
00087 ! check for nodata values
00088         if (sib(1)%prog%tm2.eq.nodata) then
00089            sib%prog%tm2=sib%prog%tm1
00090            sib%prog%sh2=sib%prog%sh1
00091            sib%prog%spdm2=sib%prog%spdm1
00092            sib%prog%ps2=sib%prog%ps1
00093            sib%prog%dlwbot2=sib%prog%dlwbot1
00094            sib%prog%sw_dwn2=sib%prog%sw_dwn1
00095            sib%prog%lspr2=sib%prog%lspr1
00096            sib%prog%cupr2=sib%prog%cupr1
00097            sib%prog%tcc2=sib%prog%tcc1
00098         endif
00099     enddo
00100 
00101 end subroutine sibdrv_read_single
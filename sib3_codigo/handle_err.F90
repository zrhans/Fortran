00001 subroutine handle_err( status, routine, number )
00002 !==========================================================================
00003 ! handle_err is a generic subroutine that checks for errors in the netcdf 
00004 ! functions and prints out a description of the error.  it then terminates 
00005 ! the program
00006 !
00007 #ifdef PGF
00008 use netcdf
00009 use typeSizes
00010 #endif
00011 
00012 ! parameters
00013 integer, intent(in) :: status                       ! error status
00014 character(len=*), intent(in), optional :: routine   ! routine where err occurred
00015 integer, intent(in), optional :: number             ! error "line" number
00016                                                     ! used to pinpoint which line
00017                                                     ! caused the error
00018 
00019     print *, 'error', status, (nf90_strerror(status))
00020     if ( present(routine) ) print *, routine
00021     if ( present(number) ) print *, 'at',number
00022     stop 'stopped-netcdf error'
00023 
00024 end subroutine handle_err
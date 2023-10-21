00001 
00002 subroutine clm_tridia (n, a, b, c, r, u )
00003 
00004 !=========================================================================
00005 !
00006 !  CLMCLMCLMCLMCLMCLMCLMCLMCL  A community developed and sponsored, freely   
00007 !  L                        M  available land surface process model.  
00008 !  M --COMMON LAND MODEL--  C  
00009 !  C                        L  CLM WEB INFO: http://clm.gsfc.nasa.gov
00010 !  LMCLMCLMCLMCLMCLMCLMCLMCLM  CLM ListServ/Mailing List: 
00011 !
00012 !=========================================================================
00013 ! DESCRIPTION:
00014 !
00015 ! REVISION HISTORY:
00016 !  15 September 1999: Yongjiu Dai; Initial code
00017 !  15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision 
00018 !  15 January 2002: Ian Baker revision to work in SiB
00019 !=========================================================================
00020 ! $Id: tridiag_solver.F90,v 1.1 2005/04/06 22:24:04 chorak Exp $
00021 !=========================================================================
00022 
00023     use kinds
00024     implicit none
00025 
00026     !=== Arguments ===========================================================
00027 
00028     integer , intent(in)  :: n
00029     real(kind=dbl_kind), intent(in)  :: a(1:n),b(1:n),c(1:n),r(1:n)
00030     real(kind=dbl_kind), intent(out) :: u(1:n)
00031 
00032     !=== Local Variables =====================================================
00033 
00034     integer j
00035     real(kind=dbl_kind) gam(1:n),bet
00036 
00037     !=== End Variable List ===================================================
00038 
00039 
00040     bet  = b(1)
00041     u(1) = r(1) / bet
00042     do j = 2, n
00043         gam(j) = c(j-1) / bet
00044         bet = b(j) - a(j) * gam(j)
00045         u(j) = (r(j) - a(j)*u(j-1)) / bet
00046     enddo
00047 
00048     do j = n-1, 1, -1
00049         u(j) = u(j) - gam(j+1) * u(j+1)
00050     enddo
00051 
00052 end subroutine clm_tridia
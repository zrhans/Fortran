00001 
00002 !===================SUBROUTINE SORTIN===================================
00003 
00004 subroutine sortin( eyy, pco2y, range, gammas, ic)
00005 
00006 !=======================================================================
00007 !
00008 !     ARRANGES SUCCESSIVE PCO2/ERROR PAIRS IN ORDER OF INCREASING PCO2.
00009 !       ESTIMATES NEXT GUESS FOR PCO2 USING COMBINATION OF LINEAR AND
00010 !       QUADRATIC FITS.
00011 !
00012 !=======================================================================
00013 
00014 
00015     use kinds
00016 
00017     implicit none
00018 
00019 
00020     !Bio...INPUT VARIABLES
00021 
00022     integer(kind=int_kind),intent(in) :: ic     ! iteration count
00023 
00024     real(kind=dbl_kind),intent(in) :: range     !
00025     real(kind=dbl_kind),intent(in) :: gammas    !
00026 
00027 
00028     !Bio...OUTPUT VARIABLES...
00029     real(kind=dbl_kind),intent(inout),dimension(6) :: eyy    !
00030     real(kind=dbl_kind),intent(inout),dimension(6) :: pco2y  !
00031 
00032     !Bio...LOCAL VARIABLES
00033 
00034     integer(kind=int_kind) ::  i,j,n,l,i1,i2,i3,isp,is,ix
00035 
00036     logical (kind=log_kind) :: bitx    !
00037 
00038     real(kind=dbl_kind) :: one         !
00039     real(kind=dbl_kind) :: pmin        !
00040     real(kind=dbl_kind) :: emin        !
00041     real(kind=dbl_kind) :: a           !
00042     real(kind=dbl_kind) :: b           !
00043     real(kind=dbl_kind) :: pco2yl      !
00044     real(kind=dbl_kind) :: pco2yq      !
00045     real(kind=dbl_kind) :: ac1         !
00046     real(kind=dbl_kind) :: ac2         !
00047     real(kind=dbl_kind) :: bc1         !
00048     real(kind=dbl_kind) :: bc2         !
00049     real(kind=dbl_kind) :: cc1         !
00050     real(kind=dbl_kind) :: cc2         !
00051     real(kind=dbl_kind) :: aterm       !
00052     real(kind=dbl_kind) :: bterm       !
00053     real(kind=dbl_kind) :: cterm       !
00054     real(kind=dbl_kind) :: pco2b       !
00055     real(kind=dbl_kind) :: eyyisp      !
00056     real(kind=dbl_kind) :: eyyis       !
00057     real(kind=dbl_kind) :: eyyi1       !
00058     real(kind=dbl_kind) :: eyyi2       !
00059     real(kind=dbl_kind) :: eyyi3       !
00060     real(kind=dbl_kind) :: pco2yisp    !
00061     real(kind=dbl_kind) :: pco2yis     !
00062     real(kind=dbl_kind) :: pco2yi1     !
00063     real(kind=dbl_kind) :: pco2yi2     !
00064     real(kind=dbl_kind) :: pco2yi3     !
00065 
00066 
00067     one = 1.0_dbl_kind
00068 
00069     if( ic < 4 ) then
00070         pco2y(1) = gammas + 0.5_dbl_kind*range
00071         pco2y(2) = gammas                                             &
00072             + range*( 0.5_dbl_kind - 0.3_dbl_kind*sign(one,eyy(1)) )
00073         pco2y(3) = pco2y(1)- (pco2y(1)-pco2y(2))                      &
00074             /(eyy(1)-eyy(2)+1.e-10_dbl_kind)*eyy(1)
00075         pmin = min( pco2y(1), pco2y(2) )
00076         emin = min(   eyy(1),   eyy(2) )
00077         if ( emin > 0. .and. pco2y(3) > pmin )                        &
00078             pco2y(3) = gammas
00079     else
00080 
00081         n = ic - 1
00082         bitx = abs(eyy(n)) > 0.1
00083         if(.not. bitx) pco2y(ic) = pco2y(n)
00084         if(bitx) then
00085             do j = 2, n
00086                 a = eyy(j)
00087                 b = pco2y(j)
00088                 do i = j-1,1,-1
00089                     if(eyy(i) <= a ) go to 100
00090                     eyy(i+1) = eyy(i)
00091                     pco2y(i+1) = pco2y(i)
00092                 enddo ! i loop
00093                 i = 0
00094                 100        continue
00095                 eyy(i+1) = a
00096                 pco2y(i+1) = b
00097             enddo  ! j loop
00098         endif
00099 
00100 !-----------------------------------------------------------------------
00101 
00102         if(bitx) then
00103             pco2b = 0.
00104             is    = 1
00105         endif
00106 
00107         do ix = 1, n
00108             if(bitx) then
00109                 if( eyy(ix) < 0. )  then
00110                     pco2b = pco2y(ix)
00111                     is = ix
00112                 endif
00113             endif
00114         enddo
00115 
00116         if(bitx) then
00117             i1 = is-1
00118             i1 = MAX(1, i1)
00119             i1 = min(n-2, i1)
00120             i2 = i1 + 1
00121             i3 = i1 + 2
00122             isp   = is + 1
00123             isp = min0( isp, n )
00124             is = isp - 1
00125             eyyisp = eyy(isp)
00126             eyyis = eyy(is)
00127             eyyi1 = eyy(i1)
00128             eyyi2 = eyy(i2)
00129             eyyi3 = eyy(i3)
00130             pco2yisp = pco2y(isp)
00131             pco2yis = pco2y(is)
00132             pco2yi1 = pco2y(i1)
00133             pco2yi2 = pco2y(i2)
00134             pco2yi3 = pco2y(i3)
00135         endif
00136 
00137         if(bitx) then
00138 
00139             !itb...Neil Suits' patch to check for zero in the denominator...
00140             if(eyyis /= eyyisp)then
00141                 pco2yl=pco2yis - (pco2yis-pco2yisp) / (eyyis-eyyisp)*eyyis
00142             else
00143                 pco2yl = pco2yis * 1.01
00144             endif
00145 
00146             !   METHOD USING A QUADRATIC FIT
00147 
00148             ac1 = eyyi1*eyyi1 - eyyi2*eyyi2
00149             ac2 = eyyi2*eyyi2 - eyyi3*eyyi3
00150             bc1 = eyyi1 - eyyi2
00151             bc2 = eyyi2 - eyyi3
00152             cc1 = pco2yi1 - pco2yi2
00153             cc2 = pco2yi2 - pco2yi3
00154 
00155             !itb...Neil Suits' patch to prevent zero in denominator...
00156             if(bc1*ac2-ac1*bc2 /= 0.0 .and. ac1 /= 0.0_dbl_kind)then
00157                 bterm = (cc1*ac2-cc2*ac1)/(bc1*ac2-ac1*bc2)
00158                 aterm = (cc1-bc1*bterm)/ac1
00159                 cterm = pco2yi2-aterm*eyyi2*eyyi2-bterm*eyyi2
00160                 pco2yq= cterm
00161                 pco2yq= MAX( pco2yq, pco2b )
00162                 pco2y(ic) = ( pco2yl+pco2yq)/2.0_dbl_kind
00163             else
00164                 pco2y(ic) = pco2y(ic) * 1.01_dbl_kind
00165             endif
00166 
00167         endif
00168 
00169     endif
00170 !
00171 ! make sure pco2 does not fall below compensation point
00172     pco2y(ic) = MAX(pco2y(ic),gammas+0.01_dbl_kind)
00173 
00174 end
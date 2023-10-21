00001 !===================================================================================
00002 subroutine diagnostic_output ( sib, qp2, qp3, pbp1, pbp2, nnqp2,      & 
00003         nnqp3, npbp1, npbp2, npbp1mx, npbp2mx, ijtlen, doqp2, doqp3,  &
00004         nnqp2mx, nnqp3mx, indxqp3, indxqp2, indxpbp1, indxpbp2 )
00005 !===================================================================================
00006 ! Calculates time averages for diagnostic output for single point (pbp) and entire domain (qp)
00007 !
00008 ! Modifications:
00009 !  Kevin Schaefer corrected pbp2 from indxpbp1 to indxpbp2 (11/17/04)
00010 !===================================================================================
00011 
00012     use kinds
00013     use sib_const_module
00014     use sibtype
00015     use physical_parameters, only:    &
00016            hltm
00017 
00018 
00019     use sib_io_module, only: qpintp, histpp, imultpbpsib
00020 
00021     implicit none
00022 
00023 
00024     integer(kind=int_kind),intent(in) :: nnqp2
00025     integer(kind=int_kind),intent(in) :: nnqp3
00026     integer(kind=int_kind),intent(in) :: npbp1
00027     integer(kind=int_kind),intent(in) :: npbp2
00028     integer(kind=int_kind),intent(in) :: ijtlen
00029     integer(kind=int_kind),intent(in) :: nnqp3mx
00030     integer(kind=int_kind),intent(in) :: nnqp2mx
00031     integer(kind=int_kind),intent(in) :: npbp2mx 
00032     integer(kind=int_kind),intent(in) :: npbp1mx
00033 
00034     logical(kind=log_kind),intent(in) :: doqp2(nnqp2mx) 
00035     logical(kind=log_kind),intent(in) :: doqp3(nnqp3mx)
00036 
00037 
00038     ! time average diagnostic fields (all points)
00039     real(kind=dbl_kind), intent(inout) :: qp2(subcount,nnqp2) 
00040     real(kind=dbl_kind), intent(inout) :: qp3(subcount, nsoil, nnqp3)
00041 
00042     ! index of saved diagnostics
00043     integer(kind=int_kind),intent(in) :: indxqp2(nnqp2mx) 
00044     integer(kind=int_kind),intent(in) :: indxqp3(nnqp3mx)        
00045     integer(kind=int_kind),intent(in) :: indxpbp1(npbp1mx)  
00046     integer(kind=int_kind),intent(in) :: indxpbp2(npbp2mx)
00047 
00048 
00049     ! time series diagnostic fields (select points)
00050     real(kind=dbl_kind), intent(inout) :: pbp1(npbp1+1,ijtlen) 
00051     real(kind=dbl_kind), intent(inout) :: pbp2(nsoil, npbp2, ijtlen)
00052 
00053     !----------------------------------------------------------------------
00054 
00055     type(sib_t), dimension(subcount), intent(inout) :: sib
00056 
00057     !----------------------------------------------------------------------  
00058 
00059     !...LOCAL VARIABLES...
00060     integer(kind=int_kind) :: out_index,i,l,n
00061     real(kind=dbl_kind) :: discrim3 
00062     real(kind=dbl_kind) :: auxeadem   
00063 
00064     if(qpintp) then
00065         do out_index = 1,subcount
00066         !     save diagnostic output
00067         !------------------------------------------
00068         !  depth dependent diagnostics
00069             if(doqp3(1)) then 
00070                 do l = 1,nsoil
00071                     qp3(out_index,l,indxqp3(1)) = qp3(out_index,l,indxqp3(1)) + &
00072                         sib(out_index)%diag%soilscale(l)
00073                 enddo
00074             endif
00075             if(doqp3(2)) then 
00076                 do l = 1,nsoil
00077                     qp3(out_index,l,indxqp3(2)) = qp3(out_index,l,indxqp3(2)) + &
00078                         sib(out_index)%prog%td(l)
00079                 enddo
00080             endif
00081             if(doqp3(3)) then 
00082                 do l = 1,nsoil
00083                     qp3(out_index,l,indxqp3(3)) = qp3(out_index,l,indxqp3(3)) + &
00084                         sib(out_index)%prog%www_liq(l) 
00085                 enddo
00086             endif
00087             if(doqp3(4)) then 
00088                 do l = 1,nsoil
00089                     qp3(out_index,l,indxqp3(4)) = qp3(out_index,l,indxqp3(4)) + &
00090                         sib(out_index)%prog%www_ice(l) 
00091                 enddo
00092             endif
00093             if(doqp3(5)) then 
00094                 do l = 1,nsoil
00095                     qp3(out_index,l,indxqp3(5)) = qp3(out_index,l,indxqp3(5)) + &
00096                         sib(out_index)%diag%soilq10(l)
00097                 enddo
00098             endif
00099 
00100         !   depth independent diagnostics
00101             if(doqp2(1)) then 
00102                 qp2(out_index,indxqp2(1)) = qp2(out_index,indxqp2(1)) +    &
00103                     sib(out_index)%diag%ventmf 
00104             endif
00105             if(doqp2(2)) then 
00106                 qp2(out_index,indxqp2(2)) = qp2(out_index,indxqp2(2)) +    &
00107                     sib(out_index)%diag%ustar 
00108             endif
00109             if(doqp2(3)) then 
00110                 qp2(out_index,indxqp2(3)) = qp2(out_index,indxqp2(3)) +    &
00111                     sib(out_index)%prog%td(sib(out_index)%prog%nsl+1) 
00112             endif
00113             if(doqp2(4)) then 
00114                 qp2(out_index,indxqp2(4)) = qp2(out_index,indxqp2(4)) +    &
00115                     sib(out_index)%prog%tc 
00116             endif
00117             if(doqp2(5)) then 
00118                 qp2(out_index,indxqp2(5)) = qp2(out_index,indxqp2(5)) +    &
00119                     sib(out_index)%diag%fws*day 
00120             endif
00121             if(doqp2(6)) then 
00122                 qp2(out_index,indxqp2(6)) = qp2(out_index,indxqp2(6)) +    &
00123                     sib(out_index)%prog%snow_depth
00124             endif
00125             if(doqp2(7)) then 
00126                 qp2(out_index,indxqp2(7)) = qp2(out_index,indxqp2(7)) +    &
00127                     sib(out_index)%prog%snow_veg 
00128             endif
00129             if(doqp2(8)) then 
00130                 qp2(out_index,indxqp2(8)) = qp2(out_index,indxqp2(8)) +    &
00131                     sib(out_index)%diag%roffo 
00132             endif
00133             if(doqp2(9)) then 
00134                 qp2(out_index,indxqp2(9)) = qp2(out_index,indxqp2(9)) +    &
00135                     sib(out_index)%diag%ggl(6) 
00136             endif
00137             if(doqp2(10)) then 
00138                 if(sib(out_index)%prog%em.lt.1.e-6) then
00139                     auxeadem = 1.e6
00140                 else
00141                     auxeadem = sib(out_index)%prog%ea / sib(out_index)%prog%em
00142                 endif
00143                 qp2(out_index,indxqp2(10)) = qp2(out_index,indxqp2(10)) +  &
00144                     sib(out_index)%diag%ggl(6) * AUXeadem
00145             endif
00146             if(doqp2(11)) then 
00147                 qp2(out_index,indxqp2(11)) = qp2(out_index,indxqp2(11)) + &
00148                     sib(out_index)%diag%ggl(6) * sib(out_index)%diag%rha
00149             endif
00150             if(doqp2(12)) then 
00151                 qp2(out_index,indxqp2(12)) = qp2(out_index,indxqp2(12)) + &
00152                     sib(out_index)%diag%antemp(6) / sib(out_index)%diag%rb
00153             endif
00154             if(doqp2(13)) then 
00155                 qp2(out_index,indxqp2(13)) = qp2(out_index,indxqp2(13)) +    &
00156                     sib(out_index)%diag%assimn(6)*1.0E6
00157             endif
00158             if(doqp2(14)) then 
00159                 qp2(out_index,indxqp2(14)) = qp2(out_index,indxqp2(14)) +    &
00160                     sib(out_index)%diag%ra
00161             endif
00162             if(doqp2(15)) then 
00163                 qp2(out_index,indxqp2(15)) = qp2(out_index,indxqp2(15)) +    &
00164                     sib(out_index)%diag%rb
00165             endif
00166             if(doqp2(16)) then 
00167                 qp2(out_index,indxqp2(16)) = qp2(out_index,indxqp2(16)) +    &
00168                     sib(out_index)%diag%rc
00169             endif
00170             if(doqp2(17)) then 
00171                 qp2(out_index,indxqp2(17)) = qp2(out_index,indxqp2(17)) +    &
00172                     sib(out_index)%diag%rd
00173             endif
00174             if(doqp2(18)) then 
00175                 qp2(out_index,indxqp2(18)) = qp2(out_index,indxqp2(18)) +    &
00176                     sib(out_index)%diag%rsoil
00177             endif
00178             if(doqp2(19)) then 
00179                 qp2(out_index,indxqp2(19)) = qp2(out_index,indxqp2(19)) +    &
00180                     sib(out_index)%diag%rstfac(1)
00181             endif
00182             if(doqp2(20)) then 
00183                 qp2(out_index,indxqp2(20)) = qp2(out_index,indxqp2(20)) +    &
00184                     sib(out_index)%diag%rstfac(2)
00185             endif
00186             if(doqp2(21)) then 
00187                 qp2(out_index,indxqp2(21)) = qp2(out_index,indxqp2(21)) +    &
00188                     sib(out_index)%diag%rstfac(3)
00189             endif
00190             if(doqp2(22)) then 
00191                 qp2(out_index,indxqp2(22)) = qp2(out_index,indxqp2(22)) +    &
00192                     sib(out_index)%diag%rstfac(4)
00193             endif
00194             if(doqp2(23)) then 
00195                 qp2(out_index,indxqp2(23)) = qp2(out_index,indxqp2(23)) +    &
00196                     sib(out_index)%diag%aparkk
00197             endif
00198             if(doqp2(24)) then 
00199                 qp2(out_index,indxqp2(24)) = qp2(out_index,indxqp2(24)) +    &
00200                     sib(out_index)%param%zlt
00201             endif
00202             if(doqp2(25)) then 
00203                 qp2(out_index,indxqp2(25)) = qp2(out_index,indxqp2(25)) +    &
00204                     sib(out_index)%param%green
00205             endif
00206             if(doqp2(26)) then 
00207                 qp2(out_index,indxqp2(26)) = qp2(out_index,indxqp2(26)) +    &
00208                     sib(out_index)%diag%chf
00209             endif
00210             if(doqp2(27)) then 
00211                 qp2(out_index,indxqp2(27)) = qp2(out_index,indxqp2(27)) +    &
00212                     sib(out_index)%diag%shf
00213             endif
00214             if(doqp2(28)) then 
00215                 qp2(out_index,indxqp2(28)) = qp2(out_index,indxqp2(28)) +    &
00216                     sib(out_index)%diag%hc * dti
00217             endif
00218             if(doqp2(29)) then 
00219                 qp2(out_index,indxqp2(29)) = qp2(out_index,indxqp2(29)) +    &
00220                     sib(out_index)%diag%hg * dti
00221             endif
00222             if(doqp2(30)) then 
00223                 qp2(out_index,indxqp2(30)) = qp2(out_index,indxqp2(30)) +    &
00224                     sib(out_index)%diag%ect * dti
00225             endif
00226             if(doqp2(31)) then 
00227                 qp2(out_index,indxqp2(31)) = qp2(out_index,indxqp2(31)) +    &
00228                     sib(out_index)%diag%eci * dti
00229             endif
00230             if(doqp2(32)) then 
00231                 qp2(out_index,indxqp2(32)) = qp2(out_index,indxqp2(32)) +    &
00232                     sib(out_index)%diag%egs * dti
00233             endif
00234             if(doqp2(33)) then 
00235                 qp2(out_index,indxqp2(33)) = qp2(out_index,indxqp2(33)) +    &
00236                     sib(out_index)%diag%egi * dti
00237             endif
00238             if(doqp2(34)) then 
00239                 qp2(out_index,indxqp2(34)) = qp2(out_index,indxqp2(34)) +    &
00240                     sib(out_index)%prog%ea
00241             endif
00242             if(doqp2(35)) then 
00243                 qp2(out_index,indxqp2(35)) = qp2(out_index,indxqp2(35)) +    &
00244                     sib(out_index)%prog%ta
00245             endif
00246             if(doqp2(36)) then 
00247                 qp2(out_index,indxqp2(36)) = qp2(out_index,indxqp2(36)) +    &
00248                     sib(out_index)%prog%em
00249             endif
00250             if(doqp2(37)) then 
00251                 qp2(out_index,indxqp2(37)) = qp2(out_index,indxqp2(37)) +    &
00252                     sib(out_index)%diag%rha
00253             endif
00254             if(doqp2(38)) then 
00255                 qp2(out_index,indxqp2(38)) = qp2(out_index,indxqp2(38)) +    &
00256                     sib(out_index)%diag%omepot(6)
00257             endif
00258             if(doqp2(39)) then 
00259                 qp2(out_index,indxqp2(39)) = qp2(out_index,indxqp2(39)) +    &
00260                     sib(out_index)%diag%assimpot(6)
00261             endif
00262             if(doqp2(40)) then 
00263                 qp2(out_index,indxqp2(40)) = qp2(out_index,indxqp2(40)) +    &
00264                     sib(out_index)%diag%assimnp(6)
00265             endif
00266             if(doqp2(41)) then 
00267                 qp2(out_index,indxqp2(41)) = qp2(out_index,indxqp2(41)) +    &
00268                     sib(out_index)%diag%antemp(6)
00269             endif
00270             if(doqp2(42)) then 
00271                 qp2(out_index,indxqp2(42)) = qp2(out_index,indxqp2(42)) +    &
00272                     sib(out_index)%diag%wsfws(6)
00273             endif
00274             if(doqp2(43)) then 
00275                 qp2(out_index,indxqp2(43)) = qp2(out_index,indxqp2(43)) +    &
00276                     sib(out_index)%diag%wsfht(6)
00277             endif
00278             if(doqp2(44)) then 
00279                 qp2(out_index,indxqp2(44)) = qp2(out_index,indxqp2(44)) +    &
00280                     sib(out_index)%diag%wsflt(6)
00281             endif
00282             if(doqp2(45)) then 
00283                 qp2(out_index,indxqp2(45)) = qp2(out_index,indxqp2(45)) +    &
00284                     sib(out_index)%diag%wags(6)
00285             endif
00286             if(doqp2(46)) then 
00287                 qp2(out_index,indxqp2(46)) = qp2(out_index,indxqp2(46)) +    &
00288                     sib(out_index)%diag%wegs(6)
00289             endif
00290             if(doqp2(47)) then 
00291                 qp2(out_index,indxqp2(47)) = qp2(out_index,indxqp2(47)) +    &
00292                     sib(out_index)%param%aparc
00293             endif
00294             if(doqp2(48)) then 
00295                 qp2(out_index,indxqp2(48)) = qp2(out_index,indxqp2(48)) +    &
00296                     sib(out_index)%diag%assimci(6)
00297             endif
00298             if(doqp2(49)) then 
00299                 qp2(out_index,indxqp2(49)) = qp2(out_index,indxqp2(49)) +     &
00300                     sib(out_index)%diag%wci(6)
00301             endif
00302             if(doqp2(50)) then 
00303                 qp2(out_index,indxqp2(50)) = qp2(out_index,indxqp2(50)) +    &
00304                     sib(out_index)%diag%pfd
00305             endif
00306             if(doqp2(51)) then 
00307                 qp2(out_index,indxqp2(51)) = qp2(out_index,indxqp2(51)) +    &
00308                     (sib(out_index)%diag%ecmass + sib(out_index)%diag%egmass) &
00309                     * dti * 55.56
00310             endif
00311             if(doqp2(52)) then 
00312                 qp2(out_index,indxqp2(52)) = qp2(out_index,indxqp2(52)) +    &
00313                     sib(out_index)%diag%assim(6)*1.0E6
00314             endif
00315             if(doqp2(53)) then 
00316                 qp2(out_index,indxqp2(53)) = qp2(out_index,indxqp2(53)) +    &
00317                     sib(out_index)%diag%whs(6)
00318             endif
00319             if(doqp2(54)) then 
00320                 qp2(out_index,indxqp2(54)) = qp2(out_index,indxqp2(54)) +    &
00321                     sib(out_index)%prog%capac(1)
00322             endif
00323             if(doqp2(55)) then 
00324                 qp2(out_index,indxqp2(55)) = qp2(out_index,indxqp2(55)) +    &
00325                     sib(out_index)%prog%capac(2)
00326             endif
00327             if(doqp2(56)) then 
00328                 qp2(out_index,indxqp2(56)) = qp2(out_index,indxqp2(56)) +    &
00329                     1./sib(out_index)%prog%rst(6)
00330             endif
00331             if(doqp2(57)) then 
00332                 qp2(out_index,indxqp2(57)) = qp2(out_index,indxqp2(57)) +    &
00333                     sib(out_index)%diag%antemp(6) * sib(out_index)%prog%tc
00334             endif
00335             if(doqp2(58)) then 
00336                 qp2(out_index,indxqp2(58)) = qp2(out_index,indxqp2(58)) +    &
00337                     sib(out_index)%diag%snowmelt
00338             endif
00339             if(doqp2(59)) then 
00340                 qp2(out_index,indxqp2(59)) = qp2(out_index,indxqp2(59)) +    &
00341                     sib(out_index)%diag%ansqr(6)
00342             endif
00343             if(doqp2(60)) then 
00344                 do l = 1,nsoil
00345                     qp2(out_index,indxqp2(60)) = qp2(out_index,indxqp2(60)) +    &
00346                         sib(out_index)%prog%www_liq(l) * 0.001 
00347                 enddo
00348             endif
00349             if(doqp2(61)) then 
00350                 qp2(out_index,indxqp2(61)) = qp2(out_index,indxqp2(61)) +    &
00351                     sib(out_index)%diag%fss
00352             endif
00353             if(doqp2(62)) then
00354                 qp2(out_index,indxqp2(62)) = qp2(out_index,indxqp2(62)) +    &
00355                     sib(out_index)%prog%tm
00356             endif
00357             if(doqp2(63)) then
00358                 qp2(out_index,indxqp2(63)) = qp2(out_index,indxqp2(63)) +    &
00359                     sib(out_index)%prog%thm
00360             endif
00361             if(doqp2(64)) then
00362                 qp2(out_index,indxqp2(64)) = qp2(out_index,indxqp2(64)) +    &
00363                     sib(out_index)%prog%sh
00364             endif
00365             if(doqp2(65)) then
00366                 qp2(out_index,indxqp2(65)) = qp2(out_index,indxqp2(65)) +    &
00367                     sib(out_index)%prog%radvbc
00368             endif
00369             if(doqp2(66)) then
00370                 qp2(out_index,indxqp2(66)) = qp2(out_index,indxqp2(66)) +    &
00371                     sib(out_index)%prog%radvdc
00372             endif
00373             if(doqp2(67)) then
00374                 qp2(out_index,indxqp2(67)) = qp2(out_index,indxqp2(67)) +    &
00375                     sib(out_index)%prog%radnbc
00376             endif
00377             if(doqp2(68)) then
00378                 qp2(out_index,indxqp2(68)) = qp2(out_index,indxqp2(68)) +    &
00379                     sib(out_index)%prog%radndc
00380             endif
00381             if(doqp2(69)) then
00382                 qp2(out_index,indxqp2(69)) = qp2(out_index,indxqp2(69)) +    &
00383                     sib(out_index)%prog%dlwbot
00384             endif
00385             if(doqp2(70)) then
00386                 qp2(out_index,indxqp2(70)) = qp2(out_index,indxqp2(70)) +    &
00387                     sib(out_index)%prog%spdm
00388             endif
00389             if(doqp2(71)) then
00390                 qp2(out_index,indxqp2(71)) = qp2(out_index,indxqp2(71)) +    &
00391                     sib(out_index)%prog%ps
00392             endif
00393             if(doqp2(72)) then
00394                 qp2(out_index,indxqp2(72)) = qp2(out_index,indxqp2(72)) +    &
00395                     sib(out_index)%prog%lspr*3600.0
00396             endif
00397             if(doqp2(73)) then
00398                 qp2(out_index,indxqp2(73)) = qp2(out_index,indxqp2(73)) +    &
00399                     sib(out_index)%prog%cupr*3600.0
00400             endif
00401             if(doqp2(74)) then 
00402                 qp2(out_index,indxqp2(74)) = qp2(out_index,indxqp2(74)) +    &
00403                     sib(out_index)%diag%radc3(1)
00404             endif
00405             if(doqp2(75)) then 
00406                 qp2(out_index,indxqp2(75)) = qp2(out_index,indxqp2(75)) +    &
00407                     sib(out_index)%diag%radc3(2)
00408             endif
00409             if(doqp2(76)) then
00410                 discrim3 = -5.0 + 30.0 * sib(out_index)%diag%pco2i(6)   &
00411                     /sib(out_index)%prog%pco2ap
00412                 qp2(out_index,indxqp2(76)) = qp2(out_index,indxqp2(76)) +    &
00413                     discrim3
00414             endif
00415             if(doqp2(77))then
00416                 qp2(out_index,indxqp2(77)) = qp2(out_index,indxqp2(77)) +    &
00417                     sib(out_index)%prog%pco2ap
00418             endif
00419             if(doqp2(78))then
00420                 qp2(out_index,indxqp2(78)) = qp2(out_index,indxqp2(78)) +    &
00421                     sib(out_index)%diag%pco2c(6)
00422             endif
00423             if(doqp2(79))then
00424                 qp2(out_index,indxqp2(79)) = qp2(out_index,indxqp2(79)) +    &
00425                     sib(out_index)%diag%pco2i(6)
00426             endif
00427             if(doqp2(80))then
00428                 qp2(out_index,indxqp2(80)) = qp2(out_index,indxqp2(80)) +    &
00429                     sib(out_index)%diag%pco2s(6)
00430             endif
00431             if(doqp2(81))then
00432                 qp2(out_index,indxqp2(81)) = qp2(out_index,indxqp2(81)) +    &
00433                     sib(out_index)%prog%d13cca
00434             endif
00435             if(doqp2(82))then
00436                 qp2(out_index,indxqp2(82)) = qp2(out_index,indxqp2(82)) +    &
00437                     sib(out_index)%prog%d13cm
00438             endif
00439             if(doqp2(83))then
00440                 qp2(out_index,indxqp2(83)) = qp2(out_index,indxqp2(83)) +    &
00441                     sib(out_index)%param%d13cresp
00442             endif
00443             if(doqp2(84))then
00444                 qp2(out_index,indxqp2(84)) = qp2(out_index,indxqp2(84)) +    &
00445                     sib(out_index)%diag%kiecps(1)       !C3 plants
00446             endif
00447             if(doqp2(85))then
00448                 qp2(out_index,indxqp2(85)) = qp2(out_index,indxqp2(85)) +    &
00449                     sib(out_index)%diag%kiecps(2)       !C4 plants
00450             endif
00451             if(doqp2(86))then
00452                 qp2(out_index,indxqp2(86)) = qp2(out_index,indxqp2(86)) +    &
00453                     sib(out_index)%diag%d13cassimn(1)   !C3 plants
00454             endif
00455             if(doqp2(87))then
00456                 qp2(out_index,indxqp2(87)) = qp2(out_index,indxqp2(87)) +    &
00457                     sib(out_index)%diag%d13cassimn(2)   !C4 plants
00458             endif
00459             if(doqp2(88))then
00460                 qp2(out_index,indxqp2(88)) = qp2(out_index,indxqp2(88)) +    &
00461                     sib(out_index)%diag%d13cassimn(6)   !All plants summed
00462             endif
00463             if(doqp2(89))then
00464                 qp2(out_index,indxqp2(89)) = qp2(out_index,indxqp2(89)) +    &
00465                     sib(out_index)%diag%flux13c
00466             endif
00467 
00468             if(doqp2(90))then
00469                 qp2(out_index,indxqp2(90)) = qp2(out_index,indxqp2(90)) +    &
00470                     sib(out_index)%diag%flux12c
00471             endif
00472 
00473             if(doqp2(91)) then
00474                 qp2(out_index,indxqp2(91)) = qp2(out_index,indxqp2(91)) +    &
00475                     sib(out_index)%diag%flux_turb
00476             endif
00477             if(doqp2(92)) then
00478                 qp2(out_index,indxqp2(92)) = qp2(out_index,indxqp2(92)) +    &
00479                     sib(out_index)%diag%resp_grnd
00480             endif
00481             if(doqp2(93)) then
00482                 qp2(out_index,indxqp2(93)) = qp2(out_index,indxqp2(93)) +    &
00483                     sib(out_index)%prog%sw_dwn
00484             endif
00485             if(doqp2(94)) then
00486                 qp2(out_index,indxqp2(94)) = qp2(out_index,indxqp2(94)) +    &
00487                     sib(out_index)%stat%coszbar
00488             endif
00489             if(doqp2(95)) then
00490                 qp2(out_index,indxqp2(95)) = qp2(out_index,indxqp2(95)) +    &
00491                     sib(out_index)%stat%cosz
00492             endif
00493             if(doqp2(96)) then
00494                 qp2(out_index,indxqp2(96)) = qp2(out_index,indxqp2(96)) +    &
00495                     sib(out_index)%param%physfrac(2)
00496             endif
00497             if(doqp2(97)) then
00498                 qp2(out_index,indxqp2(97)) = qp2(out_index,indxqp2(97)) +    &
00499                     sib(out_index)%diag%rcassimn(1) * sib(out_index)%diag%assimn(1)
00500             endif
00501             if(doqp2(98)) then
00502                 qp2(out_index,indxqp2(98)) = qp2(out_index,indxqp2(98)) +    &
00503                     sib(out_index)%diag%rcassimn(2) * sib(out_index)%diag%assimn(2)
00504             endif
00505             if(doqp2(99)) then
00506                 qp2(out_index,indxqp2(99)) = qp2(out_index,indxqp2(99)) +    &
00507                     sib(out_index)%diag%rcassimn(6) * sib(out_index)%diag%assimn(6)
00508             endif
00509             if(doqp2(100)) then
00510                 qp2(out_index,indxqp2(100)) = qp2(out_index,indxqp2(100)) +    &
00511                     sib(out_index)%diag%assimn(1)
00512             endif
00513             if(doqp2(101)) then
00514                 qp2(out_index,indxqp2(101)) = qp2(out_index,indxqp2(101)) +    &
00515                     sib(out_index)%diag%assimn(2)
00516             endif
00517             if(doqp2(102)) then
00518                 qp2(out_index,indxqp2(102)) = qp2(out_index,indxqp2(102)) +    &
00519                     sib(out_index)%diag%assimn(6)*(sib(out_index)%diag%eastar-sib(out_index)%prog%ea)
00520             endif
00521             if(doqp2(103)) then
00522                 qp2(out_index,indxqp2(103)) = qp2(out_index,indxqp2(103)) +    &
00523                     sib(out_index)%diag%antemp(1)*sib(out_index)%diag%kiecps(1) 
00524             endif
00525             if(doqp2(104)) then
00526                 qp2(out_index,indxqp2(104)) = qp2(out_index,indxqp2(104)) +    &
00527                     sib(out_index)%diag%antemp(2)*sib(out_index)%diag%kiecps(2)
00528             endif
00529             if(doqp2(105)) then 
00530                 qp2(out_index,indxqp2(105)) = qp2(out_index,indxqp2(105)) +    &
00531                     sib(out_index)%diag%antemp(1) 
00532             endif
00533             if(doqp2(106)) then 
00534                 qp2(out_index,indxqp2(106)) = qp2(out_index,indxqp2(106)) +    &
00535                     sib(out_index)%diag%antemp(2)
00536             endif       
00537             if(doqp2(107)) then
00538                 qp2(out_index,indxqp2(107)) = qp2(out_index,indxqp2(107)) +    &
00539                     (sib(out_index)%diag%resp_grnd - sib(out_index)%diag%assimn(6))*1.0E6
00540             endif
00541             if(doqp2(108)) then
00542                 qp2(out_index,indxqp2(108)) = qp2(out_index,indxqp2(108)) +    &
00543                     sib(out_index)%diag%cflux*1.0E6
00544             endif
00545             if(doqp2(109)) then
00546                 qp2(out_index,indxqp2(109)) = qp2(out_index,indxqp2(109)) +    &
00547                     sib(out_index)%diag%fws
00548             endif
00549             if(doqp2(110)) then
00550                 qp2(out_index,indxqp2(110)) = qp2(out_index,indxqp2(110)) +    &
00551                     sib(out_index)%diag%www_tot_soil
00552             endif
00553             if(doqp2(111)) then
00554                 qp2(out_index,indxqp2(111)) = qp2(out_index,indxqp2(111)) +    &
00555                     sib(out_index)%diag%resp_auto*1.0E6
00556             endif
00557             if(doqp2(112)) then
00558                 qp2(out_index,indxqp2(112)) = qp2(out_index,indxqp2(112)) +    &
00559                     sib(out_index)%diag%resp_tot*1.0E6
00560             endif
00561             if(doqp2(113)) then
00562                 qp2(out_index,indxqp2(113)) = qp2(out_index,indxqp2(113)) +    &
00563                     sib(out_index)%diag%resp_het*1.0E6
00564             endif
00565             if(doqp2(114)) then
00566                 qp2(out_index,indxqp2(114)) = qp2(out_index,indxqp2(114)) +    &
00567                     sib(out_index)%diag%resp_can(6)*1.0E6
00568             endif
00569 
00570             if(doqp2(115)) then
00571                 qp2(out_index,indxqp2(115)) = qp2(out_index,indxqp2(115)) +    &
00572                     sib(out_index)%prog%pcosap
00573             endif
00574 
00575             if(doqp2(116)) then
00576                 qp2(out_index,indxqp2(116)) = qp2(out_index,indxqp2(116)) +    &
00577                     sib(out_index)%diag%cosflux*1.0E6
00578             endif
00579 
00580             if(doqp2(117)) then
00581                 qp2(out_index,indxqp2(117)) = qp2(out_index,indxqp2(117)) +    &
00582                     sib(out_index)%diag%cos_grnd*1.0E12
00583             endif
00584 
00585             if(doqp2(118)) then
00586                 qp2(out_index,indxqp2(118)) = qp2(out_index,indxqp2(118)) +    &
00587                     sib(out_index)%diag%cos_flux_pbl*1.0E6
00588             endif
00589 
00590             if(doqp2(119)) then
00591                 qp2(out_index,indxqp2(119)) = qp2(out_index,indxqp2(119)) +    &
00592                     sib(out_index)%diag%rcos*1.0E6
00593             endif
00594 
00595             if(doqp2(120)) then
00596                 qp2(out_index,indxqp2(120)) = qp2(out_index,indxqp2(120)) +    &
00597                     sib(out_index)%diag%coss
00598             endif
00599 
00600             if(doqp2(121)) then
00601                 qp2(out_index,indxqp2(121)) = qp2(out_index,indxqp2(121)) +    &
00602                     sib(out_index)%diag%cosi
00603             endif
00604 
00605             if(doqp2(122)) then
00606                 qp2(out_index,indxqp2(122)) = qp2(out_index,indxqp2(122)) +    &
00607                     sib(out_index)%diag%cosc
00608             endif
00609 
00610 
00611 
00612 
00613                             
00614         enddo ! index
00615     endif
00616 
00617     if(histpp) then
00618         do n = 1,ijtlen
00619         !   depth dependent diagnostics
00620             do l = 1,nsoil
00621                 pbp2(l,indxpbp2(1),n) = pbp2(l,indxpbp2(1),n) +    &
00622                     sib(imultpbpsib(n))%prog%www_liq(l)/(sib(imultpbpsib(n))%prog%dz(l) * &
00623                     sib(imultpbpsib(n))%param%poros * denh2o) + &
00624                     sib(imultpbpsib(n))%prog%www_ice(l)/(sib(imultpbpsib(n))%prog%dz(l) * &
00625                     sib(imultpbpsib(n))%param%poros * denice)
00626             enddo
00627             do l = 1,nsoil
00628                 pbp2(l,indxpbp2(2),n) = pbp2(l,indxpbp2(2),n) +    &
00629                     sib(imultpbpsib(n))%prog%td(l)
00630             enddo
00631             do l = 1,nsoil
00632                 pbp2(l,indxpbp2(3),n) = pbp2(l,indxpbp2(3),n) +    &
00633                     sib(imultpbpsib(n))%param%csolid(l)
00634             enddo
00635             do l = 1,nsoil
00636                 pbp2(l,indxpbp2(4),n) = pbp2(l,indxpbp2(4),n) +    &
00637                     sib(imultpbpsib(n))%prog%node_z(l)
00638             enddo
00639 
00640 
00641 
00642             
00643         !   depth independent diagnostics
00644             pbp1(indxpbp1(1),n) = pbp1(indxpbp1(1),n) +    &
00645                 sib(imultpbpsib(n))%diag%fss
00646 
00647             pbp1(indxpbp1(2),n) = pbp1(indxpbp1(2),n) +    &
00648                 sib(imultpbpsib(n))%diag%fws
00649 
00650             pbp1(indxpbp1(3),n) = pbp1(indxpbp1(3),n) +    &
00651                 sib(imultpbpsib(n))%prog%rst(6)
00652 
00653             pbp1(indxpbp1(4),n) = pbp1(indxpbp1(4),n) +    &
00654                 sib(imultpbpsib(n))%diag%assimn(6)*1.0E6
00655 
00656             pbp1(indxpbp1(5),n) = pbp1(indxpbp1(5),n) +    &
00657                 sib(imultpbpsib(n))%diag%resp_grnd*1.E6
00658 
00659             pbp1(indxpbp1(6),n) = pbp1(indxpbp1(6),n) +    &
00660                 (sib(imultpbpsib(n))%diag%resp_grnd - &
00661                 sib(imultpbpsib(n))%diag%assimn(6))*1.E6
00662 
00663             pbp1(indxpbp1(7),n) = pbp1(indxpbp1(7),n) +    &
00664                 sib(imultpbpsib(n))%diag%cflux * 1.E6
00665 
00666             pbp1(indxpbp1(8),n) = pbp1(indxpbp1(8),n) +    &
00667                 sib(imultpbpsib(n))%prog%td(sib(imultpbpsib(n))%prog%nsl+1)
00668 
00669             pbp1(indxpbp1(9),n) = pbp1(indxpbp1(9),n) +    &
00670                 sib(imultpbpsib(n))%prog%tc
00671 
00672             pbp1(indxpbp1(10),n) = pbp1(indxpbp1(10),n) +    &
00673                 sib(imultpbpsib(n))%prog%ea
00674 
00675             pbp1(indxpbp1(11),n) = pbp1(indxpbp1(11),n) +    &
00676                 sib(imultpbpsib(n))%prog%ta
00677 
00678             pbp1(indxpbp1(12),n) = pbp1(indxpbp1(12),n) +    &
00679                 sib(imultpbpsib(n))%prog%em
00680 
00681             pbp1(indxpbp1(13),n) = pbp1(indxpbp1(13),n) +    &
00682                 sib(imultpbpsib(n))%diag%rha
00683 
00684             pbp1(indxpbp1(14),n) = pbp1(indxpbp1(14),n) +    &
00685                 sib(imultpbpsib(n))%prog%capac(1)
00686 
00687             pbp1(indxpbp1(15),n) = pbp1(indxpbp1(15),n) +    &
00688                 sib(imultpbpsib(n))%prog%capac(2)
00689 
00690             pbp1(indxpbp1(16),n) = pbp1(indxpbp1(16),n) +    &
00691                 sib(imultpbpsib(n))%prog%snow_veg
00692 
00693             pbp1(indxpbp1(17),n) = pbp1(indxpbp1(17),n) -    &
00694                 sib(imultpbpsib(n))%prog%nsl
00695 
00696             pbp1(indxpbp1(18),n) = pbp1(indxpbp1(18),n) +    &
00697                 sib(imultpbpsib(n))%diag%areas
00698 
00699             pbp1(indxpbp1(19),n) = pbp1(indxpbp1(19),n) +    &
00700                 sib(imultpbpsib(n))%prog%snow_mass
00701 
00702             pbp1(indxpbp1(20),n) = pbp1(indxpbp1(20),n) +    &
00703                 sib(imultpbpsib(n))%prog%snow_depth
00704 
00705             pbp1(indxpbp1(21),n) = pbp1(indxpbp1(21),n) +    &
00706                 sib(imultpbpsib(n))%prog%pco2ap
00707 
00708             pbp1(indxpbp1(22),n) = pbp1(indxpbp1(22),n) +    &
00709                 sib(imultpbpsib(n))%diag%pco2c(6)
00710 
00711             pbp1(indxpbp1(23),n) = pbp1(indxpbp1(23),n) +    &
00712                 sib(imultpbpsib(n))%diag%pco2i(6)
00713 
00714             pbp1(indxpbp1(24),n) = pbp1(indxpbp1(24),n) +    &
00715                 sib(imultpbpsib(n))%diag%pco2s(6)
00716 
00717             pbp1(indxpbp1(25),n) = pbp1(indxpbp1(25),n) +    &
00718                 sib(imultpbpsib(n))%diag%roff
00719 
00720             pbp1(indxpbp1(26),n) = pbp1(indxpbp1(26),n) +    &
00721                 sib(imultpbpsib(n))%diag%qqq 
00722 
00723             pbp1(indxpbp1(27),n) = pbp1(indxpbp1(27),n) +    &
00724                 sib(imultpbpsib(n))%diag%roffo
00725 
00726             pbp1(indxpbp1(28),n) = pbp1(indxpbp1(28),n) +    &
00727                 sib(imultpbpsib(n))%diag%ra
00728 
00729             pbp1(indxpbp1(29),n) = pbp1(indxpbp1(29),n) +    &
00730                 sib(imultpbpsib(n))%diag%rb
00731 
00732             pbp1(indxpbp1(30),n) = pbp1(indxpbp1(30),n) +    &
00733                 sib(imultpbpsib(n))%diag%rc
00734 
00735             pbp1(indxpbp1(31),n) = pbp1(indxpbp1(31),n) +    &
00736                 sib(imultpbpsib(n))%diag%rd
00737 
00738             pbp1(indxpbp1(32),n) = pbp1(indxpbp1(32),n) +    &
00739                 sib(imultpbpsib(n))%diag%rsoil
00740 
00741             pbp1(indxpbp1(33),n) = pbp1(indxpbp1(33),n) +    &
00742                 sib(imultpbpsib(n))%diag%rstfac(1)
00743 
00744             pbp1(indxpbp1(34),n) = pbp1(indxpbp1(34),n) +    &
00745                 sib(imultpbpsib(n))%diag%rstfac(2)
00746 
00747             pbp1(indxpbp1(35),n) = pbp1(indxpbp1(35),n) +    &
00748                 sib(imultpbpsib(n))%diag%rstfac(3)
00749 
00750             pbp1(indxpbp1(36),n) = pbp1(indxpbp1(36),n) +    &
00751                 sib(imultpbpsib(n))%diag%rstfac(4)
00752 
00753             pbp1(indxpbp1(37),n) = pbp1(indxpbp1(37),n) +    &
00754                 sib(imultpbpsib(n))%diag%aparkk
00755 
00756             pbp1(indxpbp1(38),n) = pbp1(indxpbp1(38),n) +    &
00757                 sib(imultpbpsib(n))%param%zlt
00758 
00759             pbp1(indxpbp1(39),n) = pbp1(indxpbp1(39),n) +    &
00760                 sib(imultpbpsib(n))%param%green
00761 
00762             pbp1(indxpbp1(40),n) = pbp1(indxpbp1(40),n) +    &
00763                 sib(imultpbpsib(n))%diag%chf
00764 
00765             pbp1(indxpbp1(41),n) = pbp1(indxpbp1(41),n) +    &
00766                 sib(imultpbpsib(n))%diag%shf
00767 
00768             pbp1(indxpbp1(42),n) = pbp1(indxpbp1(42),n) +    &
00769                 sib(imultpbpsib(n))%diag%hc * dti
00770 
00771             pbp1(indxpbp1(43),n) = pbp1(indxpbp1(43),n) +    &
00772                 sib(imultpbpsib(n))%diag%hg * dti
00773 
00774             pbp1(indxpbp1(44),n) = pbp1(indxpbp1(44),n) +    &
00775                 sib(imultpbpsib(n))%diag%ect * dti
00776 
00777             pbp1(indxpbp1(45),n) = pbp1(indxpbp1(45),n) +    &
00778                 sib(imultpbpsib(n))%diag%eci * dti
00779 
00780             pbp1(indxpbp1(46),n) = pbp1(indxpbp1(46),n) +    &
00781                 sib(imultpbpsib(n))%diag%egs * dti
00782 
00783             pbp1(indxpbp1(47),n) = pbp1(indxpbp1(47),n) +    &
00784                 sib(imultpbpsib(n))%diag%egi * dti
00785 
00786             pbp1(indxpbp1(48),n) = pbp1(indxpbp1(48),n) +    &
00787                 sib(imultpbpsib(n))%diag%ess * dti  
00788 
00789             pbp1(indxpbp1(49),n) = pbp1(indxpbp1(49),n) +    &
00790                 sib(imultpbpsib(n))%diag%omepot(6)
00791 
00792             pbp1(indxpbp1(50),n) = pbp1(indxpbp1(50),n) +    &
00793                 sib(imultpbpsib(n))%diag%assimpot(6)
00794 
00795             pbp1(indxpbp1(51),n) = pbp1(indxpbp1(51),n) +    &
00796                 sib(imultpbpsib(n))%diag%assimnp(6) 
00797 
00798             pbp1(indxpbp1(52),n) = pbp1(indxpbp1(52),n) +    &
00799                 sib(imultpbpsib(n))%diag%antemp(6) 
00800 
00801             pbp1(indxpbp1(53),n) = pbp1(indxpbp1(53),n) +    &
00802                 sib(imultpbpsib(n))%diag%wsfws(6) 
00803 
00804             pbp1(indxpbp1(54),n) = pbp1(indxpbp1(54),n) +    &
00805                 sib(imultpbpsib(n))%diag%wsfht(6)
00806 
00807             pbp1(indxpbp1(55),n) = pbp1(indxpbp1(55),n) +    & 
00808                 sib(imultpbpsib(n))%diag%wsflt(6)
00809 
00810             pbp1(indxpbp1(56),n) = pbp1(indxpbp1(56),n) +    &
00811                 sib(imultpbpsib(n))%diag%wags(6)
00812 
00813             pbp1(indxpbp1(57),n) = pbp1(indxpbp1(57),n) +    &
00814                 sib(imultpbpsib(n))%diag%wegs(6)
00815 
00816             pbp1(indxpbp1(58),n) = pbp1(indxpbp1(58),n) +    &
00817                 sib(imultpbpsib(n))%param%aparc
00818 
00819             pbp1(indxpbp1(59),n) = pbp1(indxpbp1(59),n) +    &
00820                 sib(imultpbpsib(n))%diag%assimci(6)
00821 
00822             pbp1(indxpbp1(60),n) = pbp1(indxpbp1(60),n) +    &
00823                 sib(imultpbpsib(n))%diag%wci(6)
00824 
00825             pbp1(indxpbp1(61),n) = pbp1(indxpbp1(61),n) +    &
00826                 sib(imultpbpsib(n))%diag%pfd
00827 
00828             pbp1(indxpbp1(62),n) = pbp1(indxpbp1(62),n) + &
00829                 (sib(imultpbpsib(n))%diag%ecmass +    &
00830                 sib(imultpbpsib(n))%diag%egmass) * dti * 55.56
00831 
00832             pbp1(indxpbp1(63),n) = pbp1(indxpbp1(63),n) +    &
00833                 sib(imultpbpsib(n))%diag%assim(6)*1.e6
00834 
00835             pbp1(indxpbp1(64),n) = pbp1(indxpbp1(64),n) +    &
00836                 sib(imultpbpsib(n))%diag%whs(6)
00837 
00838             pbp1(indxpbp1(65),n) = pbp1(indxpbp1(65),n) +    &
00839                 sib(imultpbpsib(n))%diag%cu
00840 
00841             pbp1(indxpbp1(66),n) = pbp1(indxpbp1(66),n) +    &
00842                 sib(imultpbpsib(n))%diag%ct
00843 
00844             pbp1(indxpbp1(67),n) = pbp1(indxpbp1(67),n) +    &
00845                 sib(imultpbpsib(n))%diag%ventmf
00846 
00847             pbp1(indxpbp1(68),n) = pbp1(indxpbp1(68),n) +    &
00848                 sib(imultpbpsib(n))%prog%tm
00849 
00850             pbp1(indxpbp1(69),n) = pbp1(indxpbp1(69),n) +    &
00851                 sib(imultpbpsib(n))%prog%thm
00852 
00853             pbp1(indxpbp1(70),n) = pbp1(indxpbp1(70),n) +    &
00854                 sib(imultpbpsib(n))%prog%sh
00855 
00856             pbp1(indxpbp1(71),n) = pbp1(indxpbp1(71),n) +    &
00857                 sib(imultpbpsib(n))%prog%radvbc
00858 
00859             pbp1(indxpbp1(72),n) = pbp1(indxpbp1(72),n) +    &
00860                 sib(imultpbpsib(n))%prog%radvdc
00861 
00862             pbp1(indxpbp1(73),n) = pbp1(indxpbp1(73),n) +    &
00863                 sib(imultpbpsib(n))%prog%radnbc
00864 
00865             pbp1(indxpbp1(74),n) = pbp1(indxpbp1(74),n) +    &
00866                 sib(imultpbpsib(n))%prog%radndc
00867 
00868             pbp1(indxpbp1(75),n) = pbp1(indxpbp1(75),n) +    &
00869                 sib(imultpbpsib(n))%prog%dlwbot
00870 
00871             pbp1(indxpbp1(76),n) = pbp1(indxpbp1(76),n) +    &
00872                 sib(imultpbpsib(n))%prog%spdm
00873 
00874             pbp1(indxpbp1(77),n) = pbp1(indxpbp1(77),n) +    &
00875                 sib(imultpbpsib(n))%prog%ps
00876 
00877             pbp1(indxpbp1(78),n) = pbp1(indxpbp1(78),n) +    &
00878                 sib(imultpbpsib(n))%prog%lspr*3600.0
00879 
00880             pbp1(indxpbp1(79),n) = pbp1(indxpbp1(79),n) +    &
00881                 sib(imultpbpsib(n))%prog%cupr*3600.0
00882 
00883             pbp1(indxpbp1(80),n) = pbp1(indxpbp1(80),n) +    &
00884                 sib(imultpbpsib(n))%diag%radc3(1)
00885 
00886             pbp1(indxpbp1(81),n) = pbp1(indxpbp1(81),n) +    &
00887                 sib(imultpbpsib(n))%diag%radc3(2)
00888 
00889             pbp1(indxpbp1(82),n) = pbp1(indxpbp1(82),n) +    &
00890                 sib(imultpbpsib(n))%stat%cosz
00891 
00892             pbp1(indxpbp1(83),n) = pbp1(indxpbp1(83),n) +    &
00893                 sib(imultpbpsib(n))%prog%d13cca
00894 
00895             pbp1(indxpbp1(84),n) = pbp1(indxpbp1(84),n) +    &
00896                 sib(imultpbpsib(n))%prog%d13cm
00897 
00898             pbp1(indxpbp1(85),n) = pbp1(indxpbp1(85),n) +    &
00899                 sib(imultpbpsib(n))%param%d13cresp
00900 
00901             pbp1(indxpbp1(86),n) = pbp1(indxpbp1(86),n) +    &
00902                 sib(imultpbpsib(n))%diag%kiecps(1)
00903 
00904             pbp1(indxpbp1(87),n) = pbp1(indxpbp1(87),n) +    &
00905                 sib(imultpbpsib(n))%diag%kiecps(2)
00906 
00907             pbp1(indxpbp1(88),n) = pbp1(indxpbp1(88),n) +    &
00908                 sib(imultpbpsib(n))%diag%d13cassimn(1)
00909 
00910             pbp1(indxpbp1(89),n) = pbp1(indxpbp1(89),n) +    &
00911                 sib(imultpbpsib(n))%diag%d13cassimn(2)
00912 
00913             pbp1(indxpbp1(90),n) = pbp1(indxpbp1(90),n) +    &
00914                 sib(imultpbpsib(n))%diag%d13cassimn(6)
00915 
00916             pbp1(indxpbp1(91),n) = pbp1(indxpbp1(91),n) +    &
00917                 sib(imultpbpsib(n))%diag%flux13c
00918 
00919             pbp1(indxpbp1(92),n) = pbp1(indxpbp1(92),n) +    & 
00920                 sib(imultpbpsib(n))%diag%flux12c
00921 
00922             pbp1(indxpbp1(93),n) = pbp1(indxpbp1(93),n) +    &
00923                 sib(imultpbpsib(n))%diag%flux_turb
00924 
00925 !itb...soil/snow temperatures (loop)
00926 
00927             out_index = 94
00928             do l=-nsnow+1,nsoil
00929 
00930                 pbp1(indxpbp1(out_index),n) = pbp1(indxpbp1(out_index),n) +    &
00931                     sib(imultpbpsib(n))%prog%td(l)
00932                 out_index = out_index + 1
00933             enddo
00934 
00935 
00936 !itb...soil/snow liquid water (loop)
00937 
00938             out_index = 109
00939             do l=-nsnow+1,nsoil
00940                 pbp1(indxpbp1(out_index),n) = pbp1(indxpbp1(out_index),n) +    &
00941                     sib(imultpbpsib(n))%prog%www_liq(l)
00942 
00943                 out_index = out_index + 1
00944             enddo
00945 
00946 
00947 !itb...soil/snow ice water (loop)
00948 
00949             out_index = 124
00950             do l=-nsnow+1,nsoil
00951                 pbp1(indxpbp1(out_index),n) = pbp1(indxpbp1(out_index),n) +    &
00952                     sib(imultpbpsib(n))%prog%www_ice(l)
00953 
00954                 out_index = out_index + 1
00955             enddo
00956 
00957 !itb...volumetric soil water content (loop)
00958 
00959             out_index = 139
00960             do l=1,nsoil
00961 
00962                 pbp1(indxpbp1(out_index),n) = pbp1(indxpbp1(out_index),n) + &
00963                     sib(imultpbpsib(n))%prog%www_liq(l)/   &
00964                     (sib(imultpbpsib(n))%prog%dz(l)*denh2o)
00965 
00966                 out_index = out_index + 1
00967             enddo
00968 
00969 
00970 !itb...phystype-specific stomatal resistance (loop)
00971 
00972             out_index = 149
00973             do l=1,5
00974 
00975                 pbp1(indxpbp1(out_index),n) = pbp1(indxpbp1(out_index),n) +    &
00976                     sib(imultpbpsib(n))%prog%rst(l)
00977 
00978                 out_index = out_index + 1
00979             enddo
00980 
00981 
00982 !itb...phystype-specific net assimilation (loop)
00983 
00984             out_index = 154
00985             do l=1,5 
00986                 pbp1(indxpbp1(out_index),n) = pbp1(indxpbp1(out_index),n) +    &
00987                     sib(imultpbpsib(n))%diag%assimn(l)*1.0E6
00988 
00989                 out_index = out_index + 1
00990             enddo
00991 
00992 
00993 !itb...phystype-specific chloroplast CO2 partial pressure (loop)
00994 
00995             out_index = 159
00996             do l =1,5
00997                 pbp1(indxpbp1(out_index),n) = pbp1(indxpbp1(out_index),n) +    &
00998                     sib(imultpbpsib(n))%diag%pco2c(l)
00999 
01000                 out_index = out_index + 1
01001             enddo
01002 
01003 
01004 !itb...phystype-specific leaf internal CO2 partial pressure (loop)
01005 
01006             out_index = 164
01007             do l=1,5
01008 
01009                 pbp1(indxpbp1(out_index),n) = pbp1(indxpbp1(out_index),n) +    &
01010                     sib(imultpbpsib(n))%diag%pco2i(l)
01011 
01012                 out_index = out_index + 1
01013             enddo
01014 
01015 !itb...phystype-specific leaf surface CO2 partial pressure (loop)
01016 
01017             out_index = 169
01018             do l=1,5
01019                 pbp1(indxpbp1(out_index),n) = pbp1(indxpbp1(out_index),n) +    &
01020                     sib(imultpbpsib(n))%diag%pco2s(l)
01021 
01022                 out_index = out_index + 1
01023             enddo
01024 
01025             pbp1(indxpbp1(174),n) = pbp1(indxpbp1(174),n) +    &
01026                 sib(imultpbpsib(n))%stat%coszbar
01027 
01028             pbp1(indxpbp1(175),n) = pbp1(indxpbp1(175),n) +    &
01029                 sib(imultpbpsib(n))%prog%sw_dwn2
01030 
01031             pbp1(indxpbp1(176),n) = pbp1(indxpbp1(176),n) +    &
01032                 sib(imultpbpsib(n))%prog%sw_dwn
01033 
01034             pbp1(indxpbp1(177),n) = pbp1(indxpbp1(177),n) +    &
01035                 sib(imultpbpsib(n))%diag%radt(1)
01036 
01037             pbp1(indxpbp1(178),n) = pbp1(indxpbp1(178),n) +    &
01038                 sib(imultpbpsib(n))%diag%radt(2)
01039 
01040             pbp1(indxpbp1(179),n) = pbp1(indxpbp1(179),n) +    &
01041                 sib(imultpbpsib(n))%diag%radt(3)
01042 
01043             pbp1(indxpbp1(180),n) = pbp1(indxpbp1(180),n) +    &
01044                 sib(imultpbpsib(n))%diag%cas_e_storage
01045 
01046             pbp1(indxpbp1(181),n) = pbp1(indxpbp1(181),n) +    &
01047                 sib(imultpbpsib(n))%diag%radfac(1,1,1)
01048 
01049             pbp1(indxpbp1(182),n) = pbp1(indxpbp1(182),n) +    &
01050                 sib(imultpbpsib(n))%diag%radfac(1,2,1)
01051 
01052             pbp1(indxpbp1(183),n) = pbp1(indxpbp1(183),n) +    &
01053                 sib(imultpbpsib(n))%diag%radfac(1,1,2)
01054 
01055             pbp1(indxpbp1(184),n) = pbp1(indxpbp1(184),n) +    &
01056                 sib(imultpbpsib(n))%diag%radfac(1,2,2)
01057 
01058             pbp1(indxpbp1(185),n) = pbp1(indxpbp1(185),n) +    &
01059                 sib(imultpbpsib(n))%diag%radfac(2,1,1)
01060 
01061             pbp1(indxpbp1(186),n) = pbp1(indxpbp1(186),n) +    &
01062                 sib(imultpbpsib(n))%diag%radfac(2,2,1)
01063 
01064 
01065             pbp1(indxpbp1(187),n) = pbp1(indxpbp1(187),n) +    &
01066                 sib(imultpbpsib(n))%diag%radfac(2,1,2)
01067 
01068             pbp1(indxpbp1(188),n) = pbp1(indxpbp1(188),n) +    &
01069                 sib(imultpbpsib(n))%diag%radfac(2,2,2)
01070 
01071             pbp1(indxpbp1(189),n) = pbp1(indxpbp1(189),n) +    &
01072                 sib(imultpbpsib(n))%diag%resp_auto*1.0E6
01073 
01074             pbp1(indxpbp1(190),n) = pbp1(indxpbp1(190),n) +    &
01075                 sib(imultpbpsib(n))%diag%resp_tot*1.0E6
01076 
01077             pbp1(indxpbp1(191),n) = pbp1(indxpbp1(191),n) +    &
01078                 sib(imultpbpsib(n))%diag%resp_het*1.0E6
01079 
01080             pbp1(indxpbp1(192),n) = pbp1(indxpbp1(192),n) +    &
01081                 sib(imultpbpsib(n))%diag%resp_can(6)*1.0E6
01082 
01083             pbp1(indxpbp1(193),n) = pbp1(indxpbp1(193),n) +    &
01084                 sib(imultpbpsib(n))%diag%wbal
01085 
01086             pbp1(indxpbp1(194),n) = pbp1(indxpbp1(194),n) +    &
01087                 sib(imultpbpsib(n))%diag%ebal
01088 
01089             pbp1(indxpbp1(195),n) = pbp1(indxpbp1(195),n) +    &
01090                 sib(imultpbpsib(n))%diag%abal
01091 
01092             pbp1(indxpbp1(196),n) = pbp1(indxpbp1(196),n) +    &
01093                 sib(imultpbpsib(n))%diag%cbal
01094 
01095             pbp1(indxpbp1(197),n) = pbp1(indxpbp1(197),n) +    &
01096                 sib(imultpbpsib(n))%diag%gbal
01097 
01098             pbp1(indxpbp1(198),n) = pbp1(indxpbp1(198),n) +    &
01099                 sib(imultpbpsib(n))%diag%cas_w_storage
01100 
01101             pbp1(indxpbp1(199),n) = pbp1(indxpbp1(199),n) +    &
01102                 sib(imultpbpsib(n))%param%zlt
01103 
01104             pbp1(indxpbp1(200),n) = pbp1(indxpbp1(200),n) +    &
01105                 sib(imultpbpsib(n))%prog%pcosap
01106 
01107             pbp1(indxpbp1(201),n) = pbp1(indxpbp1(201),n) +    &
01108                 sib(imultpbpsib(n))%diag%cosflux*1.0E6
01109 
01110             pbp1(indxpbp1(202),n) = pbp1(indxpbp1(202),n) +    &
01111                 sib(imultpbpsib(n))%diag%cos_flux_pbl*1.0E6
01112 
01113             pbp1(indxpbp1(203),n) = pbp1(indxpbp1(203),n) +    &
01114                 sib(imultpbpsib(n))%diag%rcos*1.0E6
01115 
01116             pbp1(indxpbp1(204),n) = pbp1(indxpbp1(204),n) +    &
01117                 sib(imultpbpsib(n))%diag%coss
01118 
01119             pbp1(indxpbp1(205),n) = pbp1(indxpbp1(205),n) +    &
01120                 sib(imultpbpsib(n))%diag%cosi
01121 
01122             pbp1(indxpbp1(206),n) = pbp1(indxpbp1(206),n) +    &
01123                 sib(imultpbpsib(n))%diag%cosc
01124 
01125             pbp1(indxpbp1(207),n) = pbp1(indxpbp1(207),n) +    &
01126                 sib(imultpbpsib(n))%diag%cos_grnd * 1.0E6
01127 
01128         enddo
01129     endif
01130 
01131 end subroutine diagnostic_output
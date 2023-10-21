00001 subroutine output_control( sib, time, rank )
00002 
00003 use kinds
00004 use sibtype
00005 use timetype
00006 use sib_const_module
00007 use sib_io_module
00008 implicit none
00009 
00010 ! parameters
00011 type(sib_t), dimension(subcount), intent(inout) :: sib
00012 type(time_struct), intent(in) :: time
00013 integer(kind=int_kind), intent(in) :: rank
00014 
00015     !itb...output routine here
00016     if(time%sec_year /= starttime) then
00017         call diagnostic_output(sib,                            &
00018                 qpsib, qp3sib, pbpsib, pbp2sib, iiqpsib, iiqp3sib, &
00019                 iipbpsib, iipbp2sib, npbpsib, npbp2sib, ijtlensib, &
00020                 doqpsib, doqp3sib, nqpsib, nqp3sib, indxqp3sib,    &
00021                 indxqpsib, indxpbpsib, indxpbp2sib )
00022     endif
00023 
00024   
00025 
00026     ! switch qp file
00027     if ( time%switch_qp ) then
00028         call create_qp2( out_path, nqpsib, subcount, ihr, jhr, time%year,    &
00029                          time%month, longitude, latitude, sublon, sublat,    &
00030                          doqpsib, nameqpsib, listqpsib, qp2id, qp2varid,     &
00031                          qp2timeid, qp2charid, qp2startid, qp2endid,         &
00032                          qp2periodid, drvr_type, biome_source, soil_source,  &
00033                          soref_source, ndvi_source, c4_source, d13cresp_source,  &
00034                          rank )
00035         call create_qp3( out_path, nqp3sib, subcount, ihr, jhr,               &
00036                          time%year, time%month, nsoil, longitude, latitude,   &
00037                          sublon, sublat, doqp3sib, nameqp3sib,                &
00038                          listqp3sib, drvr_type, biome_source, soil_source,    &
00039                          soref_source, ndvi_source, c4_source, d13cresp_source,  &
00040                          qp3id, qp3varid, qp3timeid, qp3charid, qp3startid,   &
00041                          qp3endid, qp3periodid, rank )
00042     endif
00043 
00044 
00045     
00046     ! switch pbp file
00047     if ( time%switch_pbp .and. histpp ) then
00048         call create_pbp( ijtlensib, time%year, time%month, iipbpsib, npbpsib,  &
00049                          latpbp, lonpbp, dopbpsib, namepbpsib, listpbpsib,  &
00050                          indxpbpsib, drvr_type, biome_source, soil_source,  &
00051                          soref_source, ndvi_source, c4_source, d13cresp_source, &
00052                          out_path, pbptimeid, pbpcharid, pbpvarid,rank )
00053         call create_pbp2( ijtlensib, nsoil, time%year, time%month, iipbp2sib,  &
00054                           npbp2sib, latpbp, lonpbp, dopbp2sib,  &
00055                           namepbp2sib, listpbp2sib, indxpbp2sib, drvr_type,  &
00056                           biome_source, soil_source, soref_source, ndvi_source,  &
00057                           c4_source, d13cresp_source, out_path,  &
00058                           pbp2timeid, pbp2charid, pbp2varid,rank )
00059     endif
00060 
00061     ! output to pbp
00062     if ( time%write_pbp .and. histpp ) then
00063   
00064         pbpsib = pbpsib * time%pbp_incnt
00065         pbp2sib = pbp2sib * time%pbp_incnt
00066         call write_pbp( ijtlensib, time%year, time%month, time%day, time%sec_year,  &
00067                         iipbpsib, pbpsib, pbptimeid, pbpcharid,  &
00068                         pbpvarid, out_path, rank )
00069         call write_pbp2( ijtlensib, nsoil, time%year, time%month, time%day,  &
00070                          time%sec_year, iipbp2sib, pbp2sib, pbp2timeid,  &
00071                          pbp2charid, pbp2varid, out_path, rank )
00072         pbpsib(:,:) = 0.0
00073         pbp2sib(:,:,:) = 0.0
00074     endif
00075 
00076     ! output to qp
00077     if ( time%write_qp ) then
00078         qpsib = qpsib * time%qp_incnt
00079         qp3sib = qp3sib * time%qp_incnt
00080         call write_qp2( qp2id, qp2timeid, qp2startid, qp2endid, qp2periodid,  &
00081                         qp2charid, nqpsib, subcount, qp2varid, qpsib,         &
00082                         doqpsib, indxqpsib, time%year, time%month, time%day,  &
00083                         time%sec_year, time%end_period, time%period_length )
00084         call write_qp3( qp3id, qp3timeid, qp3startid, qp3endid, qp3periodid,  &
00085                         qp3charid, nqp3sib, subcount, nsoil, qp3varid,        &
00086                         qp3sib, doqp3sib, indxqp3sib, time%year, time%month,  &
00087                         time%day, time%sec_year, time%end_period,             &
00088                         time%period_length )
00089         qpsib(:,:) = 0.0
00090         qp3sib(:,:,:) = 0.0
00091     endif
00092 
00093 
00094 end subroutine output_control
00095 
00096 
00097 !-------------------------------------------------------------------------------
00098 subroutine file_closer
00099 !-------------------------------------------------------------------------------
00100 
00101 use kinds
00102 use sib_io_module
00103 #ifdef PGF
00104 use netcdf
00105 use typeSizes
00106 #endif
00107 
00108 ! local variables
00109 integer(kind=int_kind) :: status
00110 
00111     ! close all output files
00112     status = nf90_close( qp2id )
00113     status = nf90_close( qp3id )
00114     status = nf90_close( pbpid )
00115     status = nf90_close( pbp2id )
00116 
00117 end subroutine file_closer
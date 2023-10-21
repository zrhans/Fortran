00001 subroutine read_ndvi_old(filename, month, ndvidata, physfrac, d13cresp)
00002 !--------------------------------------------------------------------
00003 ! opens current sib_bc file and checks if nsib matches, then reads data
00004 
00005 use kinds
00006 use sib_const_module, only: nsib,     & ! total number of SiB points
00007                             subset,   & ! array mapping subdomain to nsib vector
00008                             subcount, & ! actual number of SiB points being simulated
00009                             physmax    ! maximum # of physiology types
00010 use sib_io_module, only: drvr_type,  & !jk flag to indicate driver data type
00011                          c4_source,  &
00012                          ndvi_source,  &
00013                          d13cresp_source
00014 #ifdef PGF
00015 use netcdf
00016 use typeSizes
00017 #endif
00018 
00019 ! define input variables
00020 character *100, intent(in) :: filename ! Filename for sib_bc
00021 integer(kind=int_kind), intent(in) :: month ! month to read
00022 real(kind=real_kind), dimension(subcount), intent(inout) :: ndvidata ! a month of ndvi data
00023 real(kind=dbl_kind), dimension(subcount,physmax), intent(out) :: physfrac
00024 real(kind=dbl_kind), dimension(subcount), intent(inout) :: d13cresp
00025 
00026 ! define local variables
00027 integer(kind=int_kind) :: i,j
00028 integer(kind=int_kind) :: ntest1     ! value of nsib read from sib_bc file
00029 integer(kind=int_kind) :: dimid
00030 integer(kind=int_kind) :: status
00031 integer(kind=int_kind) :: begin (2)  ! indices where to start and 
00032 integer(kind=int_kind) :: finish (2) !   finish reading from file
00033 integer(kind=int_kind) :: yyid       ! ndvi file id#
00034 integer(kind=int_kind) :: ndvi_id    ! ndvi variable id#
00035 integer(kind=int_kind) :: phys_id    ! physfrac variable id#
00036 integer(kind=int_kind) :: d13_id     ! d13cresp variable id#
00037 integer(kind=int_kind) :: x,y
00038 real(kind=real_kind), dimension(nsib) :: ndvi ! temp variables for parameters
00039 real(kind=real_kind), dimension(nsib,physmax)  :: frac
00040 real(kind=real_kind), dimension(nsib)  :: d13
00041 character(len=10) :: name
00042 
00043 
00044     ! Open the sib_bc file
00045     status = nf90_open ( trim(filename), nf90_nowrite, yyid )
00046     if (status /= nf90_noerr) call handle_err(status)
00047     status = nf90_inq_dimid ( yyid, 'nsib', dimid )
00048     if (status /= nf90_noerr) call handle_err(status)
00049     status = nf90_inquire_dimension ( yyid, dimid, name, ntest1 )
00050     if (status /= nf90_noerr) call handle_err(status)
00051     status = nf90_inq_varid ( yyid, 'ndvi', ndvi_id )
00052     if (status /= nf90_noerr) call handle_err(status)
00053     status = nf90_inq_varid ( yyid, 'd13cresp', d13_id )
00054     if (status /= nf90_noerr) call handle_err(status)
00055     status = nf90_inq_varid ( yyid, 'physfrac', phys_id )
00056     if (status /= nf90_noerr) call handle_err(status)
00057 
00058     ! test if boundary condition files are correct
00059     if(ntest1 /= nsib) stop ' open: file sib_bc no match with model for nsib'
00060 
00061     ! read in one month's data for all nsib points
00062     begin (1)  = month 
00063     begin (2)  = 1 
00064     finish (1) = 1 
00065     finish (2) = nsib 
00066     status = nf90_get_var ( yyid, ndvi_id, ndvi, begin, finish )
00067     if (status /= nf90_noerr) call handle_err (status)
00068     status = nf90_get_var ( yyid, d13_id, d13 )
00069     if (status /= nf90_noerr) call handle_err (status)
00070     status = nf90_get_var ( yyid, phys_id, frac )
00071     if (status /= nf90_noerr) call handle_err (status)
00072     
00073     ! read in global attributes that are passed on to output files
00074     status = nf90_get_att( yyid, nf90_global, 'ndvi_source', ndvi_source )
00075     status = nf90_get_att( yyid, nf90_global, 'c4_source', c4_source )
00076     status = nf90_get_att( yyid, nf90_global, 'd13cresp_source',  &
00077         d13cresp_source )
00078 
00079     ! close file
00080     status = nf90_close ( yyid )
00081     if (status /= nf90_noerr) call handle_err (status)
00082 
00083     ! copy only points in subdomain
00084     do i = 1, subcount
00085         ndvidata(i) = ndvi(subset(i))
00086         d13cresp(i) = d13(subset(i))
00087         do j = 1, physmax
00088             physfrac(i,j) = frac(subset(i),j)
00089         enddo
00090     enddo
00091 
00092 end subroutine read_ndvi_old
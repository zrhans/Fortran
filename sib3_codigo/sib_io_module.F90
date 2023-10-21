00001 module sib_io_module
00002 
00003     use kinds
00004     implicit none
00005 
00006     ! SiB diagnostics
00007     ! allocatable arrays
00008 
00009     integer(kind=int_kind) :: driver_id     ! driver data file id#
00010 
00011     real(kind=dbl_kind), dimension(:,:),   allocatable ::    
00012         qpsib,    ! horizontal only, time mean global diagnostics
00013         pbpsib     ! time series diagnostics at selected points, no vertical
00014 
00015     real(kind=dbl_kind), dimension(:,:,:), allocatable ::    
00016         qp3sib,      ! 3d time mean global diagnostics
00017         pbp2sib       ! time-height series diagnostics at selected points, 
00018 !
00019 ! time dependent parameter file id numbers
00020 integer(kind=int_kind) :: param_id ! time dependent parameter netcdf file id#
00021 
00022 !itb_modis
00023 !integer(kind=int_kind) :: ndvi_id  ! ndvi netcdf variable id#
00024 !integer(kind=int_kind) :: ndvi_time_id  ! ndvi time netcdf variable id#
00025 
00026 integer(kind=int_kind) :: mlai_id        ! modis LAI netcdf variable id#
00027 integer(kind=int_kind) :: mfpar_id       ! modis fPAR netcdf variable id#
00028 integer(kind=int_kind) :: modis_time_id  ! modis time netcdf variable id#
00029 !itb_modis
00030 
00031 integer(kind=int_kind) :: phys_id  ! physfrac netcdf variable id#
00032 integer(kind=int_kind) :: d13_id   ! d13cresp netcdf variable id#
00033 
00034     ! netcdf variable id #s
00035     integer(kind=int_kind) :: 
00036         qp2id,                
00037         qp2timeid,            
00038         qp2charid,            
00039         qp2startid,           
00040         qp2endid,             
00041         qp2periodid,          
00042         qp3id,                
00043         qp3timeid,            
00044         qp3charid,            
00045         qp3startid,           
00046         qp3endid,             
00047         qp3periodid,          
00048         pbpid,                
00049         pbptimeid,            
00050         pbpcharid,            
00051         pbp2id,               
00052         pbp2timeid,           
00053         pbp2charid
00054         
00055     integer(kind=int_kind), dimension(:), allocatable ::  
00056         qp2varid,       
00057         qp3varid,       
00058         pbpvarid,       
00059         pbp2varid
00060 
00061     integer, dimension(:), allocatable ::    
00062         indxqpsib,    ! indices of saved qpsib fields
00063         indxqp3sib,   ! indices of saved qp3sib fields
00064         indxpbpsib,   ! indices of saved pbpsib fields
00065         indxpbp2sib,  ! indices of saved pbp2sib fields
00066         numqpsib,     ! index number of qpsib field (from sib_qpopts)
00067         numqp3sib,    ! index number of qp3sib field (from sib_qpopts)
00068         numpbpsib,    ! index number of pbpsib field (from sib_pbpopts)
00069         numpbp2sib,   ! index number of pbp2sib field (from sib_pbpopts)
00070         imultpbpsib    ! gridpoint indices where pbp fields saved
00071 
00072     character(len=80), dimension(:), allocatable ::    
00073         listqpsib,   ! descriptions of qpsib fields
00074         listpbpsib,  ! descriptions of qp3sib fields
00075         listqp3sib,  ! descriptions of pbpsib fields
00076         listpbp2sib   ! descriptions of pbp2sib fields
00077 
00078     character(len=16), dimension(:), allocatable ::    
00079         nameqpsib,   ! field names of qp fields
00080         nameqp3sib,  ! field names of qp3 fields
00081         namepbpsib,  ! field names of pbp fields
00082         namepbp2sib  ! field names of pbp2 fields
00083 
00084     logical, dimension(:), allocatable ::    
00085         doqpsib,     ! true for qpsib fields to be saved
00086         dopbpsib,    ! true for qp3sib fields to be saved
00087         doqp3sib,    ! true for pbpsib fields to be saved
00088         dopbp2sib     ! true for pbp2sib fields to be saved
00089 
00090 
00091     integer  ijtlensib,   ! number of gridpoints where pbp fields are saved
00092         nqpsib,      ! number of possible qpsib fields (from sib_qpopts)
00093         nqp3sib,     ! number of possible qp3sib fields (from sib_qpopts)
00094         npbpsib,     ! number of possible pbpsib fields (from sib_pbpopts)
00095         npbp2sib,    ! number of possible pbp2sib fields (from sib_pbpopts)
00096         iiqpsib,     ! number of saved qp fields
00097         iipbpsib,    ! number of saved pbp fields
00098         iiqp3sib,    ! number of saved qp3 fields
00099         iipbp2sib     ! number of saved pbp2 fields
00100 
00101     ! file path names read in from namel file
00102     character (len=256) ::     
00103         param_path,    !jk path to sib parameters file
00104         ic_path,       !jk path to initial conditions file
00105         dr_format,     !jk path to driver data in FORMAT form
00106                         !jk with provision to write *yyyymm*
00107                         !jk filenames.  Binary files need additional
00108                         !jk alpha format: *aaa_yyyymm* to specify data type
00109         out_path,     & !jk path for output files
00110         qp_path,      & !jk path to qpopts file
00111         pbp_path,     & !jk path to pbpopts file
00112         co2_path,     & !jk path to CO2_resp file
00113         grid_path,    & !jk path to gridmap file
00114         c4_path         !itb path to c4 fraction file-monthly bc
00115 
00116     ! jk flag to indicate driver data type
00117     character (len=8) :: drvr_type 
00118                         !jk 'ecmwf'   - ECMWF global 
00119                         !jk 'single'  - single point ASCII table
00120                         !KS 'ncep1'   - ncep1 global
00121                         !   'ncep2'   - ncep2 global
00122                         !   'geos4'   - geos4 global
00123                         !itb 'ncp_sngl' - single point from ncep2
00124 
00125     ! parameter attributes
00126     character (len=100) :: biome_source
00127     character (len=100) :: soil_source
00128     character (len=100) :: soref_source
00129     character (len=100) :: ndvi_source
00130 
00131 !itb_modis
00132     character (len=100) :: modis_source
00133 !itb_modis
00134 
00135     character (len=100) :: c4_source
00136     character (len=100) :: d13cresp_source
00137 
00138     !itb...LOGICAL OPTIONS
00139     logical (kind=log_kind)  ::   
00140         hswtch = .false.      , !  flag to call switch routines
00141         qpintp = .true.       , !  flag to save qp info
00142         histpp = .false.      , !  flag to save pbp info
00143         debug = .false.       , !  run in debug mode?
00144         roll_respf,             !  calculate a rolling respfactor?
00145         use_diffuse=.false.      ! read diffuse driver data
00146 
00147 
00148 end module sib_io_module
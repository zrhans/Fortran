00001 module sib_bc_module
00002 
00003     !*************************************************************************
00004     ! This module contains boundary condition variable data that is used to  *
00005     ! calculate time variant boundary condition data. Values here must be    * 
00006     ! stored from month to month.                                            *
00007     !*************************************************************************
00008 
00009     use kinds
00010     implicit none
00011 
00012     ! Morph Table variables
00013     type biome_morph_var
00014         real(kind=real_kind) :: zc               ! Canopy inflection height (m)
00015         real(kind=real_kind) :: LWidth           ! Leaf width
00016         real(kind=real_kind) :: LLength          ! Leaf length 
00017         real(kind=real_kind) :: LAImax           ! Maximum LAI
00018         real(kind=real_kind) :: stems            ! Stem area index
00019         real(kind=real_kind) :: NDVImax          ! Maximum NDVI
00020         real(kind=real_kind) :: NDVImin          ! Minimum NDVI
00021         real(kind=real_kind) :: SRmax            ! Maximum simple ratio
00022         real(kind=real_kind) :: SRmin            ! Minimum simple ratio
00023     end type biome_morph_var
00024 
00025     type(biome_morph_var), allocatable :: MorphTab(:)
00026 
00027     ! Aerodynamic Interpolation table variables
00028     type aero_var
00029         real(kind=real_kind) :: zo          ! Canopy roughness coeff 
00030         real(kind=real_kind) :: zp_disp          ! Zero plane displacement
00031         real(kind=real_kind) :: RbC              ! RB Coefficient
00032         real(kind=real_kind) :: RdC             ! RC Coefficient
00033     end type aero_var
00034 
00035     type(aero_var),allocatable :: AeroVar(:,:,:)
00036 
00037     ! LAIgrid and FVCovergrid are used in mapper
00038     real(kind=real_kind) :: LAIgrid (50)        !
00039     real(kind=real_kind) :: fVCovergrid (50)    !
00040 
00041     ! NDVI data
00042     real(kind=real_kind), dimension(:), allocatable :: prevNDVI
00043 
00044 end module sib_bc_module
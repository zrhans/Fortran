00001 module kinds
00002 
00003     !-----------------------------------------------------------------------
00004     !
00005     !     This module defines variable precision for all common data
00006     !     types.
00007     !
00008     !-----------------------------------------------------------------------
00009 
00010     implicit none
00011     save
00012 
00013     !-----------------------------------------------------------------------
00014 
00015     integer, parameter :: 
00016         char_len  = 80,                    
00017         int_kind  = kind(1),               
00018         long_kind = selected_int_kind(18), 
00019         log_kind  = kind(.true.),          
00020         real_kind = selected_real_kind(6), 
00021         dbl_kind  = selected_real_kind(13)
00022 
00023 end module kinds
PROGRAM Regular_Block_Volume
!----------------------------------
! The volume of a regular block is
! calculated from the locations of
! the  three corners indicated  in 
! the diagram below.
!               
!       z           3----+
!       |          /    /|
!       |         +----+ |
!       +---- y   |    | 2 
!      /          |    |/
!     x           1----+
!
! The program demonstrates the use
! of structures as  arguments in a
! subroutine.
!----------------------------------

  IMPLICIT NONE

  TYPE Point
    REAL :: x, y, z
  END TYPE Point

  TYPE Block_Info
    TYPE(Point) :: Point1, Point2, Point3
    REAL :: Width, Height, Depth
    REAL :: Volume
  END TYPE Block_Info

  TYPE (Block_Info) :: Block

  PRINT *, "Enter the coordinates of the first point"
  READ *, Block%Point1

  PRINT *, "Enter the coordinates of the second point"
  READ *, Block%Point2

  PRINT *, "Enter the coordinates of the third point"
  READ *, Block%Point3

  CALL Volume(Block)

  Print *, "The volume of the block is ", Block%Volume

CONTAINS

  SUBROUTINE Volume(Block)

    TYPE (Block_Info), INTENT(INOUT) :: Block

     Block%Depth = ABS(Block%Point1%x - Block%Point2%x)
     Block%Width = ABS(Block%Point2%y - Block%Point3%y)
    Block%Height = ABS(Block%Point3%z - Block%Point1%z)

    Block%Volume = Block%Width * Block%Depth * Block%Height

  END SUBROUTINE Volume

END PROGRAM Regular_Block_Volume

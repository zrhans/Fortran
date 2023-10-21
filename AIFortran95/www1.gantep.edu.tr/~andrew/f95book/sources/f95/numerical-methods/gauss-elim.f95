PROGRAM Gauss_Elimination
!----------------------------------------------------
! Gauss Elimination with partial pivoting.
! This program solves the matrix equation "a x = y".
! "x" is an unknown one-dimensional matrix,  "a" and
! "y" are known one- and two-dimensional matrixes.
!----------------------------------------------------

  IMPLICIT NONE
  INTEGER :: n, i, j
  REAL(KIND=8) :: tol = 1.0E-20_8
  REAL(KIND=8) :: a(:,:), x(:), y(:), s(:)
  ALLOCATABLE  :: a, x, y, s

  n=3 ! "a" is an n-by-n matrix, "x" and "y" are vectors of size n.
  ALLOCATE( x(n), y(n), a(n,n), s(n) )

  ! Define known vector "y"
  y = (/ 8.0_8, -3.0_8 ,5.0_8 /)

  ! Define known matrix "a"
  a = RESHAPE((/ 0.0_8, 2.0_8, 3.0_8, &
                 4.0_8, 6.0_8, 7.0_8, &
                 2.0_8, 1.0_8, 6.0_8  /), (/ n, n /),ORDER=(/2,1/))

  ! Output "y" and "a"
  PRINT *, " "
  PRINT *, "y"
  PRINT *, "="
  PRINT *, REAL(y)
  PRINT *, " "
  PRINT *, "a"
  PRINT *, "="
  DO i=1,n
    PRINT *, (REAL(a(i,j)),j=1,n)
  END DO

  ! Solve the equation for x
  CALL Gauss_elimination_Solver(n,a,x,y,tol)
 
  ! Output "x"
  PRINT *, " "
  PRINT *, "x"
  PRINT *, "="
  PRINT *, REAL(x)

END PROGRAM Gauss_Elimination
 
SUBROUTINE Gauss_Elimination_Solver(n,a,x,y,tol)
 
  IMPLICIT NONE
  INTEGER :: n, er, j
  REAL(KIND=8) :: tol
  REAL(KIND=8) :: a(n,n), x(n), y(n), s(n)
 
  er = 0
  s(:) = ABS(a(:,1))
  DO j=2,n
    WHERE ( ABS(a(:,j)) > s(:) ) s(:) = ABS(a(:,j))
  END DO

  CALL Eliminate(a,s,n,y,tol,er)
  IF ( er /= -1 ) CALL Substitute(a,n,y,x)
 
END SUBROUTINE Gauss_Elimination_Solver

SUBROUTINE Eliminate(a,s,n,y,tol,er)

  IMPLICIT NONE
  INTEGER :: n,k,i,j,er
  REAL(KIND=8) :: tol, factor
  REAL(KIND=8) :: a(n,n), x(n), y(n), s(n)

  DO k=1,n-1

    CALL Pivot(a,y,s,n,k)
    IF ( ABS(a(k,k)/s(k)) < tol ) THEN
      er = -1
      EXIT
    END IF

    DO i=k+1,n
      factor = a(i,k)/a(k,k)
      a(i,k+1:n) = a(i,k+1:n) - factor*a(k,k+1:n)
      y(i) = y(i) - factor*y(k)
    END DO

  END DO

  IF ( ABS(a(k,k)/s(k)) < tol ) er=-1
 
END SUBROUTINE Eliminate
 
SUBROUTINE Pivot(a,y,s,n,k)
 
  IMPLICIT NONE
  INTEGER :: n, k, i, j, p
  REAL(KIND=8) :: big,dummy
  REAL(KIND=8) :: a(n,n), x(n), y(n), s(n)
 
  p = k-1 + MAXLOC(ABS(a(k:n,k)/s(k:n)),DIM=1)

  IF ( p /= k ) THEN
    DO j=k,n
      dummy = a(p,j)
      a(p,j) = a(k,j)
      a(k,j) = dummy
    END DO
    dummy = y(p)
    y(p) = y(k)
    y(k) = dummy
    dummy = s(p)
    s(p) = s(k)
    s(k) = dummy
  END IF

END SUBROUTINE Pivot
 
SUBROUTINE Substitute(a,n,y,x)
 
  IMPLICIT NONE
  INTEGER :: n, i
  REAL(KIND=8) :: s
  REAL(KIND=8) :: a(n,n), x(n), y(n)
 
  x(n) = y(n)/a(n,n)
  DO i=n-1,1,-1
    s = SUM(a(i,i+1:n)*x(i+1:n))
    x(i) = (y(i)-s)/a(i,i)
  END DO
 
END SUBROUTINE Substitute

program fucoes
implicit none

real :: start, finish, x
integer :: i, j, a = 0, b = 1.0

!Returns a REAL value representing the elapsed CPU time in seconds. 
!This is useful for testing segments of code to determine execution !time. 
!https://gcc.gnu.org/onlinedocs/gcc-4.7.4/gfortran/CPU_005fTIME.html#CPU_005fTIME
              
print*,'----'
do i = 1, 10
    call cpu_time(start)
	do j = 1, 1000000
		a = a + b
	enddo
	call cpu_time(finish)
	print '("Custou = ",e18.3," seconds.")',finish-start
enddo
print*,'----'
stop 'Programa finalizado.'
end
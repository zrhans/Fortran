program voltagem_ac
implicit none

real    :: v0, w, freq, v,t, dt,res
integer :: i,l,lmax

character(len=200)  :: linha

! Equacao: V(t) = V0 sen (wt)
! w = 2*PI*f

freq = 2.0
v0 = 1.0
w = 2.0 * 3.14 * freq
t= 0.0
open(unit = 10, file = 'veloc_data.txt')



!
! Entrada de dados
!
write(*,'(A)',ADVANCE="NO") 'Tensao (volts): '
read(*,*) v0

write(*,'(A)',ADVANCE="NO") 'Frequencia (Hz): '
read(*,*) freq

write(*,'(A)',ADVANCE="NO") 'Resolucao (segundos): Ex: 10e3 '
read(*,*) res


! Preparando 

l = 2 * 3.1416 ! 2Pi
print *,'l:',l
dt = l / res
print *,' dt: ',dt
lmax = nint (l / dt) ! nint(real)->integer | Inteiro mais proximo "nearest integer"
print *,' lmax: ',lmax
write(10,011) ' tempo  v rms'
011 format (3x,a,t1,a,t1,a)
write(10,*) '----------'

do i = 0, lmax
    t = i * dt
    v = v0 * sin(freq * t)
    print *,i,' V(',t,'): ', v
    write(10,*)t, v, v*1.41
end do

close(10)
stop("Finalizado!!!")

end program voltagem_ac
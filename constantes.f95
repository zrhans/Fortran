program constantes

real, parameter :: a = 1.7
double precision, parameter :: b = 1.7
INTEGER, PARAMETER :: long = selected_real_kind(9,99)
real, parameter :: c = selected_real_kind(6);

real, parameter :: mproton  = 1.67262178e-18 ! microgramas
real, parameter :: meletron = 9.10938291E-22 ! microgramas

! 1u (u.m.a) de 1,66 · 10–24 g
real, parameter :: uma = 1.66e-24


double precision :: diff

print *,'Real                           : '  ,a
print *,'Dupla precisao                 : '   ,b
print *,'selected_real_kind(4)          : '   ,c
print *,'selected_real_kind(9, 99)      : ',1.7_long

print *,'--------------- ' 
print *,'mproton       : ',mproton* (10**(-1*18))
print *,'meletron      : ',meletron* (10**(-1*22))
print *,'--------------- ' 
diff =  mproton - meletron
print *,'(p-e)         : ',mproton - meletron
print *,'diff          : ',diff 
print '(1x,a,Es10.2e2,a)','O pesa        :  ',14*uma,' g' 
   
   
end program constantes
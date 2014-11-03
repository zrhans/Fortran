program teste

character*50 s
integer i
real d ,m
logical t,f

s = '1234'
read (s,*) i

s = '1234.5678'
read (s,*) d

s = 'T' !TRUE
read (s,*) t

s = 'F' !FALSE
read (s,*) f

print *,'tipo Integer: ',i
print *,'tipo    Real: ',d
print *,'tipo Logical: ',t
print *,'tipo Logical: ',f
print *,'--------------'
write (s, *) i
print *,s

write (s, *) d
print *,s

write (s, *) t
print *,s

write (s, *) f
print *,s

!print '("Press any key to exit... "$)'
!read (*,*)

! Somando um Integer + Real
m=i+d
	print*,m
end

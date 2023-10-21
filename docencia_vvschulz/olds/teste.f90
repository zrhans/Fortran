program mruv
implicit none
real:: s0, s, v0, t, a

write(*,*) "Inserir valores s0(m) v0(km/h) t(s) a(m/s**2):"
read(*,*) s0, v0, t, a
v0 = v0/3.6
s = s0+v0*t+(a*t**2)/2

write(*,*) " o carro percorreu por s =", s, "(m) até parar."

end program
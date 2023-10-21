program mruv
implicit none
real:: s0, s, v1, v0, t, a
Character(len = 5) :: tipo, modo1, modo2, modo3

write(*,*) " Escolha a grandeza a ser calculada: "
write(*,*) " digite v para equação da velocidade, "
write(*,*) " Ou digite s para equação da posição "

modo1 = "s"
modo2 = "v"
read(*,*) tipo

if (tipo == modo1) then
        write(*,*) "Inserir valores s0(m) v0(km/h) t(s) a(m/s^2):"
        read(*,*) s0, v0, t, a
        v0 = v0/3.6
        s = s0+v0*t+(a*t**2)/2
        
        write(*,*) " o carro percorreu por s =", s, "(m) até parar."


else if (tipo == modo2) then
        write(*,*) "Inserir valores v0(km/h) v1(m/s) t(s) :"
        read(*,*) v0, v1, t
        ! Partindo dessa equação v1 = v0 + a*t
        ! isolamos a para ser calculado.
        
        !v = 0
        v0 = v0/3.6
        a = (v1-v0)/t

        write(*,*) " A aceleração é a =", a, "m/s**2"

end if
end program
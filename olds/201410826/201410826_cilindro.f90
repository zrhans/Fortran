        program cilindro
        implicit none
        real V,pi,R,h
        pi=3.141592
        print*,"digite os valores do raio e da altura nessa mesma ordem"
        read (*,*) R, h
        V = pi* (R**2)*h
        print*, "o volume deste cilindro ‚ de", V
        write(201410826,*) V
        end program cilindro

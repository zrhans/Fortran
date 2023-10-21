        ! Programa Feito por Lucas Zago, UFSM F¡sica Licenciatura 2017/1
        ! Este programa le informa‡äes de temperatura e converte em outras unidades
        
        program conversordetemperatura
        implicit none
        
        real*4 :: temperaturaC, temperaturaF, temperaturaK
        
        ! Solicitar ao usu rio uma temperatura em graus celsius
        write(*,*) "Insira a temperatura em graus Celsius"
        
        ! Ler a temperatura
        read (*,*) temperaturaC
        
        ! Converter para graus Fahrenheit
         temperaturaF = temperaturaC*1.8+32
         
        ! Converter para Kelvin
         temperaturaK = temperaturaC+273.15
         
        ! Mostrar na tela a temperatura convertida nas duas unidades
        
         write(*,*) "A temperatura convertida em graus Fahrenheit e " , temperaturaF, "graus Fahrenheit"
         write(*,*) "A temperatura convertida em Kelvin e " , temperaturaK, "Kelvin"
         
         end program conversordetemperatura

        

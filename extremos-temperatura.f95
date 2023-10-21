program extremos_temperatura
implicit none

real :: temperatura, tmax, tmin
logical :: continuar = .true.
character*1 :: repetir = 's'
integer :: n = 0
do while ( continuar )
    print*, 'Digite um valor para temperatura: '
    read(*,*) temperatura

    ! Analisando Temperatura Máxima e Mínima
    if ( temperatura > tmax ) then
        tmax = temperatura
    elseif ( temperatura < tmin ) then
        tmin = temperatura
    endif
    
    ! O proximo comando somente sera válido para o primeiro valor n = 0
    if ( n < 1 ) tmin = temperatura
    
    ! incrementa o contador de valores n
    n = n + 1
        
    ! Processamento para perguntar se o usuario quer continuar
    print*, 'Continuar inserindo valores? (S) '
    read(*,*) repetir
        
        ! Testa se o usuário digitou s ou S
        if ( (repetir == "s").OR.(repetir == "S") ) then
            continuar = .true.
        else
            continuar = .false.
        endif
enddo

print*,' --------------------------------------------'
print*,' Numero de dados   : ',n
print*,' Temperatura mínima: ',tmin
print*,' Temperatura máxima: ',tmax

print*,''
print*,' ========================'
stop '>>> Programa finalizado.'


end

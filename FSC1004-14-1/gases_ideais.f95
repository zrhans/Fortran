program gases_ideais
implicit none
 
real :: T,P,N,VOLUME
real :: le_valor, gas_ideal
 
    T = le_valor('o valor da temperatura');
    P = le_valor('o valor da pressão');
    N = le_valor('o numero de mols');
 
    volume = gas_ideal(T,P,N)
 
    write(*,*)"Volume: ",volume
 
end program gases_ideais
 
! ==================================
! FUNCOES
! ==================================
 
! Esta funcao calcula o volume de um gas ideal
real function gas_ideal(temp,press,n_mols)
implicit none
!integer,intent(in) :: n_mols
real :: temp, press,N_MOLS
    gas_ideal = n_mols*8.314*temp/press
    return
end function
 
! Esta funcao auxilia na leitura de valores digitados pelo usuario
real function le_valor(msg)
implicit none
 
    character(len=*), intent(in) :: msg
 
    ! Mostra a mensagem pedindo para o usuario entrar algum valor
    write(*, '(A,A,A)', ADVANCE = "NO") "Por favor entre ",msg,": "
    read(*,*) le_valor
end function le_valor
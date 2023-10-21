!----------------------------------------------------------
!                    Lei dos Gases Ideais
!==========================================================
! Equação: P.V = n.R.T 
!
! P  =  Pressão em Pascal.
! V  =  Volume em metro cúbico.
! n  =  Número de mols da amostra gasosa.
! R  =  Constante universal dos gases ideais.
! T  =  Temperatura em Kelvin.
!
!----------------------------------------------------------
! Ref: https://pt.wikipedia.org/wiki/Lei_dos_gases_ideais
!----------------------------------------------------------
program gases_ideais
implicit none
 
real :: T,P,N,VOLUME
real :: le_valor, calculavolgas_ideal ! Declarando o tipo das funções
 
    T = le_valor('o valor da temperatura [K]');
    P = le_valor('o valor da pressão [Pa]');
    N = le_valor('o número de mols');
 
    volume = calculavolgas_ideal(T,P,N)
 
    write(*,'(1x,A,f10.2)')"Volume: ",volume
 
end program gases_ideais
 
! ==================================
! FUNCOES
! ==================================
 
! Esta funcao calcula o volume de um gas ideal
real function calculavolgas_ideal(temp, press, n_mols)
implicit none
real, parameter :: Rgas = 8.314598
!integer,intent(in) :: n_mols
real :: temp, press,N_MOLS
    ! P.V = n.R.T -- Lei dos Gases Ideais
    calculavolgas_ideal = (n_mols * Rgas * temp) / press
end function
 
! Esta funcao auxilia na leitura de valores digitados pelo usuario
real function le_valor(msg)
implicit none
 
    character(len=*), intent(in) :: msg
    ! Mostra a mensagem pedindo para o usuario entrar algum valor
    write(*, '(A,A,A)', ADVANCE = "NO") "Digite ",msg,": "
    read(*,*) le_valor
end function le_valor
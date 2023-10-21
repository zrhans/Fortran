! NOME: Luisa Crauss, MATRICULA: 201810670
! DATA DE ENTREGA: 20/06/2018

program notas
  implicit none
real :: desvio_padrao, media=0, soma=0, variancia=0
integer :: i
integer, parameter :: n=6   
real, dimension(n) :: x
x(1)=9
x(2)=7
x(3)=5
x(4)=3
x(5)=2
x(6)=5.5

! Calcula a media

do i=1, n
   media = media+x(i)

end do

    media = media/n

  

  print*, 'A media das notas de Joao eh: ',media



  ! Calcula o somatório do desvio padrão

    do i=1, n

      soma = soma + ((x(i)-media)**2)

    end do

  

  ! Calcula o desvio padrão

    desvio_padrao = sqrt(soma/n)



    print*, 'O desvio padrao das notas de Joao eh: ',desvio_padrao



    ! Calcula a variância

    do i=1, n

      variancia = variancia + (media-x(i))**2

    end do

    variancia = variancia/n



    print*, 'a variancia das notas de Joao eh', variancia



    

end program notas

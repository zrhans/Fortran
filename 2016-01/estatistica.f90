program estatistica
! Programa que pede 5 notas e calcula a media, dispersao,
! variancia e o desvio padrao

! Criando variaveis

integer :: i, total_notas = 5
real :: nota, somatorio_notas, nota_media


! Criando um contador para as notas usadio a estrutura
! de repeticao do end do

! 9 5 7 3 2

do i = 1, total_notas
  print*,'Digite a nota ',i, ' de ',total_notas,' :'  
  read(*,*) nota
  somatorio_notas = somatorio_notas + nota
end do

   ! Calculo da media
   nota_media = somatorio_notas / total_notas

   print *, "A nota media vale : ", nota_media

end
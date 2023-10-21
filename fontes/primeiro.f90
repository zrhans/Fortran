program programa_do_umpierre
implicit none

integer :: i


! estutura de repedição
do i = 1, 5
    !Escreve o valor do i
    print*, "valor de i= ", i, ' feito'
enddo


read* ! Apenas na versao windows (para não fechar automaticamente a
      ! janela/terminal do prompt do MSDOS)
end program programa_do_umpierre

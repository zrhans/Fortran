program pesquisa

implicit none
character*5 nom(10), nome
integer i
logical achou

! Inicializaá∆o do vetor

nom(1) = 'aaaaa'; nom(2) = 'bbbbb'; nom(3) = 'ccccc'; nom(4) = 'ddddd';
nom(5) = 'eeeee'; nom(6) = 'fffff'; nom(7) = 'ggggg'; nom(8) = 'hhhhh';
nom(9) = 'iiiii'; nom(10) = 'jjjjj'

write(*, '(a)') 'Informe um nome: '    ! Usu†rio informa o nome
read(*,*) nome
achou = .false.

do 100 i = 1, 10                      ! Pesquisa nome
   if (nome == nom(i)) then
      write(*, '(/, a, i2)') 'Nome encontrado na posiá∆o ', i
      achou = .true.
      exit
    end if
100   continue

if (.not.achou) then
   write(*, '(/, a, a, a)') 'Nome: ', nome, ' n∆o encontrado'
end if

read*,
stop

end program pesquisa

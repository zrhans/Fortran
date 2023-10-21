program Notas_e_Media

implicit none
real notas(10), nota, soma, media
integer i

soma = 0

do 100 i = 1, 10
   write(*,50) 'Introduza a', i, 'a. nota: '
   read(*,*) nota
   soma = soma + nota
   notas(i) = nota
100   continue

media = soma / 10

write(*,60) 'M‚dia da turma: ', media
write(*,70) 'Notas acima da m‚dia: '
do 200 i = 1, 10
   if (notas(i) > media) then
      write(*,80) notas(i)
   end if
200   continue

read*,

stop
50    format(a, i2.1, a)
60    format(/, a, f4.1)
70    format(/, a, /)
80    format(f4.1)

end program Notas_e_Media

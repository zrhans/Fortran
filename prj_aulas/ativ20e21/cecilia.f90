program cecilia_20e21_jun_2018
  
implicit none
!mt= media do aluno
!nota(i)=nota do aluno
!nt= numero total de medias
 integer:: i, nt 
 real::media, desvio, soma, nota, variancia
 character:: sqrt
soma=0.0

print*, 'digite o numero de notas'
read*, nt 
do  i=1,nt

print*, 'digite a nota do aluno',i 

read*, nota
soma=soma+nota

end do
media=soma/nt
 desvio=sqrt ((nt- media)**2)
 variancia= sqrt(nt-media)**2
 print *, 'o valor medio eh', media
 print *, 'o desvio padrao eh', desvio
 print*, 'a variancia eh', variancia
 
 
end program cecilia_20e21_jun_2018

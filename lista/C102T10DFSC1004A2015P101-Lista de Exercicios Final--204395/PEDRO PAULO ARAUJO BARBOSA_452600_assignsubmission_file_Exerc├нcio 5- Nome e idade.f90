program nomeEidade
implicit none
character nome(50)*10
integer idade (50),i,j,soma,maioridade,menoridade,maior,menor,media
i=1;soma=0;maioridade=0;menoridade=99
write(*,10)'nome: '
read (*,*)nome(i)
do while (nome(i)/='fim')
write(*,10)'Idade: '
read(*,*)idade(i)
soma=soma+idade(i)
if(idade(i)>maioridade)then
maioridade=idade(i);maior=i
endif
if(idade(i)<menoridade)then
menoridade=idade(i);menor=i
endif
i=i+1
write(*,10)'nome: '
read(*,*)nome(i)
enddo
media=soma/(i-1)
write(*,*)
write(*,*)'Maior idade: ', nome(maior)
write(*,*)'Menor idade: ', nome(menor)
write(*,20)'Idade maior que a m‚dia: ', media
do 100 j=1, i-1
if (idade(j)>media) then
write(*,*)nome (j)
endif
100 continue
stop
10 format(a,$)
20 format(/, a)
end

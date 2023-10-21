program pesquisa
implicit none
character*5 nom(10), nome
integer i
logical achou
nom(1)='Pedro';nom(2)='Paulo';nom(3)='Ara£jo';nom(4)='Barbosa'
nom(5)='Hans';nom(6)='Rog‚rio';nom(7)='zimermann';nom(8)='Edu'
nom(9)='Bohr';nom(10)='Max'
write(*,'(a,$)')'Informe um nome: '
read(*,*)nome
Achou=.false.
do 100 i=1,10
if(nome==nom(i))then
write(*,'(/,a,i2)')'Nome encontrado na posi‡Æo ',i
achou=.true.
exit
endif
100 continue
if(.not.achou)then
write(*,'(/,a,a,a)')'Nome: ', nome , 'nÆo encontrado'
endif
stop
end

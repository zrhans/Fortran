program alcoolicos_anonimos
implicit none
	type ordenador
		integer :: matricula
		logical :: alcoolistafsc_bol
	end type
	
  integer :: dimensoes, i
  ! erro: type(ordenador),dimensions(:),allocatable::lista
	type(ordenador),dimension(:),allocatable :: lista
	print*,"Quantas pobres almas estão na Física?"
	read*,dimensoes
	
  allocate(lista(dimensoes))
  
	do i=1,dimensoes
	  print*,"Matricula"
		read *,lista(i)%matricula
	  print*,"Bebe? (.T./.F.)"
		read*,lista(i)%alcoolistafsc_bol
	end do
	
	do i=1,dimensoes
		print*,"O vivente de matrícula",lista(i)%matricula," é um viciado?: ",lista(i)%alcoolistafsc_bol
	end do
	
end program alcoolicos_anonimos
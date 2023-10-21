program funcoes

real, parameter :: PI = 4.0*ATAN2(1.0)
real :: r, area, volume

  ! Comente o que ocorre [erro]caso o formato seja *
  write(*,'(A,f8.2)', ADVANCE="NO") "Digite um valor para o raio r: "
  read(*,*) r

  ! Calcular area a volume usando funcoes
  area = area_esfera(r)
  volume = volume_esfera(r)

  ! Calcular area a volume usando subrotinas
  call area_esfera_s
  call volume_esfera_s


  write(*,*) "O raio digitado foi: ", r
  write(*,*) " USANDO FUNCOES "
  write(*,*) "O volume da esfera com este raio eh: ", volume
  write(*,*) "A area  da  esfera com este raio eh: ", area
  write(*,*) " --------------------------------- "
  write(*,*) " USANDO SUBROTINAS "
  write(*,*) "O volume da esfera com este raio eh: ", volume_s
  write(*,*) "A area  da  esfera com este raio eh: ", area_s

  contains
  !-----Volume_esfera----------------------------------------------------
  !
  !  Funcao para calcular o volume de uma esfera dado o raio
  !
  !---------------------------------------------------------------------
  function volume_esfera(r)
     volume_esfera = (4.0/3.0) * PI * r ** 3
  end function

  function area_esfera(r)
     area_esfera = 4 * PI * r ** 2
  end function

  subroutine area_esfera_s
      area_s =  4 * PI * r ** 2
  end subroutine

  subroutine volume_esfera_s
      volume_s =  (4.0/3.0) * PI * r ** 3
  end subroutine


end program funcoes

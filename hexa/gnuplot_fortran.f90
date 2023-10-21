module gnuplot_fortran
  implicit none
  contains ! o que contem dentro do modulo
  subroutine plot2d(x,y,filename)
    real, intent(in), dimension(:) :: x, y
    character(16), intent(in) :: filename
    integer :: size_x, size_y, i
    size_x = size(x)
    size_y = size(y)
    
    if (size_x .ne. size_y) then
      print*, "Tamanho das series nao sao iguais"
    else
      ! Criar os arrays e salvalos em arquivo.
      open(1,file=filename)
        do i = 1, size_x
          write(1,*)x(i),y(i)
        end do     
      close(1)
    endif
    
  end subroutine plot2d



end module gnuplot_fortran
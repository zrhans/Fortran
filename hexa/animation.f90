program animation
  use gnuplot_fortran
!  use gnu_style
  implicit none
  
  integer, parameter :: N =100
  real, parameter :: PI = 4*atan(1.0)
  real, dimension(0:N) :: x, y
  real :: xmin, xmax = 2*PI, dx
  
  character(12) :: data_name, frame_name
  integer :: i, frames = 250
  
  !===========================================================================
  ! CRIA DIRETORIO PARA ARMAZENAR OS DADOS E FRAMES
  !===========================================================================
  call system('mkdir data')
  call system('mkdir frames')
  
  !===========================================================================
  ! GERA O EIXO X
  !===========================================================================
  dx = (xmax - xmin) / N
  x(0:N) = [(i*dx, i=0,N)]
  
  !===========================================================================
  ! GERA OS FRAMES PARA ANIMACAO
  !===========================================================================
  do i = 1, frames
    !------ CRIA UM FILENAME PARA OS DADOS ------
    write(data_name,'(a,i4.4,a)') 'data',i,'.dat'
    
    !------ Eixo Y para cada frame ------
    y = sin(0.01 * x) / (x+1)
    
    !------ Escreve o arquivo de dados ------
    call plot2d(x, y, 'data'//data_name)
    
    !------ Cria um filename para o frame ------
    write(frame_name,'(a,i4.4,a)') 'plot',i,'.png'

    !------ Escreve o arquivo de estilo para o gnuplot ------
!    call style_new()
!    call pngcairo()
!    call output('frames/'//frame_name)
!    call grid()
!    call keyoff()
!    call xrange(0.0,2.0*PI)
!    call yrange(-1.0,1.0)
!    call title('Animation')
!    call datafile('data/'//data_name)
!    call style_close()
    
    !------ Gera o Frame ------
    call system('gnuplot style.gnu')    
    
  end do  
  
  !===========================================================================
  ! CRIA A ANIMACAO
  !===========================================================================
  call system('ffmpeg -i frames/plot%04d.png animation.avi')
  
  !===========================================================================
  ! Apaga os dados e frames
  !===========================================================================
  !call system('rm -rf data')
  !call system('rm -rf frames')
end program animation
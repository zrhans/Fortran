program homework_01_2
implicit none
real, parameter :: so = 1367.0    ! fluxo médio anual da energia solar incidente sobre o planeta Terra 
real, parameter :: alfa = 0.3     ! média do albedo
real, parameter :: sigma = 5.57e8 ! W m^-2 K^-4 - constante de Stefan-Boltzmann
real :: temperatura_emissividade, temperatura_emissividade1,temperatura_emissividade2

  temperatura_emissividade = ( so * ( (1.0 - alfa) / (4.0 * sigma) ) )**0.25
  temperatura_emissividade1 = ( so * 0.3 * ( (1.0 - alfa) / (4.0 * sigma) ) )**0.25
  temperatura_emissividade2 = ( so * ( (1.0 - ( alfa + alfa * 0.3 )) / (4.0 * sigma) ) )**0.25

  print *, 'Temperatura de emissividade da Terra So = 1367.0  : ', temperatura_emissividade
  print *, 'Temperatura de emissividade da Terra So - 30%     : ', temperatura_emissividade1
  print *, 'Temperatura de emissividade da Terra Albedo - 30% : ', temperatura_emissividade2

stop 'Programa finalizado!'
end program

! Resultado
! Temperatura de emissividade da Terra So = 1367.0  :    2.55998746E-02                                       ! Temperatura de emissividade da Terra So - 30%     :    1.89460274E-02                                       ! Temperatura de emissividade da Terra Albedo - 30% :    2.47340817E-02   


! use ! para comentários (não são considerados pelo compilador)

program hello ! marca o início do programa principal

  write(*,*) "Hello World"  ! write é uma das funções prontas (intrínsecas) do Fortran

end program  ! marca o final do programa principal


! No terminal, use:
! cd nome_da_pasta
! pra ir para o diretorio onde está seu programa. Então o compile com:
! gfortran nome.f90
! E o execute com:
! ./a.out

! Se quiser criar uma arquivo executável, um objeto, compile usando:
! gfortran nome.f90 -o name
! Execute com:
! ./name

! OBS: Só podemos usar até a coluna 132.
!      Se a expressão for maior que isso, deve-se
!      usar \ para continuar com o mesmo comando em mais de uma linha

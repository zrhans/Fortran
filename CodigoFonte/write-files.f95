! Arquivo: write-files.f95
! Autor : Hans
!
! Criado em Setembro 2014, 15:11

program principal
    implicit none
    integer :: a,b,c

    print*,"Digite o primeiro valor e pressione enter"
    read*, a

    print*,"Digite o segundo valor e pressione enter"
    read*, b
    
    print*,"Digite o terceiro valor e pressione enter"
    read*, c
    
    open(unit=10,file="saida.txt")
    write(10,*)a,b,c
    close(10)
 
    stop '>>>programa finalizado!'
end program principal
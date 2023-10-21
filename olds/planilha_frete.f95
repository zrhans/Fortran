! Calculadora de Frete
! -- Formatando a precisõ dos valores de saida
! -- Escrevendo os valores em um arquivo externo
program fretecalc
    implicit none
    real :: fconst = 2.0**(1.0/12.0)
    real :: scale_lenght = 25.5 ! [inches]
    integer :: total_frets = 24
    integer :: fret

100 format (i3, 5x, f5.2)
    open(unit=1, file='fretes.txt')
    
    do fret =1, total_frets
        ! print *, fret, scale_lenght / (fconst**fret)
        ! write(unit=1, fmt=100) fret, scale_lenght/(fconst**fret)
        write(1, 100) fret, scale_lenght/(fconst**fret)
    end do

    close(1)
    
    stop('Programa finalizado!!!!')
    
end program fretecalc
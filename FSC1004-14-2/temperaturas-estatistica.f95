program temperaturas_estatistica
implicit none

real, dimension(:), allocatable :: temp, variancia, variancia2
real :: sum_variancia2 = 0., desv_padrao = 0., desv_padrao_media = 0. 
real :: soma = 0. , media = 0.
integer :: i, indices, linhas = 0
character(1) :: dummy
character(100) :: arquivo

!call getarg(1,arquivo)

!open(10, file='dados/'//arquivo, status='old')
    open(10, file='dados/temperatura.txt', status='old')
    do
        read(10,'(A)',end=010) dummy
        linhas = linhas + 1
    end do
010 continue

    indices = linhas
    print *,'Indices: ',linhas
    
    allocate(temp(linhas))
    allocate(variancia(linhas))
    allocate(variancia2(linhas))

    rewind(10) ! Reposiciona o arquivo no inicio
    i=0
    do
        i = i + 1
        read(10,*,end=011) temp(i)
    enddo
011 continue
 
    indices = size(temp)

    do i = 1, indices
        soma = soma + temp(i)
    enddo

    media = soma/indices
    
    do i = 1, indices
        variancia(i) = temp(i) - media
        variancia2(i) = variancia(i)**2 
        sum_variancia2 =  sum_variancia2 + variancia2(i)
    end do

    desv_padrao = sqrt( (sum_variancia2) / (indices*1.0) ) ! Desvio padao amostral
    desv_padrao_media = desv_padrao / sqrt(indices*1.0) ! N Numero de vezes que a media foi feita

    print *,

    open(11, file='dados/temperaturas.txt')
    write(11,*) 'temperatura,  media,  variancia,  variancia2,  desv_padrao,  desv_padrao_da_media'
    write(11,*)'=============================='
 
    do i = 1, indices
        write(11,*)temp(i), media, variancia(i), variancia2(i), desv_padrao, &
        desv_padrao_media 
    end do

    print *, 'Temperaturas',temp
    print *, 'valor medio:', media
    print *, 'Variancia:', sum_variancia2/(indices*1.0)
    print *, 'Desvio padrao: ', desv_padrao 
    print *, 'Desvio padrao da media: ',desv_padrao_media  

    close(10)
    close(11)
    
    deallocate(temp); deallocate(variancia); deallocate(variancia2);

stop(">>>Programa Finalizado!!!")

end program temperaturas




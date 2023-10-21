program cor1
!Talles 201812521
!Variaveis 
real :: asa,as
integer :: raio
character(len=30):: cor
character(len=30) ::  corn

pi= 3.1415
asa= 0

do l=1,4
    print*, "Digite o raio"
    read*, raio
    print*, "Digite a cor"
    read*,  cor 
    r=raio
    as= (4*pi*(r**2))

    if (as>asa) then
        asa=as
        corn=cor
    end if
end do

print*, asa, corn

end program cor1

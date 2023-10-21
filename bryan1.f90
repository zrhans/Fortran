 program alturamedia
 implicit none
 real alt(100), soma, media
 character nome(100)*10
 integer i, j
 i=1; soma=0
 write(*,10)"Entre com ",i,"o.nome: "   ! aqui a pessoa coloca um nome qualquer !
 read (*,*)nome(i)
 do while (nome(i)/="fim")
   write (*,20)"Entre com altura: "   ! e aqui coloca a altura da pessoa cujo o nome vc colocou antes
   read (*,*) alt (i)
   soma=soma+alt(i)
   i=i+1
   write(*,10) "Entre com o",i,"o.nome: "
   read(*,*) nome(i)
 enddo
 media=soma/ (i-1)
 write(*,30)"Altura media: " , media            ! aqui sera calculado a altura media
 read(*,*) media
 do 100 j=1, i-1
        if (alt(j)>media) then
          write(*,40)nome(j)," " , alt (j)
        endif
 100  continue
  read(*,*)
      stop
 10   format (a,i2,a,$)
 20   format (a,$)
 30   format (/,a,f4.2,/)
 40   format (a,a,f4.2)
      end program alturamedia

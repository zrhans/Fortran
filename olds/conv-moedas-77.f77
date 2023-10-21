C***********************************************************************
C
C conversão de moedas
C impressão formatada, repetição usando 'do'
C
C***********************************************************************
C
      program moedas
      implicit none
C 
      integer i
      real reais, dollar, euro, dlrrl, eurrl
C relações dolar por real e euro por real
      data dlrrl, eurrl / 0.42768, 0.48715 /
C     constante do tipo ´real´
C 
C strings representando especificadores de formato
      character *30 form1, form2, sep *(*), line
      parameter ( sep = " ======== " )
      data form1     / "(1x,2(a12,5x,'|'),a12)" /
      data form2     / "(1x,2(f12.3,5x,'|'),f12.3)" /
      data line   / "(1x,49('-'))" /
C       
      write (*,*)  sep, "Tabela de conversao de moedas", sep
      write (*, line)
      write (*, form1 ) "Real","Dollar", "Euro"
      write (*, line)
C
C SINTAXE Fortran 77 puro:
C     do 100 i = 1, 10
C
      do i = 1, 10
        reais = i * 1.0
        dollar = reais * dlrrl
        euro     = reais * eurrl
        write (*, form2 ) reais, dollar, euro
C
C SINTAXE Fortran 77 puro:
C 100 continue
C  
      end do
      
      end program moedas

C               R E S U L T A D O
C       ======== Tabela de conversao de moedas ======== 
C -------------------------------------------------
C         real     |      dollar     |        euro
C -------------------------------------------------
C        1.000     |        .428     |        .487
C        2.000     |        .855     |        .974
C        3.000     |       1.283     |       1.461
C        4.000     |       1.711     |       1.949
C        5.000     |       2.138     |       2.436
C        6.000     |       2.566     |       2.923
C        7.000     |       2.994     |       3.410
C        8.000     |       3.421     |       3.897
C        9.000     |       3.849     |       4.384
C       10.000     |       4.277     |       4.872
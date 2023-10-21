!***********************************************************************
!
! conversão de moedas
! impressão formatada, repetição usando 'do', entrada de e saída para arquivo
!
!***********************************************************************
!
      program moedas
      implicit none

      integer i
      real reais, dollar, euro, dlrrl, eurrl
 
! strings representando especificadores de formato
      character *30 form1, form2, form3, sep *(*), line
      parameter ( sep = " ======== " )
      data form1      / "(1x,2(a12,5x,'|'),a12)" /
      data form2      / "(1x,2(f12.3,5x,'|'),f12.3)" /
      data form3      / "(1x,a,f8.4)" /
      data line   / "(1x,49('-'))" /

!--------------- Abre arquivo de entrada de dados e lê cotações ---------
      open( unit = 7, file='cotacao.dat', status='old')
      read( 7, * ) dlrrl
      read( 7, * ) eurrl
!--------------- Fecha arquivo de entrada ---------
      close (unit=7)

!--------------- Abre arquivo de saída -------------------------
      open( unit = 8, file='cambio.txt', status='replace' )
        
      write (*,*)  sep, "Tabela de conversao de moedas", sep
      write (*, line)
      write (*, form3) "Cotacao do real em relacao ao dollar R$1 = US$",dlrrl
      write (*, form3) "Cotacao do real em relacao ao euro R$1 = EURO",eurrl
      write (*, line)
      write (*, *)
      write (*, form1 ) "Real", "Dollar", "Euro"
      write (*, line)
       
      write (8,*)  sep, "Tabela de conversao de moedas", sep
      write (8, line)
      write (8, form1 ) "Real", "Dollar", "Euro"
      write (8, line)  

      do i = 1, 10
        reais = i * 1.0
        dollar = reais * dlrrl
        euro      = reais * eurrl
        write (*, form2 ) reais, dollar, euro
        write (8, form2 ) reais, dollar, euro
      end do  
      close (unit=8) 

      end program moedas

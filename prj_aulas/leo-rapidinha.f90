      program rapidinha
      implicit none
      print*, '(7+5) > 8 ou 2-1', (7+5).gt.8.or.(2.eq.1)
      print*, '(7+5) > 8 e 2=1', (7+5).gt.8.and.(2.eq.1)
      print*, '2 != 1', .not.(2.eq.1)
      read*
      stop
      end program rapidinha

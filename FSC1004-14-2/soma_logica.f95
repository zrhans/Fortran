      program soma_logica
      implicit none
      
      print*,' Resultados: '
      print*,'a) (7+5).gt.8.or.(2.eq.1) = ',(7+5).gt.8.or.(2.eq.1)
      print*,'b) (7+5).and.8.or.(2.eq.1) = ',(7+5).gt.8.and.(2.eq.1)
      print*,'c) .not.(2.eq1) = ',  .not.(2.eq.1)
      
      
      print*,'a)',(7+5).gt.8.or.(2.eq.1)
      print*,'b) ',(7+5).gt.8.and.(2.eq.1)
      print*,'c) ',  .not.(2.eq.1)
      
      end program soma_logica
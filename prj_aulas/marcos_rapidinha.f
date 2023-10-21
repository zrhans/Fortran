        program rapidinha
        print*, '(7 + 5) > 8 ou  2 = 1'
        print*, (7+5).gt.8.or.(2.eq.1)
        print*, '(7 + 5) > 8 e  2 = 1'
        print*, (7+5).gt.8.and.(2.eq.1)
        print*, '2 nao ‚ igual a 1'
        print*, .not.(2.eq.1)
        read*
        end program rapidinha

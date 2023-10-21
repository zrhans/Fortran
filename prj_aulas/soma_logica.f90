program soma_logica
implicit none
print*, 'Resultados: '
print*, '(7+5).gt.8.or.(2.eq.1) = ', (7+5).gt.8.or.(2.eq.1)
print*, '(7+5).gt.8.and.(2.eq.1) = ', (7+5).gt.8.and.(2.eq.1)
print*, '.not.(2.eq.1) = ', .not.(2.eq.1)

read*
stop 'Terminado'
end

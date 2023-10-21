!rm temperatura.png
reset
set terminal png enhanced size 500, 350 
set output 'temperatura.png'
set samples 800
set grid xtics lc rgb "#bbbbbb" lw 1 lt 0
set grid ytics lc rgb "#bbbbbb" lw 1 lt 0

# tipo de linha e ponto
set style line 1 lc rgb '#0060ad' lt 7 lw .3 pt 7 ps .3   # --- blue
set pointsize .3

set title 'FORTRAN - FSC1004'
# plot [-pi/2:pi] cos(x),-(sin(x) > sin(x+1) ? sin(x) : sin(x+1))

set ylabel 'Voltagem (Volts)'
set xlabel 'Tempo (s)'

p  'dados/temperaturas.txt' u :1 w linespoints ls 1 title 'T',\
          ''               u :2 w lines title '<T>',\
          ''               u :3 w i title 'variancia'
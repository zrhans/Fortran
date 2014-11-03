!rm simple.png
reset
set terminal png enhanced size 500, 350 
set output 'simple.png'
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

p  'veloc_data.txt' u 1:2 w linespoints ls 1 title 'Sinal', \
              ''    u 1:3 w linespoints title 'RMS'
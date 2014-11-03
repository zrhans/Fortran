% gnuplot

g(x)=exp(-x/5)
f(x)=g(x)*sin(x)
set xrange [0:4*pi]
set zeroaxis
set noborder
set xtics axis 0,pi/2,2*pi
set ytics ("0.73" f(pi/2),"-0.39" f(3*pi/2))
set grid
set xlabel "x"
set ylabel "f(x)"
set title "f(x)=exp(-x/5)sin(x)"
plot f(x) title "f=exp(-5/x)sin(x)",g(x) title "",-g(x) title ""
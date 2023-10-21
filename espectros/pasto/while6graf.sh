#!/bin/bash
rm -f spectro.gpl
rm -f cu1
rm -f cu2
rm -f cu3

graficos='SCS1-2002-03.spc'
fim=`wc -l $graficos | awk '{print $1}'`
li=1; lf=43;

cat<<ol>>spectro.gpl
set encoding iso_8859_1
set term postscript enhanced "arial" 14
#set term png
set linestyle 4 #typeline 2  linewidth 2
set pointsize 0.5
set logscale
#tamanho figura
set size 1,1
set origin 0,0

set output 'g$graficos.eps'
set title "g$graficos.eps"
#set multiplot
set title "Gráficos de espectros | $graficos - STM/KM77"
#
#  ---   --- 
# | U | | U | 
#  ---   ---
#  ---   ----- 
# | V | | CO2 | 
#  ---   -----
#  ---   ----- 
# | W | | H2O | 
#  ---   -----
#
##grafico inferior 1
#set size 0.5,0.3
#set origin 0,0
set ylabel "S_{u} ad"
set xlabel "f=nz/U"
p "<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:2 w p t'' 7 ,\\
ol

li=$((li+44));lf=$((li+42))
while [ $li -le $fim ];
do
cat<<eof>>spectro.gpl
"<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:2 w p ti'' 7 ,\\
eof


li=$((li+44));lf=$((li+42))
done;


fim=$((fim-1))
tail -n1 spectro.gpl > cu1
head -n$fim spectro.gpl > cu2
awk '{print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13}' cu1 > cu3
rm -f spectro.gpl
cat cu3>>cu2
mv cu2 spectro.gpl

#gnuplot spectro.gpl

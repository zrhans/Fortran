#!/bin/bash
geragpl()
{
echo " \\n gerando gráficos... \\n "
cat<<eogpl>MEDIO.GPL

set encoding iso_8859_1
set term postscript enhanced "arial" 12
set logscale
set style line 4
set pointsize 0.5
set output '$tip-B$BANDAS-$classe-$anomes.eps'
set ylabel "nS_{uvw}/u@^{2}_* {/Symbol f}^{2/3}"
set xlabel "f=nz/U"
set title 'Espectros medios 15 dias. $classe $anomes - STM/km77 | BANDAS: $BANDAS | Trat: $tip'
p 'M$tip-$classe-$anomes.spc' u 1:2 w lp ti 'S_u' 4,'' u 1:3 w lp ti 'S_v' 3,'' u 1:4 w lp ti 'S_w' 2

eogpl
gnuplot 'MEDIO.GPL'
convert $tip-B$BANDAS-$classe-$anomes.eps $tip-B$BANDAS-$classe-$anomes.pdf
#gv $tip-B$BANDAS-$classe-$anomes.pdf &
return
}


if [ $# -ne 1 ]; then

 echo "";echo "Usage: $0 filename bandsize";echo ""
 echo " $USER! Digite o nome do arquivo com a classe e o numero de bandas para suavizar."
 echo " Exemplo: $0 SCC1-2002-03.spc 20";echo ""
 else
clear
rm -f tmpspc.dat
rm -f filetmp.dat
for x in `ls M?????-*`
do
BANDAS=$1
classe=`echo $x | cut -c8-11`
anomes=`echo $x | cut -c13-19`
tip=`echo $x | cut -c2-6`
geragpl
done
#gv M$BANDAS-$classe-$anomes.pdf &
fi
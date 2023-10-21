#!/bin/bash
zeravariaveis()
{
li=1; lf=43;
fim=`wc -l $graficos | awk '{print $1}'`
fim=`expr $fim - 45 `
return
}

removecus()
{
rm -f cu1
rm -f cu2
rm -f cu3
cabgpl=`wc -l spectro.gpl | awk '{print $1}'`
cabgpl=$((cabgpl-1))
return
}

if [ $# -ne 1 ]; then

 echo "";echo "Usage: $0 filename";echo ""
 echo " $USER! Digite o nome do arquivo de dados para leitura."
 echo " Exemplo: $0 arquivo";echo ""
 else
clear

rm -f spectro.gpl
graficos=$1
zeravariaveis

cat<<ol>>spectro.gpl
set encoding iso_8859_1
set term postscript enhanced "arial" 8
#set term png
set linestyle 4 #typeline 2  linewidth 2
set pointsize 0.5
set logscale
#tamanho figura
set size 1,1
set origin 0,0

set output 'GRAF-$graficos.eps'
set title "GRAF-$graficos.eps"
set multiplot
set title "Gráficos de espectros | $graficos - STM/KM77"
#
#  ---   ----- 
# | a3 | | a4 | 
#  ---   -----
#  ---   ----- 
# | a1 | | a2 | 
#  ---   -----
#
##grafico inferior A1
set size 0.5,0.5
set origin 0,0
set ylabel "S_{u} ad A1"
set xlabel "f=nz/U"
ol

cat<<a12>>spectro.gpl
p "<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:2 w p t'' 7 ,\\
a12
li=$((li+44));lf=$((li+42))
while [ $li -le $fim ];
do

cat<<eofa11>>spectro.gpl
"<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:2 w p ti'' 7 ,\\
eofa11
li=$((li+44));lf=$((li+42))
done;

removecus
tail -n1 spectro.gpl > cu1
head -n$cabgpl spectro.gpl > cu2
awk '{print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13}' cu1 > cu3
rm -f spectro.gpl
cat cu3>>cu2
mv cu2 spectro.gpl


zeravariaveis

cat<<a>>spectro.gpl

##grafico inferior A2
set size 0.5,0.5
set origin 0.5,0
set ylabel "S_{v} ad A2"
set xlabel "f=nz/U"
p "<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:3 w p t'' 7 ,\\
a
zeravariaveis

li=$((li+44));lf=$((li+42))
while [ $li -le $fim ];
do
cat<<eofa21>>spectro.gpl
"<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:3 w p ti'' 7 ,\\
eofa21
li=$((li+44));lf=$((li+42))
done;

removecus
tail -n1 spectro.gpl > cu1
head -n$cabgpl spectro.gpl > cu2
awk '{print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13}' cu1 > cu3
rm -f spectro.gpl
cat cu3>>cu2
cp cu2 spectro.gpl

# p "<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:2 w p t'' 7 ,\\
# 
# li=$((li+44));lf=$((li+42))
# while [ $li -le $fim ];
# do
# cat<<eof1>>spectro.gpl
# "<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:2 w p ti'' 7 ,\\
# eof1
# 
# 
# li=$((li+44));lf=$((li+42))
# done;
# 
# 
# fim=$((fim-1))
# tail -n1 spectro.gpl > cu1
# head -n$fim spectro.gpl > cu2
# awk '{print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13}' cu1 > cu3
# rm -f spectro.gpl
# cat cu3>>cu2
# mv cu2 spectro.gpl





# 
 cat<<eol2>>spectro.gpl
# ##grafico inferior A3
 set size 0.5,0.5
 set origin 0.0,0.5
 set ylabel "S_{u} ad A3"
 set xlabel "f=nz/U"
# 
p [1:10] x w l 2
eol2
# p "<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:2 w p t'' 7 ,\\
# 
# li=$((li+44));lf=$((li+42))
# while [ $li -le $fim ];
# do
# cat<<eof2>>spectro.gpl
# "<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:2 w p ti'' 7 ,\\
# eof2
# 
# 
# li=$((li+44));lf=$((li+42))
# done;
# 
# 
# fim=$((fim-1))
# tail -n1 spectro.gpl > cu1
# head -n$fim spectro.gpl > cu2
# awk '{print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13}' cu1 > cu3
# rm -f spectro.gpl
# cat cu3>>cu2
# mv cu2 spectro.gpl





cat<<eol3>>spectro.gpl
##grafico inferior A4
set size 0.5,0.5
set origin 0.5,0.5
set ylabel "S_{u} ad A4"
set xlabel "f=nz/U"

p [1:10] x**2 w l 3
unset multiplot
eol3
# p "<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:2 w p t'' 7 ,\\
# 
# li=$((li+44));lf=$((li+42))
# while [ $li -le $fim ];
# do
# cat<<eof3>>spectro.gpl
# "<awk 'NR>=$li && NR<=$lf {print \$33,\$34,\$35,\$36}' $graficos" u 1:2 w p ti'' 7 ,\\
# eof3
# 
# 
# li=$((li+44));lf=$((li+42))
# done;
# 
# 
# fim=$((fim-1))
# tail -n1 spectro.gpl > cu1
# head -n$fim spectro.gpl > cu2
# awk '{print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13}' cu1 > cu3
# rm -f spectro.gpl
# cat cu3>>cu2
# mv cu2 spectro.gpl








mv spectro.gpl GRAF-$graficos
gnuplot GRAF-$graficos
gv GRAF-$graficos.eps &
fi
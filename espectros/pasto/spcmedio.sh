#!/bin/bash
geragpl()
{
echo "\\n gerando gr�ficos... \\n"
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


if [ $# -ne 2 ]; then

 echo "";echo "Usage: $0 filename bandsize";echo ""
 echo " $USER! Digite o nome do arquivo com a classe e o numero de bandas para suavizar."
 echo " Exemplo: $0 SCC1-2002-03.spc 20";echo ""
 else
clear
rm -f tmpspc.dat
rm -f filetmp.dat

BANDAS=$2
classe=`echo $1 | cut -c7-10`
anomes=`echo $1 | cut -c12-18`
tip=`echo $1 | cut -c1-5`
fourcol='tmpspc.dat'
tmpfour='filetmp.dat'
L=`wc -l $1 | awk '{print $1}'`

fim=$L;bot=0;top=44;
echo "\\n executando o awk... \\n"
awk '{print $33,$34,$35,$36}' $1 >$tmpfour
echo "\\n ..........OK \\n"
echo "\\n Ordenando...`date` \\n"

sort +0 -n $tmpfour>cu.dat
mv cu.dat $fourcol

##while [ $top -le $fim ];
##do
##awk 'NR>'$bot' && NR<='$top' {print $1,$2,$3,$4}' $tmpfour | sort +0 -n >>$fourcol
##	li=$((li+44));bot=$top;top=$((top+44));
##	#echo "BOT= $bot.  TOP=$top"
##done;
echo "\\n .........OK `date` \\n"
##
resul=`grep -c '*' $fourcol`
echo "\\n Resultado da analise de lixo (*): $resul \\n"
echo "<br>"

head -n10 espectro_medio.f>COCO
cat<<eof1>>COCO
        PARAMETER(L=$L,B=$BANDAS)
	REAL SSv(1:B),SSu(1:B),SSw(1:B),fm(1:B)
	REAL MSv(1:B),MSu(1:B),MSw(1:B),Mfm(1:B)
	REAL SUMF,SUMU,SUMV,SUMW
	REAL DATA(L,4)
	INTEGER SB,f

      i=1
! arquivo de entrada de dados
        OPEN(UNIT=54,FILE='$fourcol')
!arquivo de saida dos espectros medios
        OPEN(UNIT=10,FILE='M$1')

eof1
awk 'NR>25{print $0}' espectro_medio.f>>COCO
data=`date`
cat<<eofcoco>>COCO

!=====================================================================
! source generated by Microm2006 - HANS/LUMET
! $data
!=====================================================================

eofcoco
echo "\\n compilando ... \\n "
mv COCO COCO.f
 rm spcmedio.x77
 f77 -o spcmedio.x77 COCO.f
 chmod +x spcmedio.x77
 echo "\\n compilado executando... \\n "
 ./spcmedio.x77
 geragpl

#gv M$BANDAS-$classe-$anomes.pdf &
fi
echo "<h2>$0 FINALIZADO.</h2><br>"
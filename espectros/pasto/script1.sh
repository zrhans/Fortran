#!/bin/bash
cam="350sec/"
cd $cam
clear
# funcao para apendar arquivos de espectros de 
# acordo com a classe de estabilidade
apenda(){
 nome=`echo $x | cut -c1-4`
 echo "arquivo $x apendando em $nome"
 cat $x>>$nome.spc
 echo ">>> done !!"
 gzip $x
retun
}

for x in *.D*
do
apenda
# remove F2 dos arquivos de espectro
#  nome=`echo $x | cut -c3-10`
#  echo "$x e $nome"
# mv $x $nome

done
cd ..
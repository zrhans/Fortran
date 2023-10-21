
#!/bin/bash
clear
echo "Hans<- this running @ `date`.">>log
echo "-------------------------------------------------">>log
echo "-------[ Hans<- this running @ `date`. ]---------"
echo "-------------------------------------------------"
#---------------------[ FUNCOES ]----------------------"

GPL_script()
{
echo "-----[ montando GNUPLOT script ]-----"
in=`date | awk '{print $4}'`
camfile="$path/$graficos"
fileps=`echo $file | cut -c1-19` # ano e mes
N=`echo $file | cut -c5-6` # 14 ou 16
nome16="MNOR16-$classe-$periodo.spc"
filecomp="MFDR$N-$classe-$periodo.spc"
filecomp16="MFDR16-$classe-$periodo.spc"
tipoN="FDR$N"
tipoN16="FDR16"
tipo16="NOR16"
point='7'
cat<<endGPL_script> graf.gpl
set encoding iso_8859_1
set term postscript enhanced "arial" 8
set style line 3
#set linetype 1 linewidth 2
set pointsize 0.4 


set logscale
set grid

#tamanho figura
set size 1,1
set origin 0,0
set output '$graficos/$fileps-six.eps'
set multiplot
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
##grafico A1
set size 0.5,0.33
set origin 0,0.66
set title '$classe-$periodo - 30 min'
set ylabel "nS_{u}/u@^{2}_* {/Symbol f}^{2/3}"
set xlabel "f=nz/U"
plot[][] '$data/$nome' u 1:2 t '$tipo' w lp $point,'$data/$filecomp' u 1:2 t '$tipoN' w l 

#grafico A2
set size 0.5,0.33
set origin 0.5,0.66
set title '$classe-$periodo - 120 min'
set ylabel "nS_{u}/u@^{2}_* {/Symbol f}^{2/3}"
set xlabel "f=nz/U"
plot[][] '$data/$nome16' u 1:2 t '$tipo16' w lp $point,'$data/$filecomp16' u 1:2 t '$tipoN16' w l  

#grafico A3
set size 0.5,0.33
set origin 0.0,0.33
set title '$classe-$periodo - 30 min'
set ylabel "nS_{v}/u@^{2}_* {/Symbol f}^{2/3}"
set xlabel "f=nz/U"
plot[][] '$data/$nome' u 1:3 t '$tipo' w lp $point,'$data/$filecomp' u 1:3 t '$tipoN' w l  

#grafico inferior A4
set size 0.5,0.33
set origin 0.5,0.33
set title '$classe-$periodo - 120 min'
set ylabel "nS_{v}/u@^{2}_* {/Symbol f}^{2/3}"
set xlabel "f=nz/U"
plot[][] '$data/$nome16' u 1:3 t '$tipo16' w lp $point,'$data/$filecomp16' u 1:3 t '$tipoN16' w l  

#grafico inferior A5
set size 0.5,0.33
set origin 0,0.0
set title '$classe-$periodo - 30 min'
set ylabel "nS_{w}/u@^{2}_* {/Symbol f}^{2/3}"
set xlabel "f=nz/U"
plot[][] '$data/$nome' u 1:4 t '$tipo' w lp $point,'$data/$filecomp' u 1:4 t '$tipoN' w l  

#grafico inferior A6 
set size 0.5,0.33
set origin 0.5,0.0
set title '$classe-$periodo - 120 min'
set ylabel "nS_{w}/u@^{2}_* {/Symbol f}^{2/3}"
set xlabel "f=nz/U"
plot[][] '$data/$nome16' u 1:4 t '$tipo16' w lp $point,'$data/$filecomp16' u 1:4 t '$tipoN16' w l  
unset multiplot
endGPL_script

return
}

for file in MNOR14*.spc
do
clear
tail -n10 log
echo "_________________________________________________"
       tipo=`echo $file | cut -c2-6` #FDR14; NOR 14 ou 16
       classe=`echo $file | cut -c8-11` # SWC1 etc
       path=`pwd`
       periodo=`echo $file | cut -c13-19` # ano e mes
#cat pasto.r.original > filetxt
#sed s,arquivo,"$relpath$nome",g filetxt > rawpasto.r
echo "tipo    : $tipo"
echo "classe  : $classe"
echo "periodo : $periodo"
echo "path    : $path"
# removendo arquivos velhos
filetoremove="*.gpl";remfile
filetoremove="*.eps";remfile
filetoremove="*.r";remfile
# definindo nome para criacao de pastas
data=`pwd`
graficos="$data/graficos"
nome=$file
#montando script gnuplot
GPL_script
# execucao do script R
echo "--[ executando GNUPLOT script : $t1]--"
gnuplot 'graf.gpl'
convert "$graficos/$fileps-six.eps" "$graficos/$fileps-six.pdf"
#lp $graficos/$fileps-six.pdf
echo "--[ executado  GNUPLOT script : $tempo]--"
echo "--[ done! ]--"
echo ""

#mv $file usado/
done

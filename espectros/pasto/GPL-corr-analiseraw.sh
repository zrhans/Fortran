
#!/bin/bash
clear
echo "Hans<- this running @ `date`.">>log
echo "-------------------------------------------------">>log
echo "-------[ Hans<- this running @ `date`. ]---------"
echo "-------------------------------------------------"
#---------------------[ FUNCOES ]----------------------"

montapastas()
{
if [ -d "$path/$graficos" ]; then
  echo "Checando $path/$graficos ...OK" 
  else
  mkdir "$path/$graficos"
  echo "Criando $path/$graficos ...OK" 
fi
if [ -d "$path/$newdata" ]; then
  echo "Checando $path/$newdata ...OK" 
  else
  mkdir "$path/$newdata"
  echo "Criando $path/$newdata ...OK" 
fi
return
}

remfile(){
if [ -f $filetoremove ]; then
  echo "existe $filetoremove removendo ...OK"
  rm -f $filetoremove
  else
  echo "nao existe $filetoremove"
#return
fi
return
}

verifica()
{
 if [ -f $path/$newdata/$file ]; then
 echo "Verificando $path/$newdata/$file ..OK"
 else
 echo "Verificando $path/$newdata/$file ...FAILED !!!!!"
 fi
if [ -f $path/$graficos/craw$ymd.eps ]; then
 echo "Verificando $path/$graficos/craw$ymd.eps ..OK"
 else
 echo "Verificando $path/$graficos/craw$ymd.eps ...FAILED !!!!!"
 fi
return
}

R_script()
{
cat<<endR_script>cond-rawdata.r
#------------------------------------------------------------------------
# Desenvolvido para o software R
# Verificação de consistencia de dados pastagem. para sistema UNIX
# Version : 29/10/2004
# Original: Hans Rogério Zimermann
# Develop : Hans
# Revised : 30/04/2006
#------------------------------------------------------------------------
	options(object.size = 600000000)
#	options(digits=4)
	filein<-"$nome"
#       filein<-paste("/home/hans/work/dados/22col/2002/",arqu)
 
 

#------------------------------------------------------------------------
## função para gerar o nome do arquiv jpg de acordo com o argumento "var"
## passado para a função gname(var)
#
#         gname=function(var){
#                cam<-"$relpath/"
#                nome<-substr(filein,1,10)
#                nome<-paste(cam,var,"_",nome,".png",sep="", collapse=NULL)
#                png(filename=nome)
##                nome<-paste(cam,var,"-",nome,".jpg",sep="", collapse=NULL)
##                jpeg(filename=nome)
#        }
## expressao      gname("gradu")
#                plot(x,Y)
#------------------------------------------------------------------------
 
        dados <-scan(c(filein))    
	dados <- matrix(dados, ncol = 22, byrow = T)
	cuwind <- uwind <- dados[, 5]
	cvwind <- vwind <- dados[, 6]
	cwwind <- wwind <- dados[, 7]
	ctemp <- temp <- dados[, 8]
	cco2 <- co2 <- dados[, 9]
	ch2o <- h2o <- dados[, 10]

## condicionamento
	cuwind[(cuwind<(-15)) | (cuwind>15) ]<-mean(cuwind[(cuwind>(-15)) & (cuwind<15)])
	cvwind[(cvwind<(-15)) | (cvwind>15) ]<-mean(cvwind[(cvwind>(-15)) & (cvwind<15)])
	cwwind[(cwwind<(-15)) | (cwwind>15) ]<-mean(cwwind[(cwwind>(-15)) & (cwwind<15)])
	ctemp[(ctemp<15) | (ctemp>50) ]<-mean(ctemp[(ctemp>15) & (ctemp<50)])
	cco2[(cco2>700) | (cco2<200) ]<-mean(cco2[(cco2>200) & (cco2<700)])
	ch2o[(ch2o<15) | (ch2o>100) ]<-mean(ch2o[(ch2o>15) & (ch2o<100)])

## matrix com dados corrigidos pela média condicionada
	dados[, 5]  <- cuwind
	dados[, 6]  <- cvwind
	dados[, 7]  <- cwwind
	dados[, 8]  <- ctemp
	dados[, 9]  <- cco2
	dados[, 10] <- ch2o

## escreve os arquivos limpos (condicionados) em outro diretório
	write(t(dados),"$newdata/$nome",ncol=22,sep=" ")
## depois daqui utiliza externamente gnuplot p os graficos
endR_script
return
}

GPL_script()
{
echo "-----[ montando GNUPLOT script ]-----"
in=`date | awk '{print $4}'`
camfile="$path/$graficos"
cat<<endGPL_script> graf.gpl
set encoding iso_8859_1
set term postscript enhanced "arial" 7
#set term png
#set style line 1 linetype 1 linewidth 2
#set point size 2.

#tamanho figura
set size 1,1
set origin 0,0

set output '$camfile/craw$ymd.eps'
set title "Fonte: $path/$file"
set multiplot
set title "Gráficos de dados brutos condicionados | $ymd - STM/KM77"
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
set size 0.5,0.3
set origin 0,0
set ylabel "w"
plot[][] '$newdata/$nome' u :7 w l
#grafico inferior 2
set size 0.5,0.3
set origin 0,0.3
set ylabel "v"
plot[][] '$newdata/$nome' u :6 w l
#grafico superior 3
set size 0.5,0.3
set origin 0,0.6
set ylabel "u"
plot[][] '$newdata/$nome' u :5 w l
#grafico inferior 4 segunda coluna
set size 0.5,0.3
set origin 0.5,0.0
set ylabel "H_{2}O"
plot[][] '$newdata/$nome' u :10 w l
#grafico inferior 5 segunda coluna
set size 0.5,0.3
set origin 0.5,0.3
set ylabel "CO_{2}"
plot[][] '$newdata/$nome' u :9 w l
#grafico superior 6 segunda coluna
set size 0.5,0.3
set origin 0.5,0.6
set ylabel "Temperatura"
plot[][] '$newdata/$nome' u :8 w l
unset multiplot
endGPL_script
return
}

timer()
{
 tempo=`date | awk '{print $4}'`
return
}

extrai()
{
 echo "--[ extraindo $file ]--"
 gunzip $file
return
}

comprime()
{
echo "--[ comprimindo $nome ]--"
gzip $nome
echo "Original ... OK"
if [ -f $path/$newdata/$nome ]; then
 gzip $path/$newdata/$nome
 mv $file usado/
 echo "Tratado ... OK"
else
echo "$path/$newdata ...FAILED" 
fi
return
}



for file in *.dat.gz
do
clear
tail -n10 log
echo "_________________________________________________"
       dir=`echo $file | cut -c1-4`
       nome=`echo $file | cut -c1-14`
       path=`pwd`
       ymd=`echo $file | cut -c1-10`
#cat pasto.r.original > filetxt
#sed s,arquivo,"$relpath$nome",g filetxt > rawpasto.r
echo "file    : $file"
echo "dir     : $dir"
echo "nome    : $nome"
echo "path    : $path"
# removendo arquivos velhos
filetoremove="*.gpl";remfile
filetoremove="*.eps";remfile
filetoremove="*.r";remfile
# definindo nome para criacao de pastas
newdata="data-cond"
graficos="graf-cond"
montapastas
extrai

# montando script R
R_script
#montando script gnuplot
GPL_script
# execucao do script R
timer
t1=$tempo
echo "--[ executando R script : $t1 ]--"
R --no-save < cond-rawdata.r

timer
echo "--[ executado  R script : $tempo]--"
echo "R: $nome ini/fim: $t1 / $tempo. Total: $elapsed" >>log

# execucao do script gnuplot
timer
t1=$tempo
echo "--[ executando GNUPLOT script : $t1]--"
gnuplot 'graf.gpl'

timer
echo "--[ executado  GNUPLOT script : $tempo]--"
echo "G: $nome ini/fim: $t1 / $tempo. Total: $elapsed " >>log

comprime
echo "--[ verificando.... ]--"
verifica
echo "--[ done! ]--"
echo ""
#mv $file usado/
done

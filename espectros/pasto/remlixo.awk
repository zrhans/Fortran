#!/usr/bin/awk -f
#*********************************************************************
#LABORATORIO DE MICROMETEOROLOGIA - UNIV. FED. SANTA MARIA - BRASIL  *
#
#  Script para remocao de caracteres (* e ?) em arquivos de dados    *
#                                                                    *
# Desenvolvido por: Hans Rogerio Zimermann -  hans@w3.ufsm.br        *
# Ultima atualizacao: 14-12-05                                       *
#*********************************************************************

{ 
  
  for(k=1;k<=NF;++k)
  {if (($k~/\*/)||($k~/\?/)||($k~/^[na]/)) $k="NA"}{print $i;$i++}
  ++k; 
}







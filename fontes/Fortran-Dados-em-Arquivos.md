
# Fortran - Dados em Arquivos

*Autor: [Hans Zimermann](http://portalfisica.com)*

## Aula dia 31-05-2017
----

O objetivo desta aula é ensinar os princípios de criação, escrita e leitura de dados em arquivos usando a Linguagem de programação Fortran. Para tanto, empregamos um problema matemático básico para gerar os dados a serem escritos em um arquivo. A proposta foi o seguinte exercício apresentado na Figura 1:

![Figura 1 - Proposta de exercício](http://i.imgur.com/0dZ4aZw.png)

O código fonte criado em fortran:

```fortran
!--------------------------------------------------------
! Arquivo: aula-31052017.f90
! Programa para calcular f(x) = x^2 no intervamo x[-5,5]
!
! Autor: Hans Z
! Data: 31-05-2017
!------------------------------------------------------
program aula_31052017
implicit none
! declaração de constantes e variáveis
integer :: t
real :: ft = 0.0

! Criando o arquivo
open(15, file='dados.txt')

! Repetindo no intervalo -5 até 5
do t = -50, 50

  !Calculando a função x²
  ft = t**2

  ! Escrevendo x² no arquivo
  write(15,100) t, ft
  
  ! Mostrando na tela o valor de x²
  print 100, t, ft

end do

100 FORMAT(I3,',',F8.2)

! Fechando o arquivo
close(15)

end program aula_31052017
```


O programa python a seguir, fora do escopo do Fortran, tem a finalidade de criar um gráfico com os dados gerados pelo programa Fortran e que foram salvos no arquivo ASCII `dados.txt` gerando a Figura 2.

```py
import io
import pandas as pd
import matplotlib.pyplot as plt
%matplotlib inline
import requests

url = 'http://dl.dropboxusercontent.com/s/kj5w60vnmd5ikgn/dados.txt'
r = requests.get(url) # envia o resquest e pega o retorno em uma só vez
text = r.text # o metodo text retorna o html como string
dados = pd.read_csv(io.StringIO(text),header=None,index_col=0, sep=',')
plt.figure(figsize=(16,8))
plt.title("Figura do execício de aula dia 31-05-2017 - FSC1004 \n Fonte: portalfisica.com")
plt.ylabel("$F(t)\;[s^2]$")
plt.xlabel("$t\;[s]$")
plt.plot(dados,label="$t^{2}$")
plt.legend(loc='right') # Legend sempre após o plot e usar o label no plot
plt.grid()
plt.savefig('./figura.png')
plt.show()
```

![Figura 2 - Gráfico da função proposta no exercício](https://i.imgur.com/LLn4zjh.png)


## Referências
----
* [pandas](http://pandas.pydata.org/pandas-docs/stable/io.html#csv-text-files)
* [portalfisica.com](http://portalfisica.com)

#FORTRAN
---

Repositório com exemplos de códigos em FORTRAN

Este Repositório contém, em pastas, os códigos fonte criados pelos alunos
da disciplina **FSC1004 - Computação Básica para Física - FORTRAN**.

##### Estrutura de Pastas: 

    FSC1004-YYYY-S__matricula/
    │
    ├── aulas/
    ├── atividades/
    └── etc/


## DICAS GERAIS
 
- [x] @zrhans, #refs, [links](), **formatting**, and <del>tags</del> supported
- [x] *list* syntax ~~required~~ (any unordered or ordered list supported)
- [x] _this is a complete item_
- [ ] __this is an incomplete item__
 
 
## Automatizando a publicação no Repositório

Use o secript **`publica`** digitando no terminal:

```bash
./publica
```

Conteúdo do arquivo (script shell) `publica`

```bash
#!/bin/bash
git pull;
git add .;
git commit -m "Edicao";
git push;
```
 
### Demais dicas
---
 Para saber qual versão do Red Hat / CentOS que está instalada é bem simples, execute o comando

 ```bash 
 #cat /etc/redhat-release```

 

##Instalação do Compilador
----

###CENTOS

Installing GNU 4.4 of C, C++ and gfortran for CentOS 5
Something I forget the package names of these essential compilers for 
**CentOS 5**

 ```bash
 #yum install gcc44 gcc44-c++ gcc44-gfortran
 ```

Caso tenha problemas, tente

 ```bash
 #yum install gcc gcc-c++ gcc-gfortran```

###UBUNTU

Installing gfortran for UBUNTU
Something I forget the package names of these essential compilers for 
 
 ```bash
 #sudo apt-get install gfortran
 ``` 
---
###Compilar o código fonte:
```bash
gfortran -std=f95 main.f95 -o demo 2>&1
```
**Resumidamente:**
```bash
gfortran codigo_fonte.f95 -o executavel
```
Caso a compilação tenha sido feita com sucesso, foi criado um arquivo executável que pode ser rodado com o seguinte comando:
```bash
./executavel
```

>**Nota**:
   The GNU Fortran compiler uses the Fortran 77 standard by default, so the input file must have the .f90 (or of later standard) suffix, so that the compiler can guess the desired standard automatically. Alternatively you can supply the -ffree-form option with the usual .f suffix to enable free-form format instead of the fixed-form format used by the Fortran 77 standard.
   
   
   #Compilando
---

##Unix 
Há vários compiladores Fortran disponíveis. O mais popular é o gfortran (*GNU Fortran compiler*).
Para compilar o código fonte **`exemplo.f95`**, digitamos na linha de comando o seguinte código:    
    
    gfortran -o exemplo exemplo.f95
    

> **Nota:** O compilador gfortran usa como padrão o Fortran 77, portanto, seu código fonte deve
utilizar a extensão `.f90` ou `.f95`, desta forma o compilador poderá selecionar automaticamente as bibliotecas
adequadas para a versão do seu código fonte. 

Como alternativa, voce pode utilizar a opção `-ffree-form` e o seu arquivo poderá teer a extensão usual <code>.f</code>
usada pelo fortran 77.

Uma vez que seu código fonte tenha sido compilado com sucesso, você pode executá-lo digitando no terminal
o seguinte comando:

    ./exemplo
 
---
###Ex:

1. código fonte:

        PROGRAM exemplo01
        IMPLICIT NONE
        REAL :: temperature
        INTEGER :: cows
        temperature = 18.6
        cows = 9
        WRITE (*,*) "Existem ", cows, " no curral."
        WRITE (*,*) "Agora fazem ", temperature, " graus"
        END PROGRAM exemplo01
    
2. Compilando:
    
        mauro_rafael@fortran:~/workspace$ gfortran exemplo-01.f95 -o exemplo-01
    
3. Executando:

        mauro_rafael@fortran:~/workspace $ ./exemplo-01 
    
4. Resultado:

        mauro_rafael@fortran:~/workspace $ ./exemplo-01 
         Existem            9  no curral.
         Agora fazem    18.6000004      graus
        mauro_rafael@fortran:~/workspace $ 
        
        
        

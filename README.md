FORTRAN
=========

Repositório com exemplos de códigos em FORTRAN

Este Repositório contém, em pastas, os códigos fonte criados pelos alunos
da disciplina FSC1004 - Computação Básica para Física - FORTRAN.

##### Estrutura de Pastas: 

    FSC1004-YYYY-S__matricula/
    │
    ├── aulas/
    ├── atividades/
    └── etc/


## DICAS GERAIS
===============
 ## Automatizando a publicação no Repositório

Use o secript `push` digitando no terminal:

```bash
./push
```

Conteúdo do arquivo (script shell) `push`

```bash
#!/bin/bash
git pull;
git add .;
git commit -m "Edicao";
git push;
```
===========
### Demais dicas
---
 Para saber qual versão do Red Hat / CentOS que está instalada é bem simples, execute o comando

 ```bash 
 #cat /etc/redhat-release```

---
Installing GNU 4.4 of C, C++ and gfortran for CentOS 5
Something I forget the package names of these essential compilers for  CentOS 5

 ```bash
 #yum install gcc44 gcc44-c++ gcc44-gfortran```

Caso tenha problemas, tente

 ```bash
 #yum install gcc gcc-c++ gcc-gfortran```

---
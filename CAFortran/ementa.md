## UNIDADE 1 - INTRODUÇÃO AO PROCESSAMENTO DE DADOS  
    1.1 - Estrutura de computadores. 
    1.2 - Representação de números na memória. 
    1.3 - Conceito de algoritmo e fluxograma. 
    1.4 - Tipos de sistemas operacionais. 
    1.5 - Tipos de linguagens de programação. 
    1.6 - Linguagem Fortran.   
        1.6.1 - A evolução da linguagem Fortran.   
        1.6.2 - Sintaxe de programas em Fortran.   
        1.6.3 - Formatos livre e fixo.   
        1.6.4 - Comandos de edição, compilação e execução.  
## UNIDADE 2 - CONSTANTES VARIÁVEIS E EXPRESSÕES  
    2.1 - Constantes inteiras, reais e caracteres. 
    2.1 - Variáveis inteiras, reais e caracteres. 
    2.3 - Comandos de especificação de variáveis e constantes. 
    2.4 - Comandos de designação de áreas na memória. 
    2.5 - Expressões aritméticas, relacionais e lógicas.  
## UNIDADE 3 - COMANDOS DE CONTROLE DE FLUXO  
    3.1 - Comando GO TO. 
    3.2 - Comandos IF lógico e bloco.  
## UNIDADE 4 - COMANDOS DE ENTRADA E SAÍDA  
    4.1 - Registros, arquivos e unidades. 
    4.2 - Comandos OPEN e CLOSE. 
    4.3 - Comandos READ.
    4.4 - Comandos WRITE. 
    4.5 - Comandos FORMAT e especificação de formato.  
## UNIDADE 5 - SUBPROGRAMAS  
    5.1 - Argumentos de subprogramas. 
    5.2 - Funções intrínsecas. 
    5.3 - Comandos COMMON e BLOCK DATA. 
    5.4 - Funções e comando FUCTION. 
    5.5 – Sub-rotinas e comando SUBROUTINE.  
## UNIDADE 6 - APLICAÇÕES SIMPLES  
    6.1 - Solução de equações de segundo grau. 
    6.2 - Cálculo de médias e desvio padrão. 
    6.3 - Multiplicação de matrizes. 
    6.4 - Derivação e integração numéricas. 
    6.5 - Método de Euler para equações diferenciais. 

The g95-MinGW.exe g95 Installer
===============================

G95-MinGW.exe is a self-extracting installer for the free GNU g95 Fortran compiler. It works on Windows VISTA, XP, 2000, ME, 95 and 98. 

The g95-MinGW.exe installer allows the user to specify any install directory for g95. However, if MinGW is already installed on your system, it is best to install g95 in the root MinGW directory. 

If MinGW is installed, install g95 in your mingw directory and include the mingw\bin directory in your PATH. 

On systems without MinGW, g95 can be installed along with an additional set of programs and libraries from the MinGW distribution that are needed to run g95. All these programs and libraries are included in the self-extracting installer. They will be installed for you if you respond by clicking the "Ok" button when the installer asks: "Install MinGW Utilities and libs?" 

Responding with "Ok" is recommended even when installing g95 into a MinGW directory structure, as doing so will ensure you have fairly recently updated versions of these files. The PATH and the LIBRARY_PATH environment variables should be properly set up to find the g95 bin and lib directories. The LIBRARY_PATH environment variable should be set to the g95 lib directory. (G95_LIBRARY_PATH is used by the g95-MinGW-41.exe installer to prevent conflicts with other programs.) 

The g95 installer contains all the libraries and programs needed to successfully use the g95 compiler, and produce executables for Windows systems. These include the GNU linker, ld.exe, and assembler, as.exe from the MinGW binutils package, as well as numerous libraries essential for g95.

About MinGW
===========

MinGW ("Minimalistic GNU for Windows") provides an environment for compiling and linking code based on the GNU GCC and binutils projects to be run on Win32 platforms. Further information, and the complete distribution of MinGW 
is available from:

  http://www.mingw.org/download.shtml.

Requirements
============

* About 15 MB free space on your hard drive.

INSTALLING THE MINGW G95 FORTRAN COMPILER
=========================================

The g95-MinGW.exe g95 installer extracts the files needed for g95 to run on supported Windows systems with or without MinGW already installed. Where MinGW is not installed a minimal set of support files from the MinGW distribution and the MinGW binutils package is required. These can also be installed if installing into MinGW. Doing this, especially if it is a the first time g95 is installed, will ensure that all the files that g95 needs to run are available immediately. To do this, when the message box asks: "Install MinGW Utilities and libs?", respond by clicking "Ok".

The PATH environment variable may then be set to include the path to MinGW and the g95 executable, eg c:\g95\bin.

Avoid directory names containing spaces, e.g. "c:\Program Files\.." 

Avoid duplicate entries in your PATH.

If you are installing G95-MinGW.exe, set the LIBRARY_PATH environment variable to the location of your MinGW\lib directory. If you downloaded the g95-MinGW-41.exe installer, set G95_LIBRARY_PATH environment variable to the location of your MinGW\lib directory, eg c:\g95\lib.

G95 also uses the environment variable TMP for saving temporary files. TMP should be set to point to a directory on your system.  

INSTALLING ON WINDOWS 95/98/ME
==============================

In Windows 95/98/Me the PATH is usually set in the autoexec.bat file, which is stored in the root directory of your hard disk; i.e. c:\. Edit this file and add the following lines:

  PATH=<MinGW_bin_path>;<g95_install_path>;%PATH%
  
  SET LIBRARY_PATH=<MinGW_library_path>
  
where <MinGW_bin_path> is the full path to the MinGW\bin directory, <g95_install_path> is the path to the directory containing g95.exe, and <MinGW_library_path> is the full path to the MinGW\lib directory.
      
You can do this by opening this file in Notepad or any other editor, adding the above line at the end, and then saving the changes. You must reboot for the changes to take effect.

INSTALLING ON WINDOWS NT/2000/XP
================================

In Windows NT/2000/XP the PATH variable can be set by going to the Control Panel, selecting System, and then locating the environment (or advanced, environment) section. 

Add a variable named LIBRARY_PATH and set its value to <MinGW_library_path>, i.e., the full path to the MinGW\lib directory.

Similarly add a new variable named PATH (or edit it if already present) and set its value to:

   <MinGW_install_path>;<g95_install_path>;%PATH%
    
Alternatively, place the following line in a batch file, and run it before invoking g95.

   SET PATH=<MinGW_install_path>;<g95_install_path>;%PATH%
   SET LIBRARY_PATH = <MinGW_library_path>

Replace <MinGW_install_path> with the full path to the MinGW\bin directory, and <g95_install_path> with the path to the directory containing g95.exe, and <MinGW_library_path> with the full path to the MinGW\lib directory.

INSTALLING ON WINDOWS VISTA
===========================

For Windows VISTA, in addition to the procedures outlined above for Windows XP, add the directory:

<g95_install_path>\lib\gcc-lib\i686-pc-mingw32\4.x.x

to both the PATH and LIBRARY_PATH environment variables, where <g95_install_path> is the directory where g95 is installed. Change "4.x.x" to the appropriate directory for the g95 version you installed.

The GNU g95 Fortran Compiler
============================
       
The g95 website URL is: http://www.g95.org. 

For license details see the file COPYING.txt included in this package.

The source code for g95 is available at: http://ftp.g95.org/g95_source.tgz

Bug reports should be sent to Andy Vaught: andyv@firstinter.net

Support
=======

For user support try posting a message on the g95 message board at:
http://groups-beta.google.com/group/gg95

Please include appropriate details such as:

  * the platform and OS you are using 
  * the build of g95 (use 'g95 -v' and post the output)
  * any compiler options used  
  * the error message
  * example code snippet that caused the error
  * specify the version of Cygwin or MinGW used
  * output from the command "echo $PATH" in a Msys shell
  * any other pertinent information
  
Documentation
=============

A manual for using the G95 compiler is provided in the doc directory. This can be viewed using the free Acrobat Reader or in a word processor that supports pdf.
  
AUTHOR
======

The script for building the g95-MinGW.exe installer was written by Doug Cox, e-mail tcc@sentex.net.
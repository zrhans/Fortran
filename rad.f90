! Faça um programa em Fortran que leia um valor para um comprimento de onda e 
! indique em qual faixa do espectro eletromagnético ela está. 
! Baseado nesta informação, calcule e mostre:
! a) A frequência
! b) A energia 
! c) A classificação espectral dessa radiação.
! Dica: Use as informações disponíveis em 
!       https://pt.wikipedia.org/wiki/Espectro_visivel.

program rad
implicit none

real,parameter :: plank = 6.626e-34, vel_luz = 3.0e8 ! constante de Planck
real :: energia, freq = 0.0
real :: comp_onda

print*,
print*, "============================================="
print*, " Forneça um valor para o comprimento de onda "
print*, " em metros."
print *, "---------------------------------------------"
read *, comp_onda
print*,

if (comp_onda .ge. 380e-9) then
    if (comp_onda .lt. 450e-9) then
        print *, "Cor violeta Freq. (668-789 THz)"
    else if (comp_onda .lt. 495e-9) then
        print * ,"Cor azul Freq. 606–668 THz" 
    else if (comp_onda .lt. 570e-9) then
        print * ,"Cor verde Freq. 526–606 THz"
    else if (comp_onda .lt. 590e-9) then
        print *, "Cor amarelo Freq. 508–526 THz"
    else if (comp_onda .lt. 620e-9) then
        print *, "Cor Laranja  Freq.484–508 THz"
    else if (comp_onda .lt. 750e-9) then
        print *, "Cor vermelho Freq. 400–484 THz"
    else
        print *, "Faixa espectral do infravermelho Freq abaixo dos 400 THz"
    endif
else
    print *, "Faixa espectral do ultravioleta Freq acima dos 789 THz"
endif

! http://pt.wikihow.com/Calcular-Comprimento-de-Onda

! Cálculo da Frequência da radiação
! freq.lambda = vel_luz

freq = vel_luz / comp_onda

! Cálculo da Energia do fóton
! E = hv

energia = plank * freq

 print *,
 print *,"Frequência (vl = c [Hz])   : ", freq, " Hz "
 print *,"Energia (E = hv [J.s * Hz]): ", energia, " J "

 print *,
 print *," |======= FORMATADO =======|"
 ! rESw.d
 ! w ≥ d + 7 (http://portalfisica.com/academico/fsc1004/unidade-03/formatacao-de-entrada-e-saida/)
 print 100,"Frequência (vl = c [Hz])   :", freq, "Hz"
 print 100,"Energia (E = hv [J.s * Hz]):", energia, "J"

100 format (A,1x,es10.2,1x,A)

end program rad

! Desafio: Faça um programa usando estrutura de seleção select case
!          para mostarar um texto explicativo das consequencias
!          da radiação de acordo com o valor do comprimento de onda fornecido
!          e a escala de Chang (1968):
! O efeito que a radiação exerce sobre as plantas varia conforme o comprimento de onda. Segundo Chang (1968), radiações com comprimento de onda:
! - até 0,28 μm, provocam rapidamente a morte das plantas (ultravioleta);
! - de 0,28 a 0,40 μm, são bastante nocivas até 0,32 μm e acima desse limite provocam inibição do crescimento (ultravioleta e violeta);
! - de 0,40 a 0,51 μm, têm acentuada absorção pela clorofila e xantofila (correspondem, aproximadamente, às cores índigo e azul);
! - de 0,51 a 0,61 μm, abrangendo praticamente as cores verde e amarela, exercem pouca influência no processo fotossintético;
! - de 0,61 a 0,72 μm, coincidem aproximadamente com as cores laranja e vermelha e possuem acentuada ação fotossintética;
! - de 0,72 a 1,0 μm, interferem na elongação, floração, coloração dos frutos e germinação das sementes;
! - acima de 1 μm, ao que se sabe, não exercem nenhum papel especial. Quando absorvidas são usadas nos processos bioquímicos.
! e de acordo com as aplicações físicas na tabela fornecida por 
! http://www.fazendovideo.com.br/ondas_new.gif
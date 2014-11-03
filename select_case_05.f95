program select_case_05
implicit none

integer :: altura
character (len=1000) :: texto

 

print*, 'Digite em metros a altitude (positivo) ou profundidade (negtivo) :  '

read *, altura

texto = 'Como qualquer outro ser vivo, o homem evoluiu para viver em &
        &determinadas condições de clima e pressão atmosférica - mas não &
        &consegue se conformar só com seu habitat. Impulsionado pela vontade de&
        &superar limites, ele testa sua capacidade de sobrevivência em &
        &ambientes inóspitos como nenhum outro animal faria. Aventurar-se no &
        &alto de uma montanha ou no fundo do mar exige que o organismo humano &
        &se acostume às diferenças no comportamento do gás essencial à vida, o &
        &oxigênio.&
        &Quanto maior a altitude, menor a quantidade de oxigênio, pois o ar &
        &fica menos denso, com mais espaços vazios entre as moléculas.'

write(*,*) texto
        
texto = 'A pressão atmosférica diminui, causando dor de cabeça,& náuseas e     &
& prostração. Já em grandes profundidades, o perigo é a pressão sobre o peito  &
&do mergulhador, um obstáculo ao trabalho dos músculos na respiração. Até a    &
&volta pode ser perigosa, tanto das alturas quanto das profundezas. É que a    &
&coordenação motora, a lucidez e a capacidade de raciocínio rápido ficam       &
&comprometidas&
&Limites à prova &
&No mar ou na montanha, saiba o esforço que seu corpo faz para controlar a     &
& respiração.'

write(*,*) texto


 select case (altura)
    case (8000:)
       texto = "8000 metros - Zona fatal&
&No topo do Monte Everest, o pico mais alto do mundo, o aventureiro consegue &
&respirar apenas 30% do oxigênio necessário. Os poucos alpinistas que &
&conseguiram chegar lá sem tubos de oxigênio agüentaram uma hora e meia, no &
&máximo. Mais do que isso, é morte certa. "

    case (5000:7999)
       texto = "5000 metros - Alerta vermelho&
&A capacidade de adaptação do organismo diminui muito. Aumenta o risco de &
&edemas por acúmulo de líquidos no pulmão ou no cérebro."

    case (3600:4999)
       texto = "3600 metros - Sufoco andino &
&La Paz, na Bolívia, é a capital mais alta do mundo. Seus 700 000 habitantes&
& estão acostumados ao ar rarefeito da Cordilheira dos Andes. Lá, é comum mascar&
& folhas de coca (foto) para atenuar os efeitos da altitude. "

    case (2800:3599)
        texto = "2800 metros - O corpo sofre &
&O organismo começa a responder à redução do oxigênio. No início, você passa a &
respirar mais depressa e mais profundamente. A freqüência cardíaca também &
aumenta, para distribuir o oxigênio a todas as células com mais eficiência. &
A partir do terceiro dia nesta altitude, o corpo se adapta e começa a produzir &
mais emoglobina, a molécula sangüínea que transporta o oxigênio."
 
     case (1:2799)
        texto = "Nível do mar em diante &
& A pressão decresce com a altitude."
 
    case (0)
        texto = "Nível do mar &
&Aqui, você tem uma coluna de ar sobre a cabeça, que corresponde a 1 atmosfera,&
&a unidade de medida da pressão atmosférica."

    case (-1:-10)
        texto = " -10 metros - Mergulhar de cabeça &
& A cada 10 metros que você desce, a pressão aumenta em 1 atmosfera. O tímpano,&
& a membrana do ouvido, pode ser empurrado para dentro, provocando dor. Para   &
& que ele não se rompa, é preciso fazer a chamada manobra de Valsava: tape o &
& nariz e a boca e faça força para expirar até que as pressões se igualem."
        
    case (-40:-9)
        texto = " -40 metros - O canto das sereias &
& Nesta profundidade, a pressão é de 5 atmosferas. Os mergulhadores precisam &
& usar um cilindro de ar comprimido. O nitrogênio, aqui, é um vilão. Ele  &
& interfere nos estímulos nervosos causando a embriaguez das profundidades. &
& Se o retorno à superfície for muito rápido, acontece a embolia, que é a  &
& formação de bolhas no sangue. &
& Resultado: deformação ou, até mesmo, rompimento do pulmão."
        
    case (:-300)
        texto = " -300 metros - Trabalho submarino &
& Para trabalhar nas plataformas de exploração de petróleo submarino, os &
& mergulhadores precisam adaptar-se lentamente em câmaras especiais, onde &
& respiram uma mistura de hélio, oxigênio e nitrogênio."
        
    case default
       texto = "CURIOSIDADES: Emergência a bordo&
& Seria impossível viajar de avião, acima dos 10 000 metros, se as cabines não &
& fossem supridas de ar pressurizado. Nessa altitude, a quantidade de oxigênio &
& no ar é insuficiente. Se, por algum motivo, ocorre uma queda de pressão, uma &
& máscara garante o suprimento necessário de oxigênio (foto). Do contrário,    &
& os passageiros perderiam a consciência em poucos minutos.&
& O recorde oficial de profundidade para a perigosa e&
& desaconselhável atividade do mergulho sem equipamento é de 130 metros,       &
& obtido pelo cubano Francisco Pipún Ferras, ao largo do Cabo San Lucas,       &
& México, no dia 10 de março de 1996.&
&Fonte: http://super.abril.com.br/saude/pressao-altos-baixos-443004.shtml"

 end select



print*, texto

 


stop '>>> Progrma finalizado'
end program select_case_05
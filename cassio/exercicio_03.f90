!#### atribuindo caracteres ####

program exercicio_03

implicit none

!definicao de caracteres

character*30 :: a, b

character*255 :: concatenar

!atribuicao de caracteres

print*, "Digite a primeiro frase"

read '(A)', a

print*, "Digite a segundo frase"

read '(A)',b

!definicao de expressao

concatenar = trim(a) // " " // trim(b)

!exibicao de expressao

print*, concatenar

end program exercicio_03

#em uma determinada rua ocorrem em média 2 acidentes de carro por dia
#calculo da probabilidade de ocorrer 3 acidentes
dpois(3, lambda=2)
#calculo da probabilidade de ocorrer 3 ou menos
ppois(3, lambda = 2)
#calculo da probabilidade de ocorrer mais que 3
1 - ppois(3, lambda = 2, lower.tail = FALSE)
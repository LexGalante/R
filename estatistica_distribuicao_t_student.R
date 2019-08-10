# Media 75
# Amostra 9
# Desvio padrao 10
# T 1.5
# Probrabilidade de achar algo menor que 75 
probabilidade_menor_que_75 <- pt(1.5, 8) * 100
# Probrabilidade de achar algo maior que 75
probabilidade_maior_que_75 <- pt(1.5, 8, lower.tail = FALSE) * 100
# Checando resultador
probabilidade_total <- probabilidade_menor_que_75 + probabilidade_maior_que_75
print(c(probabilidade_menor_que_75, probabilidade_maior_que_75, probabilidade_total))

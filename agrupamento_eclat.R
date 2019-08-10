library(arules)
#carregando a base de aprendizado
transacoes <- read.transactions(file = "/dados/transacoes2.txt", format = c("basket"))
#visualizando objeto arules
inspect(transacoes)
image(transacoes)
#extraindo as regras de associacao, definindo suporte e a confiança
regras <- eclat(transacoes, parameter = list(supp = 0.1, maxlen = 15))
#visualizando as regras
inspect(regras)
#instalando pacote arules
install.packages("arules", dependencies = TRUE)
library(arules)
#carregando a base de aprendizado
transacoes <- read.transactions(file = "/dados/transacoes.txt", format = c("basket"))
#visualizando objeto arules
inspect(transacoes)
image(transacoes)
#extraindo as regras de associacao, definindo suporte e a confiança
regras <- apriori(transacoes, list(supp = 0.5, conf = 0.5))
#visualizando as regras
inspect(regras)


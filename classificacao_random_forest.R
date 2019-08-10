#instalando o pacote random forest
install.packages("randomForest", dependencies = TRUE)
library(randomForest)
credito <- read.csv("dados/credit.csv",
                    sep = ",",
                    header = TRUE)
#carregando dados historicos para analise do modelo
credito <- read.csv("dados/credit.csv",
                    sep = ",",
                    header = TRUE)
#OBJETIVO: Construir um modelo para prever se um cliente será um bom pagador
#vetor para retira da amostragem da bases
amostra <- sample(2, 1000, replace = TRUE, prob = c(0.7, 0.3))
#separando a base de treino
credito_treino <- credito[amostra==1,]
#separando a base e teste
credito_teste <- credito[amostra==2,]
#criando o modelo
x11()
modelo <- randomForest(class ~ .,#formula
                         data = credito_treino,#dados
                         ntree = 100,#numero de arvores criadas
                         importance = TRUE#?
                      )
varImpPlot(modelo)
#previsao
previsao <- predict(modelo, credito_teste)
confusao <- table(credito_teste$class, previsao)
acuracia <- (confusao[1] + confusao[4]) / sum(confusao) * 100
erro <- (confusao[2] + confusao[3]) / sum(confusao) * 100
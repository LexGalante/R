#instalando o pacote
install.packages("rpart", dependencies = TRUE)
library(rpart)
#carregando dados historicos para analise do modelo
credito <- read.csv("/dados/credit.csv",
                    sep = ",",
                    header = TRUE)
#OBJETIVO: Construir um modelo para prever se um cliente será um bom pagador
#vetor para retira da amostragem da bases
amostra <- sample(2, 1000, replace = TRUE, prob = c(0.7, 0.3))
#separando a base de treino
credito_treino <- credito[amostra==1,]
#separando a base e teste
credito_teste <- credito[amostra==2,]
#criando o modelo de arvore decisão
#1 Parametro: seletor de atributos
#2 Parametro a base de treino
#3 Parametro CLASS para criar uma arvore de descisão ANOVA para criar uma regressão
modelo <- rpart(class ~ ., data = credito_treino, method = "class")
#visualizando modelo em forma grafica
X11()
plot(modelo)
text(modelo, use.n = TRUE, all = TRUE, cex=0.8)
#calculando a acuracia do modelo
teste <- predict(modelo, newdata = credito_teste)
credito_analise <- cbind(credito_teste, teste)
credito_analise['resultado'] <- ifelse(credito_analise$bad >= 0.25, "bad", "good")
#criando a matriz de confusao para verificar a acuracia do modelo
confusao <- table(credito_analise$class, credito_analise$resultado)
#verificando resultados do modelo
acuracia <- (confusao[1] + confusao[4]) / sum(confusao) * 100
erro <- (confusao[2] + confusao[3]) / sum(confusao) * 100
#excutando modelo com dados de produção
cliente <- read.csv("/dados/novo_credit.csv",
                    sep = ",",
                    header = TRUE)
cliente_previsao <- predict(modelo, cliente)

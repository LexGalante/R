#algoritmo disponivel na lib e1071
library(e1071)
library(caret)
#carregando os dados
espectro_politico <- read.csv(file.choose(), sep = ";")
#analises exploratorias
dim(espectro_politico)
summary(espectro_politico)
#separando base de treino e base de teste
#vetor para retira da amostragem da bases
amostra <- sample(2, 1000, replace = TRUE, prob = c(0.7, 0.3))
treino <- espectro_politico[amostra==1,]
teste <- espectro_politico[amostra==2,]
#criando o modelo
modelo.svm <- svm(classe ~ ., data = treino, kernel = "radial", gamma = 0.5, const = 0.1)
#criando predições
previsao <- predict(modelo.svm, teste)
#criando a matriz de confusao
confusao <- table(teste$classe, previsao)
#visualização na lib caret
confusao <- confusionMatrix(confusao)
#verificando melhores parametros para o modelo
modelo.tune = tune(svm,
                   train.x = treino,
                   train.y = teste,
                   kernel = "radial",
                   ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))



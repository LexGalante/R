#Instalando o pacote Neural net
install.packages("neuralnet", dependencies = TRUE)
library(neuralnet)
#preparando binarizacao de iris
myiris <- iris
myiris <- cbind(myiris, myiris$Species=='setosa')
myiris <- cbind(myiris, myiris$Species=='versicolor')
myiris <- cbind(myiris, myiris$Species=='virginica')
summary(myiris)
#renomeando as colunas
names(myiris)[6] <- 'setosa'
names(myiris)[7] <- 'versicolor'
names(myiris)[8] <- 'virginica'
#preparando amostragem para teste
amostra <- sample(2, 150, replace = TRUE, prob = c(0.7, 0.3))
treino <- myiris[amostra==1,]
teste <- myiris[amostra==2,]
#criando o modelo de rede neural
modelo <- neuralnet(setosa + versicolor + virginica ~
                      Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                    treino,
                    hidden = c(5, 4))
#plot do modelo
plot(modelo)
#testando o modelo
testando <- compute(modelo, teste[, 1:4])
resultado <- as.data.frame(testando$net.result)
names(resultado)[1] = 'setosa'
names(resultado)[2] = 'versicolor'
names(resultado)[3] = 'virginica'
#preparando matriz de confusao criando a coluna com nome da coluna com maior valor
resultado$class = colnames(resultado[, 1:3])[max.col(resultado[, 1:3], ties.method='first')]
confusao <- table(resultado$class, teste$Species)
acuracia <- sum(diag(confusao) * 100 / sum(confusao))

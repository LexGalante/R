#intalando pacote com KNN
install.packages("class", dependencies = TRUE)
library(class)
#OBJETIVO: classificador a instancia conforme proximidade com dados historicos utilizando
#K NEAREST NEIGBHOR
amostra <- sample(2, 150, replace = TRUE, prob = c(0.7, 0.3))
iris_treino <- iris[amostra==1,]
dim(iris_treino)
iris_teste <- iris[amostra==2,]
dim(iris_teste)
#criando a classificacao knn
previsao <- knn(train = iris_treino[,1:4],#1 Parametro base de treino
                test = iris_teste[,1:4],#2 Parametro base de teste
                cl = iris_treino[,5],#3 Classe para classificacao
                k = 3#4 Distantcia dos vizinhos para classificacao
            )
confusao <- table(iris_teste[,5], previsao)
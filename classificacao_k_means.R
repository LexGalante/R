dim(iris)
#criando o modelo
modelo <- kmeans(iris[1:4], centers = 3)
#verificando a acuracia
acuracia <- table(iris$Species, modelo$cluster)
#grafico de dispersao do cluster
X11()
plot(iris[,1:4], col=modelo$cluster)
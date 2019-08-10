#neste algoritmo uma instancia pode pertencer a mais de grupo
#disponivel no pacote e1071
library(e1071)
#criando o modelo
modelo <- cmeans(iris[1:4], centers = 3)
#verificando a acuracia
acuracia <- table(iris$Species, modelo$cluster)
#grafico de dispersao do cluster
X11()
plot(iris[,1:4], col=modelo$cluster)
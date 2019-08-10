#disponivel no pacote cluster
install.packages("cluster", dependencies = TRUE)
#carregando a lib
library(cluster)
#criando o modelo
modelo <- pam(iris[1:4], k=3)
#verificando a acuracia
acuracia <- table(iris$Species, modelo$clustering)
#grafico de dispersao do cluster
X11()
plot(modelo)

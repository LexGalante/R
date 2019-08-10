#tecnicas para busca de anomalias, execeções, fraudes etc...
boxplot(iris$Sepal.Width)
boxplot(iris$Sepal.Length)
#buscaras variaveis especificas em outliers
boxplot.stats(iris$Sepal.Width)$out
#pacote para trabahar com outliers
install.packages("outliers")
library(outliers)
#buscando outliers superior
outlier(iris$Sepal.Width)

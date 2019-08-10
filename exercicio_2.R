#exercício prático de R

# Liste o direório corrente
getwd()
# Liste os arquivos do diretorio
dir()
# Pesquise uma função para criar um diretório
apropos("dir")
# pesquise o manual para essa funçao
help("dir.create")
# Crie um diretório com o nome "exercicioR"
dir.create(path="dirname")
# Entre no diretório criado
#setwd(file="C:/Users/Alex/Desktop/Pós/Regressões/exercicioR")
# crie uma função que multiplique dois valores
multiplicacao <- function(a,b) {
  return(a*b)
}
# crie uma funcçao que subtraia dois valores
subtracao <- function(a,b) {
  return(a-b)
}
# crie uma funcao que eleve um valor ao quadrado
quadrado <- function(a,b) {
  return(a^2)
}
# pesquise uma função que copie os arquivos e consulte a sua utilização
apropos("file.cop")
# copie o arquivo carros.csv da area de trabalho para a pasta.
file.copy(from="dados/carros.csv",
          to="dados/carros.csv")
#Objetivo: fazer a regressão linear de mpg em relaçao a hp
#Há relaçao entre as variáveis?
#SIM
# carregue o arquivo para um data frame
dados <- read.csv(file="dados/carros.csv",
                   header=TRUE,
                   sep=";",
                  dec=",")
# qual o sentido?
## INVERSO
# veja o resumo do data frame
summary(dados)
str(dados)
# os dados OK?
##NÃO
#por que?
##PARA CONSUMO EXISTEM ITEM QUE NÃO SÃO VALOR NUMERICO
#se não estiver ok, remova os dados inconsistentes do data frame
dados <- dados[-which(dados$CONSUMO %in% c(NA)),]
#plote os gráficos de dispersão e os histogramas das variáveis hp e mpg
plot(dados$CONSUMO,dados$POTENCIA)
#calcule a media da coluna mpg e atribua a uma variável mean_y
media_consumo <- mean(dados$CONSUMO)
#calcule a media da coluna hp e atribua a uma variável mean_x
media_potencia <- mean(dados$POTENCIA)
#crie um vetor com a diferenca entre o valor e sua média (xi - mean_x) utilizando a funcao de diferenca criada
vetor_media_consumo <- dados$CONSUMO - media_consumo
#crie um vetor com a diferenca entre o valor e sua média (yi - mean_y) utilizando a funcao de diferenca criada
vetor_media_potencia <- dados$POTENCIA - media_potencia
#crie um vetor com o quadrado da diferenca de x criado utilizando a funcao de quadrado criada
vetor_quadrado <- quadrado(dados$POTENCIA, vetor_media_potencia)
#crie um vetor com o produto das diferenca de x e y criado utilizando a funcao de subtração criada
vetor_produto <- subtracao(dados$CONSUMO, dados$POTENCIA)
#concatene os vetores criados com o data frame
dados <- cbind(dados, indice = c(1:length(dados$MARCA)))
vetor_media_consumo <- cbind(vetor_media_consumo, indice_media_consumo = c(1:length(dados$CONSUMO))) 
vetor_media_potencia <- cbind(vetor_media_potencia, indice_media_potencia = c(1:length(dados$POTENCIA))) 
vetor_quadrado <- cbind(vetor_quadrado, indice_quadrado = c(1:length(dados$CONSUMO))) 
vetor_produto <- cbind(vetor_produto, indice_produto = c(1:length(dados$CONSUMO)))
dados <- merge(dados, vetor_media_consumo , by.x="indice", by.y = "indice_media_consumo")
dados <- merge(dados, vetor_media_potencia , by.x="indice", by.y = "indice_media_potencia")
dados <- merge(dados, vetor_quadrado , by.x="indice", by.y = "indice_quadrado")
dados <- merge(dados, vetor_produto , by.x="indice", by.y = "indice_produto")
# agora calcule os coeficientes b e a para a reta de regressão com os vetores calculados
#b <- sum_prod_dif_xy/sum_dif_x2
vetor_a1 <- vetor_produto[,1]/vetor_quadrado[,1]
vetor_a1 <- cbind(vetor_a1, indice_a1 = c(1:length(dados$CONSUMO)))
dados <- merge(dados, vetor_a1 , by.x="indice", by.y = "indice_a1")
#a <- y_bar- b*x_bar
vetor_a0 <- dados$vetor_media_potencia - dados$vetor_a1
vetor_a0 <- cbind(vetor_a0, indice_a0 = c(1:length(dados$CONSUMO)))
dados <- merge(dados, vetor_a0 , by.x="indice", by.y = "indice_a0")
# Com os coeficientes criados crie uma função que estime o valor de mpg dado um valor de hp
consumo <- function(a, b, potencia) {
  return(a + b * potencia)
}
# crie um vetor com os valores preditos f(x) os valores de hp
vetor_funcao <- consumo(dados$vetor_a1, dados$vetor_a0, dados$POTENCIA)
# concatene os valores preditos no data frame
vetor_funcao <- cbind(vetor_funcao, indice_funcao = c(1:length(dados$CONSUMO)))
dados <- merge(dados, vetor_funcao , by.x="indice", by.y = "indice_funcao")
# calcule os as variaçoes explicada, nao explicada e total

# Calcule o coeficiente de Determinação

# Calcule a Covariancia

# Calcule a Correlação

# Plote a curva no gráfico de dispersao. Dica: utilize a funcao curve( add = TRUE)

# Plote um histograma com os RESÍDUOS não explicados

# Escreva sobre o que encontrou e seus resultados obtidos

# salve o data frame como csv com o nome resultado_carros.csv

# salve a funcao de regressao para o arquivo linear_carros.R

# salve o script dentro da pasta.

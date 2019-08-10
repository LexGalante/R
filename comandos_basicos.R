# exibição no console
print("Teste")
# ehamada de funções
runif(10)
# chamada de funções com parametros nominais
runif(n = 10, min = 1, max = 10)
# chamada de funções sem parametros nominais
runif(10, 1, 10)
# Ver documentação
help("runif")
# Help resumido
?plot
# checar se um pacote exite
apropos("unif")
# carregar biblioteca
library("ggplot2")
# chamar pacote
# ggplot2::
require(agricolae)
# instalando pacotes do repositorio padrão
install.packages("nome_do_pacote")
# instalando pacotes com depedencias
install.packages("nome_do_pacote", dependencies = TRUE)
# definindo variavel
a <- 2
# Ver váriaveis disponiveis
ls()
# printar variavel
print(a)
# definindo funcao
funcao <- function(){
  print("TESTE")
}
funcao()
# funcao com parametros
soma <- function(x,y){
  return(x+y)
}
soma(x=2,y=2)
# funcao com argumento padrao
multiplicar <- function(x,y=10){
  return(x*y)
}
multiplicar(2)
# tipos primitivos
double() #0.0
integer() #0
numeric() #000000.00
character() # 'a'
logical() # TRUE or FALSE
complex() # 9999999999999999999999999.99
raw() # Bytes
# vetores
vetor <- c(0,1,2,3,4,5,6,7,8,9)
# acessando valores do vetor
vetor[10]
# tamanho do vetor
length(vetor)
# listas multitipadadas
lista <- list(0,'a', 123.99, 0,1,'C', 199.88991)
# acessando elementos de lista
lista[3]
# sequencia para vetores
sequencia <- seq(from=1,to=1000000,by=0.5)
# resumo sequencial
resumo <- 1:100000
# multiplicando vetor
vetor_multiplicado <- vetor * 5
vetor_ao_quadrado <- vetor ^ 2
# tipagem default vetor
soma(vetor_multiplicado,vetor_ao_quadrado)
# converter valores tipados
vetor_string <- as.character(vetor)
vetor_double <- as.double(vetor)
vetor_matrix <- as.matrix(vetor)
# matrizes default
matriz <- matrix(1:100,nrow=10,ncol=10)
# matrizes escrita por linha
matriz <- matrix(1:100,nrow=10,ncol=10,byrow=TRUE)
# adicionado linha a matriz
rbind(matriz, c(0,1,2,3,4,5,6,7,8,9))
# adicionando coluna a matriz
cbind(matriz, c(1,2,3,4,5,6,7,8,9,0))
# tamanho da matriz
length(matriz)
# dimensoes da matriz
dim(matriz)
# acessando elemento da matriz
matriz[1][1]
# acessando coluna inteira
matriz[,2]
# acessando linha inteira
matriz[3,]
# diretorio padrao
getwd()
# setando diretorio padrao
setwd("C:/Users/Alex/Documents")


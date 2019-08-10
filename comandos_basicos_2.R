# le dados digitados pelo usuário
x <- scan()
# ler tabela
tabela <- read.table(file.choose(),
                     header = TRUE,
                     sep = ";",
                     quote = "\"",
                     dec = ",")
summary(tabela)
# ler csv
dados <- dados <- read.csv(file="/dados/notas.csv",
                           header = TRUE,
                           stringsAsFactors = FALSE)
str(dados)
head(dados)
tail(dados)
summary(dados)
# ler excel
install.packages("gdata")
library(gdata)
read.xls(file="/dados/dados_excel.xlsx",
         sheet = "Plan1", header = TRUE, dec = ",")
# salvando funcao
quadrado <- function(x,y){
  return(x^y)
}
dput(quadrado, file="/dados/quadrado.R")
teste = dget(file="/dados/quadrado.R")
rm(teste)
# dump do estado de execução
dump(ls(), file="/dados/dump.R")
# concatenar dataframer
#merge()

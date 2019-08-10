df <- data.frame(nome = c("Luis","João","Arnaldo","Ana"),
                 sexo = c("M","M","M","F"),
                 idade = c(12,34,35,18))
# visualizar df inteiro
df
# acessando linha
df[3]
# dimensoes
dim(df)
# tamanho
length(df) # numero de colunas
# numero de colunas
ncol(df) 
# numero de linhas
nrow(df)  
# nomes das colunas
colnames(df)
# nomes das linhas
rownames(df)
# adicionando coluna
cbind(df, login = c("luis", "joao", "arnaldo", "ana"))
# adicionando linha
rbind(df, c("Alex", "M", 28, "lex"))
# verificar estrtutura
str(df)
# sumarização e resumos estatisticos
summary(df)
# atribuicao de nome das coluna
colnames(df) <- c("NOME", "SEXO", "IDADE")
df
# acessos avançados ao dataframe
df$NOME #subtitui df[,1]
# escrever df
write.csv2(df,file="/dados/df.csv")
save(df,file="dados/df.rda")
load(file="/dados/df.rda")


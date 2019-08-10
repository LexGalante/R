# setando o padrão para seed, afim de manter a aleatoriedade
set.seed(123)
x <- runif(20, min = 10, max = 20)
x
# -----------
x[3]
# expressão lambda para vetor
x[x > 17]
# expressão lambda complexa para vetor
x[x > 15 & x < 17]
# -----------
x <- 1:10
l <- LETTERS[1:10]
x[l == "D"]
# seletor contains
x[l %in% c("A","E","I")]
# retorna os indices dos elementos os elementos
which(l %in% c("C","B"))
# remover todos na
novos_dados <- na.omit(dados)
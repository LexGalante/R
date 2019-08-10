#dados
assiste_novela <- matrix(c(19, 6, 43, 2), nrow = 2, byrow = T)
#embutindo nome das linhas
rownames(assiste_novela) = c("MASCULINO", "FEMININO")
#embutindo nome das colunas
colnames(assiste_novela) = c("ASSISTE", "NÃO ASSISTE")
#visualizando dados
fix(assiste_novela)
#calculando qui quadrado
chisq.test(assiste_novela)
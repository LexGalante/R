#preparando o pacote
install.packages("igraph", dependencies = TRUE)
library(igraph)

X11()
grafo1 <- igraph(edges=c(1,2,2,3,3,4,4,1))
plot(grafo1)

grafo2 <- graph_from_literal(1-+2, 2-+3, 3++4, 4-+1)
plot(grafo2)
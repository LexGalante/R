# install.packages("Rcpp", dependencies = TRUE)
# install.packages("pkgconfig", dependencies = TRUE)
# install.packages("pdftools", dependencies = TRUE)
# install.packages("tibble", dependencies = TRUE)
# install.packages("tidytext", dependencies = TRUE)
# install.packages("readr", dependencies = TRUE)
# install.packages("tm", dependencies = TRUE)
# install.packages("wordcloud", dependencies = TRUE)
# install.packages("ggplot2", dependencies = TRUE)
# install.packages("tidyr", dependencies = TRUE)
# install.packages("igraph", dependencies = TRUE)
# install.packages("ggraph", dependencies = TRUE)
# install.packages("widyr", dependencies = TRUE)
#libs necessárias
library(pdftools)
library(tibble)
library(dplyr)
library(tidytext)
library(readr)
library(tm)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)
#palavras que devem ser removidas
stopwords_pt <- read_delim("C:/Users/Alex/Desktop/Projetos/R/dados/stopwords.csv",
                           ";",escape_double = FALSE, trim_ws = TRUE)

affin_pt <- read_delim("C:/Users/Alex/Desktop/Projetos/R/dados/affin_pt.csv",
                       ";",escape_double = FALSE, trim_ws = TRUE)
#preparação de lista de setenças
text <- pdf_text("C:/Users/Alex/Desktop/Projetos/R/dados/livro.pdf")
text <- unlist(strsplit(text, "[.]"))
text <- tibble(sentences = text)
text <- drop_na(text)
#preparação dos dados
text$sentences <- text$sentences %>%
  removePunctuation() %>%
  stripWhitespace() %>%
  removeNumbers()
#criando uma coluna com indices
tokens <- text %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, sentences) %>%
  anti_join(stopwords_pt)
#contagem de palavras
tokens_count <- tokens %>%
  count(word, sort = TRUE)
#nuvem de palavras
wordcloud(tokens_count$word, tokens_count$n,
          max.words = 50, scale = c(2,0.5),
          colors = brewer.pal(10, "Spectral"))
#grafico de contagem de palavras
tokens_count %>%
  mutate(word = reorder(word,n)) %>%
  head(9) %>%
  ggplot(aes(word,n,fill=factor(word)))+
  scale_fill_brewer(palette="Set1")+
  geom_col()+
  xlab(NULL)+
  coord_flip()
#extrai as principais sentenças
bigrams <- text %>%
  unnest_tokens(bigram, sentences, token = "ngrams", n = 2) %>%
  separate(bigram,c("word1","word2"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word2 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(bigram,word1,word2,sep = " ") %>%
  count(bigram, sort = TRUE)
#grafico
bigrams_graph <- bigrams %>%
  separate(bigram,c("word1","word2"), sep = " ") %>%
  filter(n>5) %>%
  graph_from_data_frame()
#definindo a base randomica
set.seed(2019)
a <- grid::arrow(type = "closed", 
                 length = unit(.15,"inches"))
ggraph(bigrams_graph, layout = "fr")+
  geom_edge_link(aes(edge_alpha = n), 
                 show.legend = FALSE,
                 arrow = a,
                 end_cap = circle(.07,"inches"))+
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name), vjust = 1, hjust = 1)+
  theme_void()
#criando palavras trigramas, não remover a palavra do meio paranao perder sentido ex: CARTÃO DE CRÉDITO
trigrams <- text %>%
  unnest_tokens(trigram, sentences, token = "ngrams", n = 3) %>%
  separate(trigram,c("word1","word2","word3"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word3 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(trigram,word1,word2,word3,sep = " ") %>%
  count(trigram, sort = TRUE)
#gráfico trigramas
trigrams_graph <- trigrams %>%
  separate(trigram,c("word1","word2","word3"), sep = " ") %>%
  filter(n>3) %>%
  graph_from_data_frame()

ggraph(trigrams_graph, layout = "fr")+
  geom_edge_link(aes(edge_alpha = n), 
                 show.legend = FALSE,
                 arrow = a,
                 end_cap = circle(.07,"inches"))+
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name), vjust = 1, hjust = 1)+
  theme_void()
X11()
#verificando a correlação entre palavras
word_cors <- tokens %>%
  group_by(word) %>%
  filter(n()>3) %>%
  pairwise_cor(word,linenumber,sort = TRUE)
#grafico da correlação
word_cors %>%
  filter(correlation != Inf) %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), 
                 show.legend = FALSE,
                 arrow = a,
                 end_cap = circle(.07,"inches"))+
  geom_node_point(color="#CC00AA",size=5)+
  geom_node_text(aes(label=name), vjust = 1, hjust = 1)+
  theme_void()
#analise de sentimento
affin <- tokens %>%
  inner_join(affin_pt) %>%
  count(index=floor(linenumber / 200), sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment = positivo - negativo) #%>%
  mutate(sentiment = sign(sentiment)*log(abs(sentiment)+1))
X11()
ggplot(affin, aes(index,sentiment))+
  geom_col(show.legend = TRUE)


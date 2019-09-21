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
#install.packages("topicmodels", dependencies = TRUE)
#install.packages("ldatuning", dependencies = TRUE)
#install.packages("purrr", dependencies = TRUE)
library(topicmodels)
library(ldatuning)
library(purrr)
library(tibble)

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
#extração de topicos
dtm <- tokens %>%
  count(linenumber, word, sort = TRUE) %>%
  cast_dtm(linenumber, word, n)
#criando o corpus de palavras
corpus_lda <- LDA(dtm, k = 4, control = list(seed= 1234))
#extraindo os topicos
get_terms(corpus_lda, 15)
#grafico do corpus
corpus_topic <- tidy(corpus_lda, matrix = "beta")
corpus_topic_terms <- corpus_topic %>%
  group_by(topic) %>%
  top_n(4, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  do(head(., n = 15)) %>%
  mutate(term=reorder(term,beta)) %>%
  mutate(order = row_number())
X11()
corpus_topic_terms %>%
  ggplot(aes(order, beta, fill = factor(topic))) +
  geom_bar(stat="identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  xlab("Termos") +
  ylab("Beta") +
  scale_x_continuous(
    breaks = corpus_topic_terms$order,
    labels = corpus_topic_terms$term,
    expand = c(0,0),
    trans = "reverse"
  ) +
  coord_flip()
#analise da quantidade de topicos
n_topics <- c(2,3,5,7,10)
ap_lda_compare <- n_topics %>%
  map(LDA, x = dtm, control = list(seed = 1109))
X11()
tibble(k = n_topics,
       perplex = map_dbl(ap_lda_compare, perplexity)) %>%
  ggplot(aes(k,perplex)) +
  geom_point() +
  geom_line() +
  labs(title = "Evaluating LDA topic models",
       subtitle = "Optimal number of topics(smaller is better)",
       x = "Number of topics",
       y = "Perpexity")









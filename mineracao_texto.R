#instalando o pacote TM
install.packages("tm", dependencies = TRUE)
install.packages("wordcloud", dependencies = TRUE)
#carregando o pacote em memoria
library(tm)
library(wordcloud)
#fontes de dados disponiveis do pacote
getSources()
#formatos de textos disoniveis
getReaders()
#criando o corpus do pacote volatil em memoria
corpus <- VCorpus(DirSource("dados/mineracao_texto"),
                  readerControl = list(reader=readPlain, language="eng"))
inspect(corpus)
#acessando dados de um corpus
meta(corpus[[1]])
inspect(corpus[[2]])
#removendo palavras sem semantica
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#removendo excessos de espaço em branco
corpus <- tm_map(corpus, stripWhitespace)
#removendo pontuacao
corpus <- tm_map(corpus, removePunctuation)
#removendo numeros
corpus <- tm_map(corpus, removeNumbers)
#criando uma nuvem de palavras
X11()
wordcloud(corpus,
          max.words = 100,
          random.order = TRUE,
          colors = rainbow(8),
          rot.per = 0.5,
          use.r.layout = TRUE)


library(quanteda)

data_corpus_inaugural

summary(data_corpus_inaugural)
class(data_corpus_inaugural)

library(tidytext)
library(tibble)
library(dplyr)

us.president.address <- tidy(data_corpus_inaugural) %>% 
  filter(Year > 1990) %>% 
  group_by(President, FirstName) %>% 
  summarise_all(list(~trimws(paste(., collapse = " ")))) %>% 
  arrange(Year) %>% 
  ungroup()

us.president.address

library(tm)
?DataframeSource()

#doc_id 열 추가하기
us.president.address <- us.president.address %>% 
  select(text, everything()) %>% 
  add_column(doc_id = 1:nrow(.), .before = 1)

us.president.address

#데이터프레임 형태를 corpus형태로
address.corpus <- VCorpus(DataframeSource(us.president.address))

address.corpus

lapply(address.corpus[1], content)

## 전처리

address.corpus <- tm_map(address.corpus, content_transformer(tolower))

address.corpus[[1]]$content

sort(stopwords("english"))

Mystopwords <- c(stopwords("english"), c("must","will","can","bless","america"))

address.corpus <- tm_map(address.corpus, removeWords, Mystopwords)

address.corpus <- tm_map(address.corpus, removePunctuation)

address.corpus <- tm_map(address.corpus, removeNumbers)
address.corpus <- tm_map(address.corpus, stripWhitespace)

address.corpus <- tm_map(address.corpus, content_transformer(trimws))

address.corpus[[1]]$content

address.corpus <-  tm_map(address.corpus, content_transformer(gsub),
                          pattern = "america|american|americans|americas",
                          replacement = "america") #동의어 처리

lapply(address.corpus[1], content)

## DTM

address.dtm <- DocumentTermMatrix(address.corpus)

inspect(address.dtm)

termfreq <- colSums(as.matrix(address.dtm))
termfreq

length(termfreq)

termfreq[head(order(termfreq,decreasing = TRUE), 10)]
termfreq[tail(order(termfreq,decreasing = TRUE), 10)]


findFreqTerms(address.dtm, lowfreq = 40)
findFreqTerms(address.dtm, lowfreq = 40, highfreq = 80)

library(ggplot2)

class(termfreq)

termfreq.df <- data.frame(word = names(termfreq), frequency= termfreq)
head(termfreq.df)

ggplot(subset(termfreq.df,frequency >= 40),
       aes(x=word, y=frequency, fill=word)) +
  geom_col(color= "dimgray") +
  labs(x=NULL, y="Term Frequency (count)")


ggplot(subset(termfreq.df,frequency >= 40),
       aes(x=reorder(word,frequency), y=frequency, fill=word)) +
  geom_col(color= "dimgray", width = 0.5, show.legent=FALSE) +
  geom_text(aes(label=frequency), size=3.5, color="black", hjust=0) +
  labs(x=NULL, y="Term Frequency (count)") +
  coord_flip()

inspect(address.dtm)
Docs(address.dtm)

row.names(address.dtm) <- c("Clinton","Bush","Obama", "Trump","Biden")
Docs(address.dtm)


## wordcloud

set.seed(123)

library(wordcloud)

head(termfreq)
wordcloud(words = names(termfreq), freq = termfreq,
          scale = c(3, 0.2), min.freq = 10,
          rot.per = 0.1, random.order = FALSE,
          colors = brewer.pal(6, 'Dark2'))

## 대통령별 빈도 그래프
address.tf <- tidy(address.dtm)

address.tf <- address.tf %>% 
  mutate(document= factor(document, levels=c("Clinton","Bush","Obama", "Trump","Biden"))) %>% 
  arrange(desc(count)) %>% 
  group_by(document) %>% 
  top_n(n=10, wt=count) %>% 
  ungroup()

address.tf

ggplot(address.tf,
       aes(x=term, y=count, fill=document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, ncol=2, scales="free")+
  labs(x=NULL, y="Term Frequency count") +
  coord_flip()

ggplot(address.tf,
       aes(reorder_within(x=term, by=count, within=document), y=count, fill=document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, ncol=2, scales="free")+
  scale_x_reordered() +
  labs(x=NULL, y="Term Frequency count") +
  coord_flip()


### TF-IDF

## DTM

address.dtm2 <- DocumentTermMatrix(address.corpus,
                                   control = list(weighting = weightTfIdf))

inspect(address.dtm2)

row.names(address.dtm2) <- c("Clinton","Bush","Obama", "Trump","Biden")

address.tfidf <- tidy(address.dtm2) %>% 
  mutate(tf_idf=count, count = NULL)

address.tfidf <- address.tfidf %>% 
  mutate(document= factor(document, levels=c("Clinton","Bush","Obama", "Trump","Biden"))) %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(document) %>% 
  top_n(n=10, wt=tf_idf) %>% 
  ungroup()

address.tfidf

ggplot(address.tfidf,
       aes(reorder_within(x=term, by=tf_idf, within=document), y=tf_idf, fill=document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, ncol=2, scales="free")+
  scale_x_reordered() +
  labs(x=NULL, y="tf_idf") +
  coord_flip()

#텍스트 벡터 생성
text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetr$ian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the ecocomy added 160,000 jobs.")

library(tm)

#텍스트 전처리
corpus.docs <- VCorpus(VectorSource(text))
lapply(corpus.docs, meta)
lapply(corpus.docs, content)
corpus.docs <- tm_map(corpus.docs, content_transformer(tolower)) #소문자 변환 
corpus.docs <- tm_map(corpus.docs, removeWords, stopwords('english')) #불용어사전 
myRemove <- content_transformer(function(x,pattern) #특정 패턴을 가지는 문자열
  {return(gsub(pattern, "", X))})
corpus.docs <- tm_map(corpus.docs, myRemove, "(f|ht)tp\\S+\\s*")
corpus.docs <- tm_map(corpus.docs, removePunctuation) #문장부호 삭제
lapply(corpus.docs, content)
corpus.docs[[1]]$content

corpus.docs <- tm_map(corpus.docs, removeNumbers) #숫자 삭제
corpus.docs <- tm_map(corpus.docs, stripWhitespace) #여백 삭제
corpus.docs <- tm_map(corpus.docs, content_transformer(trimws)) #텍스트 앞뒤의 공백 삭제
corpus.docs <- tm_map(corpus.docs, stemDocument) #어간 추출
corpus.docs <- tm_map(corpus.docs, content_transformer(gsub),
                      pattern='economist', replacement='economi') #동의어 처리


corpus.dtm <- DocumentTermMatrix(corpus.docs,
                   control = list(wordLengths = c(2,Inf)))

nTerms(corpus.dtm)
nDocs(corpus.dtm)
Terms(corpus.dtm)

Docs(corpus.dtm)
row.names(corpus.dtm) <- c("BBC","CNN","FoX")

inspect(corpus.dtm)
inspect(corpus.dtm[1:3, 10:19])



### Tidytext 형식의 데이터 셋
text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetr$ian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the ecocomy added 160,000 jobs.")
source <- c("BBC","CNN","FOX")

library(dplyr)
library(tidytext)
library(SnowballC)

text.df <- tibble(source=source, text=text)
text.df

text.df$text <- gsub("(f|ht)tp\\S+||S*", "", text.df$text) #url삭제 
text.df$text <- gsub("\\d+", "", text.df$text) #숫자 삭제 
tidy.docs <- text.df %>% 
  unnest_tokens(output = word, input = text) %>% #토큰화 
  anti_join(stop_words, by="word") %>% #불용어사전을 활용해 불용어 제거 
  mutate(word=wordStem(word)) #어간 추출 
tidy.docs$word <- gsub("\\s+","",tidy.docs$word) #공백제거 
tidy.docs$word <- gsub("economist","economi",tidy.docs$word) #동의어 처리 

tidy.dtm <- tidy.docs %>% count(source, word) %>% 
  cast_dtm(document = source, term = word, value = n)
tidy.dtm

Terms(tidy.dtm)
Docs(tidy.dtm)
inspect(tidy.dtm[1:2,3:5])

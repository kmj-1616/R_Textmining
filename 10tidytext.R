text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetr$ian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the ecocomy added 160,000 jobs.")

#부가정보로 사용할 텍스트의 출처 정보 생성
source <- c("BBC","FOX","CNN")

#tibble 생성
library(dplyr)
text.df <- tibble(source = source, text = text)
text.df
class(text.df)

#tokenization
library(tidytext) #사용할데이터프레임, 토큰화할 결과 저장 열 이름, 데이터프레임에서 토큰화할열
unnest_tokens(tbl = text.df, output = word, input = text)

#파이프연산자 사용하면 데이터프레임 인수 생략할 수 있음
head(iris)
iris %>% head(10)

tidy.docs <- text.df %>% 
  unnest_tokens(output = word, input = text)
tidy.docs

print(tidy.docs, n=10)
print(tidy.docs, n=Inf)

#출처별 문서 수 집계
tidy.docs %>% 
  count(source) %>% #count: 빈도 계산
  arrange(desc(n)) #내림차순으로 정렬 

#불용어 제거
stop_words

tidy.docs$word

tidy.docs <- tidy.docs %>% 
  anti_join(stop_words, by="word")
tidy.docs$word

#현재 tidytext에서 url을 구성하고 있던 요소들 제거
word.removed <- tibble(word=c("http","bbc.in","1g0j4agg"))
anti_join(tidy.docs, word.removed, by="word")

tidy.docs$word

tidy.docs <- tidy.docs %>% 
  anti_join(word.removed, by="word")
tidy.docs$word

#숫자 제거
grep("\\d+", tidy.docs$word)

tidy.docs <- tidy.docs[-grep("\\d+", tidy.docs$word), ]
tidy.docs$word

#어간 추출
library(SnowballC)
tidy.docs <- tidy.docs %>% 
  mutate(word=wordStem(word)) #mutate: 새로운 열 생성 
tidy.docs$word

#url 제거
text.df$text <- gsub("(f|ht)tp\\S+||s*", "", text.df$text)
text.df$text

text.df$text <- gsub("\\d+", "", text.df$text)

tidy.docs <- text.df %>% 
  unnest_tokens(output = word, input = text)
tidy.docs

tidy.docs <- tidy.docs %>% anti_join(stop_words, by="word")

#동의어 처리
tidy.docs$word <- gsub("economist", "economi", tidy.docs$word)
tidy.docs$word

#문자열 벡터인 text를 corpus로 변환
library(tm)
corpus.docs <- VCorpus(VectorSource(text))
corpus.docs

#meta 데이터 할당
meta(corpus.docs, tag="author", type = "local") <- source
tidy(corpus.docs)

#corpus를 tidy-text로 변환
tidy(corpus.docs) %>% #tidy함수를 이용해 corpus 객체를 데이터프레임으로 변환
  unnest_tokens(word, text) %>% #text열로부터 단어들을 추출해서 word라는 열에 저장
  select(source=author, word) #select 함수를 이용해 필요한 열만 따로 출력 


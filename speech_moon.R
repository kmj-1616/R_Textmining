raw_moon=readLines('speech_moon.txt', encoding="UTF-8")
head(raw_moon)
class(raw_moon)

txt <- "치킨은!! 맛있다. xyz 정말 맛있다 !@#"
txt

library(stringr)
txt <- str_replace_all(txt, "[^가-힣]", replacement = " ") #한글이 아니면 공백으로

moon <- raw_moon %>% str_replace_all("[^가-힣]", " ")
head(moon)

moon <- str_trim(moon) #앞뒤 공백 제거
head(moon)

moon <- str_squish(moon) #중간 여러 공백 하나로 통일 

library(dplyr)
moon <- as_tibble(moon) #문자열 벡터를 티블 형태로 
moon

#토큰화하기
#tidytext, unnest_token() 함수 활용

install.packages("tidytext")
library(tidytext)

text <- tibble(value="대한민국은 민주공화국이다.대한민국의 주권은 국민에게 있다.")

text %>% unnest_tokens(input=value, output = word,
                       token = "sentences") #문장으로 쪼갬

text %>% unnest_tokens(input=value, output=word,
                       token = "words") #단어 단위로 쪼갬

text %>% unnest_tokens(input=value, output=word,
                       token = "characters") #문자 하나하나 쪼갬

moon_space <- moon %>% 
  unnest_tokens(input=value, output=word,
                token = "words")
moon_space

# 단어 빈도 분석하기
moon_space <- moon_space %>% count(word, sort = T)
moon_space

str_count("배")
str_count("배배배")

moon_space <- moon_space %>% filter(str_count(word) > 1)
moon_space

top20_moon <- moon_space %>% head(20)
top20_moon


## ggplot2
library(ggplot2)

ggplot(top20_moon, aes(x= reorder(word,n), y=n)) + #단어 빈도순 정렬
  geom_col() +
  coord_flip()

#워드클라우드
install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(moon_space, aes(label=word, size=n))+
  geom_text_wordcloud(seed=1234)+
  scale_radius(limits = c(4,NA), #최소, 최대 단어 빈도 
               range = c(3,30)) #최소, 최대 글자 크기 


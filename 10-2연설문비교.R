library(dplyr)
#문재인 대통령 연설문 불러오기
raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>% 
  as_tibble() %>% 
  mutate(president = "moon")
#박근혜 대통령 연설문 불러오기
raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
park <- raw_park %>% 
  as_tibble() %>% 
  mutate(president="park")

#두 데이터 합치기
bind_speeches <- bind_rows(moon, park) %>% 
  select(president, value)

bind_speeches %>% count(president)

head(bind_speeches)
tail(bind_speeches)

#기본적인 전처리
library(stringr)
speeches <- bind_speeches %>% 
  mutate(value=str_replace_all(value, "[^가-힣]", " "),
         value=str_squish(value))

speeches

#토큰화
library(tidytext)
library(KoNLP)
speeches <- speeches %>% 
  unnest_tokens(input=value,
                output = word,
                token = extractNoun)
speeches

frequency <- speeches %>% 
  count(president, word) %>% #연설문 및 단어별 빈도
  filter(str_count(word) > 1) #두 글자 이상 추출 
head(frequency)
tail(frequency)

# dplyr::slice_max() : 값이 큰 상위 n개의 행을 추출해 내림차순 정렬 
top10 <- frequency %>% 
  group_by(president) %>% #president별로 분리
  arrange(desc(n)) %>% #상위 10개 추출
  head(10) %>% filter(president == "park")
top10

top10 <- frequency %>% 
  group_by(president) %>% #president별로 분리
  slice_max(n, n=10)
top10

top10 <- frequency %>% 
  group_by(president) %>% #president별로 분리
  slice_max(n, n=10, with_ties = F)
top10

#전처리 
word.removed <- tibble(word=c("있습니다","하게","여러분","하겠습니"))
frequency <- anti_join(frequency, word.removed, by="word")






library(ggplot2)
ggplot(top10, aes(x = reorder(word,n),
                  y = n,
                  fill = president)) +
                  geom_col() +
                  coord_flip() +
                  facet_wrap(~ president)

#y축을 통일하지 않고 각각 10개씩
ggplot(top10, aes(x = reorder(word,n),
                  y = n,
                  fill = president)) +
                  geom_col() +
                  coord_flip() +
                  facet_wrap(~ president, scales = "free_y")

#축 재정리
ggplot(top10, aes(x = reorder_within(word,n,president),
                  y = n,
                  fill = president)) +
                  geom_col() +
                  coord_flip() +
                  facet_wrap(~ president, scales = "free_y")

#tidytext::scale_x_reordered() : 각 단어 뒤의 범주 항목 제거

ggplot(top10, aes(x = reorder_within(word,n,president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +
  labs(x=NULL) + #x축 삭제
  theme(text = element_text(family = "nanumgothic")) #폰트 


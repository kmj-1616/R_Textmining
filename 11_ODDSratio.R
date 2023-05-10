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
  labs(x=NULL) 


## 11-1 ODDS Ratio

df_long <- frequency %>% 
  group_by(president) %>% 
  slice_max(n, n=10) %>% 
  filter(word %in% c("국민","우리","정치","행복"))

df_long

# pivoting

install.packages("tidyr")
library(tidyr)

df_wide <- df_long %>% 
  pivot_wider(names_from = president,
              values_from = n)
df_wide

# NA를 0으로
df_wide <- df_long %>% 
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))
df_wide


frequency_wide <- frequency %>% 
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))
frequency_wide

# ODDS Ratio 계산

frequency_wide <- frequency_wide %>% 
  mutate(ratio_moon = ((moon)/(sum(moon))), #moon에서의 단어의 비중
         ratio_park = ((park)/(sum(park)))) #park에서 단어의 비중 
frequency_wide

# 단어 비중 비교를 위해서 각 행에 1을 더함

frequency_wide <- frequency_wide %>% 
  mutate(ratio_moon = ((moon + 1)/(sum(moon + 1))), #moon에서의 단어의 비중
         ratio_park = ((park + 1)/(sum(park + 1)))) #park에서 단어의 비중 
frequency_wide


frequency_wide <- frequency_wide %>% 
  mutate(odds_ratio = ratio_moon/ratio_park)

frequency_wide

#"moon"에서 상대적인 비중 클수록 1보다 큰 값
#"park"에서 상대적인 비중 클수록 1보다 작은 값

frequency_wide %>% 
  arrange(-odds_ratio)

frequency_wide %>% 
  arrange(odds_ratio)

#상대적으로 중요한 단어 추출하기
top10 <- frequency_wide %>% 
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)
top10


top10 <- top10 %>% 
  mutate(president = ifelse(odds_ratio > 1, "moon","park"),
         n = ifelse(odds_ratio > 1, moon, park))

top10

top10 <- top10 %>% 
  group_by(president) %>% 
  slice_max(n, n=10, with_ties = F)
top10


ggplot(top10, aes(x=reorder_within(word,n, president),
                  y=n,
                  fill=president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered()

#그래프별로 축 별도 설정
ggplot(top10, aes(x = reorder_within(word,n,president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free") +
  scale_x_reordered() +
  labs(x=NULL) 

#로그 오즈비
frequency_wide <- frequency_wide %>% 
  mutate(log_odds_ratio = log(odds_ratio))
frequency_wide

frequency_wide %>% 
  arrange(-log_odds_ratio)

frequency_wide %>% 
  arrange(log_odds_ratio)

top10 <- frequency_wide %>%
  group_by(president = ifelse(log_odds_ratio > 0, "moon","park")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = F)
top10

top10 %>% 
  arrange(-log_odds_ratio) %>% 
  select(word, log_odds_ratio, president)

#서로 다른 방향으로 막대 그래프 그리기

ggplot(top10, aes(x=reorder(word, log_odds_ratio),
                  y=log_odds_ratio,
                  fill=president)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL)

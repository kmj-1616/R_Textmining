library(dplyr)
#news1 불러오기
raw_news1 <- readLines("news_1.txt", encoding = "UTF-8")
news1 <- raw_news1 %>% 
  as_tibble() %>% 
  mutate(news = "news1")
#news2 불러오기
raw_news2 <- readLines("news_2.txt", encoding = "UTF-8")
news2 <- raw_news2 %>% 
  as_tibble() %>% 
  mutate(news="news2")

#두 데이터 합치기
bind_news <- bind_rows(news1, news2) %>% 
  select(news, value)

bind_news %>% count(news)

head(bind_news)
tail(bind_news)

#기본적인 전처리
library(stringr)
newss <- bind_news %>% 
  mutate(value=str_replace_all(value, "[^가-힣]", " "),
         value=str_replace_all(value, "\\d+", " "),
         value=str_replace_all(value, "대통령의", "대통령"),
         value=str_replace_all(value, "일본의", "일본"),
         value=str_replace_all(value, "지적했", " "),
         value=str_replace_all(value, "나섰", " "),
         value=str_squish(value))

newss

#토큰화
library(tidytext)
library(KoNLP)
newss <- newss %>% 
  unnest_tokens(input=value,
                output = word,
                token = extractNoun)
newss

frequency <- newss %>% 
  count(news, word) %>% #연설문 및 단어별 빈도
  filter(str_count(word) > 1) #두 글자 이상 추출 
head(frequency)
tail(frequency)

# dplyr::slice_max() : 값이 큰 상위 n개의 행을 추출해 내림차순 정렬 
top10 <- frequency %>% 
  group_by(news) %>% #news별로 분리
  arrange(desc(n)) %>% #상위 10개 추출
  head(10) %>% filter(news == "news1")
top10

top10 <- frequency %>% 
  group_by(news) %>% 
  slice_max(n, n=10, with_ties = F)
top10


## ODDS Ratio

df_long <- frequency %>% 
  group_by(news) %>% 
  slice_max(n, n=10) %>% 
  filter(word %in% c("국민","대통령"))

df_long

# pivoting

install.packages("tidyr")
library(tidyr)

df_wide <- df_long %>% 
  pivot_wider(names_from = news,
              values_from = n)
df_wide

# NA를 0으로
df_wide <- df_long %>% 
  pivot_wider(names_from = news,
              values_from = n,
              values_fill = list(n = 0))
df_wide


frequency_wide <- frequency %>% 
  pivot_wider(names_from = news,
              values_from = n,
              values_fill = list(n = 0))
frequency_wide

# ODDS Ratio 계산

frequency_wide <- frequency_wide %>% 
  mutate(ratio_news1 = ((news1)/(sum(news1))), #news1에서의 단어의 비중
         ratio_news2 = ((news2)/(sum(news2)))) #news2에서 단어의 비중 
frequency_wide

# 단어 비중 비교를 위해서 각 행에 1을 더함

frequency_wide <- frequency_wide %>% 
  mutate(ratio_news1 = ((news1 + 1)/(sum(news1 + 1))), #news1에서의 단어의 비중
         ratio_news2 = ((news2 + 1)/(sum(news2 + 1)))) #news2에서 단어의 비중 
frequency_wide


frequency_wide <- frequency_wide %>% 
  mutate(odds_ratio = ratio_news1/ratio_news2)

frequency_wide

#"news1"에서 상대적인 비중 클수록 1보다 큰 값
#"news2"에서 상대적인 비중 클수록 1보다 작은 값

frequency_wide %>% 
  arrange(-odds_ratio)

frequency_wide %>% 
  arrange(odds_ratio)

#상대적으로 중요한 단어 추출하기
top10 <- frequency_wide %>% 
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)
top10


top10 <- top10 %>% 
  mutate(news = ifelse(odds_ratio > 1, "news1","news2"),
         n = ifelse(odds_ratio > 1, news1, news2))

top10

top10 <- top10 %>% 
  group_by(news) %>% 
  slice_max(n, n=10, with_ties = F)
top10


ggplot(top10, aes(x=reorder_within(word,n, news),
                  y=n,
                  fill=news)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ news, scales = "free_y") +
  scale_x_reordered()

#그래프별로 축 별도 설정
library(ggplot2)

ggplot(top10, aes(x = reorder_within(word,n,news),
                  y = n,
                  fill = news)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ news, scales = "free") +
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
  group_by(news = ifelse(log_odds_ratio > 0, "news1","news2")) %>% 
  slice_max(abs(log_odds_ratio), n=10, with_ties = F)
top10

top10 %>% 
  arrange(-log_odds_ratio) %>% 
  select(word, log_odds_ratio, news)

#서로 다른 방향으로 막대 그래프 그리기

ggplot(top10, aes(x=reorder(word, log_odds_ratio),
                  y=log_odds_ratio,
                  fill=news)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL)

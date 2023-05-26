# 이낙연, 이재명 경기도지사 언급 트위터 데이터 분석





library(dplyr)
library(readr)
bind_tweet <- bind_rows(
  read_csv("tweet_nak.csv") %>% mutate(candidate = "이낙연"),
  read_csv("tweet_jae.csv") %>% mutate(candidate = "이재명"))
glimpse(bind_tweet)










# 기본적인 전처리

install.packages("lubridate")
install.packages("textclean")
library(lubridate)
?lubridate
library(textclean)
library(stringr)
set.seed(1234)

tweet <- bind_tweet %>%
  mutate(text = replace_tag(str_to_lower(text)), # id 태그 제거
         text = str_squish(replace_html(text)), # html 특수 문자 제거
         date = date(created_at)) %>% # 날짜 변수 생성
  filter(!str_detect(text, "https://")) %>% # 광고 트윗 제거
  group_by(candidate) %>% # 중복 글 제거
  distinct(text, .keep_all = T) %>%
  group_by(candidate, date, screen_name) %>% # 사용자별 하루 최대 5개 추출
  slice_sample(n = 5) %>%
  ungroup()
glimpse(tweet)


# 트윗 빈도 추이

frequency_date <- tweet %>%
  count(date, candidate)
frequency_date


## 선 그래프
library(ggplot2)
ggplot(frequency_date, aes(x = date, y = n, col = candidate)) +
  geom_line()


# 이슈 알아보기

#8월 14일의 이슈 알아보기
#유독 두 후보의 언급량이 많은 8월 14일에 무슨 일이 있었는지 알아보기
#분석 절차
#1.관심 날짜와 그 외 날짜의 단어 빈도를 구합니다.
#2.로그 오즈비를 이용해 관심 날짜에 상대적으로 많이 사용된 단어를 추출합니다.
#3.트윗의 내용을 살펴보고 관심 날짜에 무슨 일이 있었는지 알아봅니다.
























library(tidytext)
library(KoNLP)
word_tweet_raw <- tweet %>%
  unnest_tokens(input = text,
                output = word,
                token = "words",
                drop = F)

#14일 트윗과 나머지 트윗으로 변환
frequency14 <- word_tweet_raw %>%
  mutate(category = ifelse(date == "2020-08-14", "target", "etc")) %>%
  filter(str_count(word) >= 2) %>%
  count(category, word, sort = T)
frequency14


#분모에 "etc" , 분자에 "target" 의 단어 빈도를 놓고 로그 오즈비를 구함

library(tidyr)
wide14 <- frequency14 %>%
  pivot_wider(names_from = category,
              values_from = n,
              values_fill = list(n = 0))
# 로그 오즈비 변수 추가
wide14 <- wide14 %>%
  mutate(log_odds_ratio = log(((target + 1) / (sum(target + 1))) /
                                ((etc + 1) / (sum(etc + 1)))))
# log_odds_ratio 높은 순 출력
wide14 %>%
  arrange(-log_odds_ratio) %>%
  head(20)


# 트윗 원문 내용 확인
tweet %>%
  filter(date == "2020-08-14" & str_detect(text, "조사")) %>%
  head(10) %>%
  pull(text)




#8월 18일~19일의 이슈 알아보기
#8월 18일과 19일에 이낙연 의원의 언급량이 크게 상승함

frequency_nak1819 <- word_tweet_raw %>%
  mutate(category = ifelse(date >= "2020-08-18" &
                             date <= "2020-08-19", "target", "etc")) %>%
  filter(candidate == "이낙연" & str_count(word) >= 2) %>%
  count(category, word, sort = T)


# Wide form으로 변환
wide_nak1819 <- frequency_nak1819 %>%
  pivot_wider(names_from = category,
              values_from = n,
              values_fill = list(n = 0))
# 로그 오즈비 변수 추가
wide_nak1819 <- wide_nak1819 %>%
  mutate(log_odds_ratio = log(((target + 1) / (sum(target + 1))) /
                                ((etc + 1) / (sum(etc + 1)))))


# log_odds_ratio 높은 순 출력
wide_nak1819 %>%
  arrange(-log_odds_ratio) %>%
  head(20)


## 감정분석

#1.트윗에 감정 점수를 부여한 다음 감정 범주로 분류합니다.
#2.감정 범주별로 자주 언급한 단어를 추출해 막대 그래프를 만듭니다.

# 감정 사전 불러오기
dic <- read_csv("knu_sentiment_lexicon.csv")
# 감정 점수 부여, 감정 극성 분류
dic %>% count(polarity)

word_tweet <- word_tweet_raw %>%
  left_join(dic, by = "word") %>% # 감정 점수 부여
  mutate(polarity = ifelse(is.na(polarity), 0, polarity), # NA를 0으로 변환
         sentiment = ifelse(polarity == 2, "긍정", # 감정 범주 분류
                            ifelse(polarity == -2, "부정", "중립")))


# 자주 언급한 단어 추출
top10_word <- word_tweet %>%
  # 불용어 제거
  filter(!(candidate == "이낙연" & str_detect(word, "이낙연")) &
           !(candidate == "이재명" & str_detect(word, "이재명"))) %>%
  filter(str_count(word) >= 2) %>%
  count(candidate, sentiment, word) %>%
  group_by(candidate, sentiment) %>%
  slice_max(n, n = 10, with_ties = F)
top10_word


ggplot(top10_word, aes(x = reorder_within(word, n, candidate),
                       y = n,
                       fill = sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(candidate ~ sentiment, # 후보, 감정 범주별 그래프 생성
             scales = "free") +
  scale_x_reordered()









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

# tidy-text 형식으로 진행
us.president.address

## 단어를 기준으로 토큰화
address.words <- us.president.address %>% 
  unnest_tokens(word,text)
address.words

# 전처리
address.words <- address.words %>% 
  anti_join(stop_words, by='word') %>% #불용어 제거
  filter(!grepl(pattern = "\\d+",word)) %>%  #숫자 제거
  mutate(word=gsub(pattern = "'",replacement = "",word)) %>% #공백 제거 
  mutate(word=gsub(pattern = "america|americas|american|americans", #동의어 처리 
                   replacement = "america",word)) %>% 
  count(President, word, sort = TRUE, name = 'count') %>% #단어 빈도수 계산
  ungroup()

address.words  

# 단어 빈도 시각화
library(ggplot2)

address.words %>% 
  group_by(word) %>% 
  summarise(count=sum(count)) %>% 
  arrange(desc(count)) %>% 
  top_n(n=10, wt=count) %>% 
  ggplot(aes(reorder(word,count),count)) +
  geom_col(color='dimgray', fill='salmon', width=0.6, show.legend=FALSE) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  geom_text(aes(label=count), size=3.5, color="black", vjust=1) +
  labs(X=NULL, y='Term Frequency (count)') +
  coord_flip()

## TF-IDF 계산
address.words <- address.words %>% 
  bind_tf_idf(term = word, document = President, n=count)
address.words

# 시각화를 통해 TF-IDF 순위 확인
address.words %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(President = factor(President,
                            levels = c('Bush','Obama','Trump','Biden','Clinton'))) %>% 
  group_by(President) %>% 
  top_n(7, wt=tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(word, tf_idf, President), tf_idf, fill=President)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~President, ncol = 2, scales = "free") +
  scale_x_reordered() +
  labs(X=NULL, y='Term Frequency-Inverse Document Frequency')+
  coord_flip()

# TF 상위 7개 단어 확인
address.words %>% 
  arrange(desc(tf)) %>% 
  mutate(President = factor(President,
                            levels = c('Bush','Obama','Trump','Biden','Clinton'))) %>% 
  group_by(President) %>% 
  top_n(7, wt=tf) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(word, tf, President), tf, fill=President)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~President, ncol = 2, scales = "free") +
  scale_x_reordered() +
  labs(X=NULL, y='Term Frequency(proportion)')+
  coord_flip()
  
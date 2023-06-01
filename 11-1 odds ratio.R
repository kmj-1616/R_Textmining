
library(dplyr)
# πÆ¿Á¿Œ ¥Î≈Î∑… ø¨º≥πÆ ∫“∑Øø¿±‚
raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>%
  as_tibble() %>%
  mutate(president = "moon")
# π⁄±Ÿ«˝ ¥Î≈Î∑… ø¨º≥πÆ ∫“∑Øø¿±‚
raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
park <- raw_park %>%
  as_tibble() %>%
  mutate(president = "park")

# µŒ µ•¿Ã≈Õ «’ƒ°±‚±‚
bind_speeches <- bind_rows(moon, park) %>%
  select(president, value)


bind_speeches %>% count(president)

head(bind_speeches)
tail(bind_speeches)

# ±‚∫ª¿˚¿Œ ¿¸√≥∏Æ
library(stringr)
speeches <- bind_speeches %>%
  mutate(value = str_replace_all(value, "[^∞°-∆R]", " "),
         value = str_squish(value))
speeches


# ≈‰≈´»≠
library(tidytext)
library(KoNLP)
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches

frequency <- speeches %>%
  count(president, word) %>% # ø¨º≥πÆ π◊ ¥‹æÓ∫∞ ∫Ûµµ
  filter(str_count(word) > 1) # µŒ ±€¿⁄ ¿ÃªÛ √ﬂ√‚
head(frequency)
tail(frequency)

# dplyr::slice_max() : ∞™¿Ã ≈´ ªÛ¿ß n∞≥¿« «‡¿ª √ﬂ√‚«ÿ ≥ª∏≤¬˜º¯ ¡§∑ƒ
top10 <- frequency %>%
  group_by(president) %>% # president∫∞∑Œ ∫–∏Æ
  arrange(desc(n)) %>% # ªÛ¿ß 10∞≥ √ﬂ√‚
  head(10) 
top10

top10 <- frequency %>%
  group_by(president) %>% # president∫∞∑Œ ∫–∏Æ
  slice_max(n, n= 10)
top10

top10 <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)
top10

library(ggplot2)
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president)

# y√‡¿ª ≈Î¿œ«œ¡ˆ æ ∞Ì ∞¢∞¢ 10∞≥æø

ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")

# √‡ ¿Á¡§∏Æ
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")

#tidytext::scale_x_reordered() : ∞¢ ¥‹æÓ µ⁄¿« π¸¡÷ «◊∏Ò ¡¶∞≈

ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL) + # x√‡ ªË¡¶
  theme(text = element_text(family = "nanumgothic")) # ∆˘∆Æ

# 11-1 ODDS Ratio


df_long <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10) %>%
  filter(word %in% c("±ππŒ", "øÏ∏Æ", "¡§ƒ°", "«‡∫π"))

df_long

# pivoting

install.packages("tidyr")
library(tidyr)
df_wide <- df_long %>%
  pivot_wider(names_from = president,
              values_from = n)
df_wide


df_long

#  NA∏¶ 0¿∏∑Œ 
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

#Odds ratio ∞ËªÍ
frequency_wide <- frequency_wide %>%
  mutate(ratio_moon = ((moon)/(sum(moon))), # moon ø°º≠ ¥‹æÓ¿« ∫Ò¡ﬂ
         ratio_park = ((park)/(sum(park)))) # park ø°º≠ ¥‹æÓ¿« ∫Ò¡ﬂ
frequency_wide

# ¥‹æÓ ∫Ò¡ﬂ ∫Ò±≥∏¶ ¿ß«ÿº≠ ∞¢ «‡ø° 1¿ª ¥ı«‘

frequency_wide <- frequency_wide %>%
  mutate(ratio_moon = ((moon + 1)/(sum(moon + 1))), # moonø°º≠ ¥‹æÓ¿« ∫Ò¡ﬂ
         ratio_park = ((park + 1)/(sum(park + 1)))) # parkø°º≠ ¥‹æÓ¿« ∫Ò¡ﬂ
frequency_wide


frequency_wide <- frequency_wide %>%
  mutate(odds_ratio = ratio_moon/ratio_park)
frequency_wide

#"moon"ø°º≠ ªÛ¥Î¿˚¿Œ ∫Ò¡ﬂ ≈¨ºˆ∑œ 1∫∏¥Ÿ ≈´ ∞™
#"park"ø°º≠ ªÛ¥Î¿˚¿Œ ∫Ò¡ﬂ ≈¨ºˆ∑œ 1∫∏¥Ÿ ¿€¿∫ ∞™

frequency_wide %>%
  arrange(-odds_ratio)


frequency_wide %>%
  arrange(odds_ratio)


# ªÛ¥Î¿˚¿∏∑Œ ¡ﬂø‰«— ¥‹æÓ √ﬂ√‚«œ±‚
top10 <- frequency_wide %>%
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)
top10



top10 <- top10 %>%
  mutate(president = ifelse(odds_ratio > 1, "moon", "park"),
         n = ifelse(odds_ratio > 1, moon, park))
top10

top10 <- top10 %>%
  group_by(president) %>% 
  slice_max(n, n= 10, with_ties = F)
top10


ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered()


# ±◊∑°«¡ ∫∞∑Œ √‡ ∫∞µµ º≥¡§
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL) + # x√‡ ªË¡¶
  theme(text = element_text(family = "nanumgothic")) # ∆˘∆Æ

# ∑Œ±◊ ø¿¡Ó∫Ò
frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(odds_ratio))
frequency_wide


frequency_wide %>%
  arrange(-log_odds_ratio)


frequency_wide %>%
  arrange(log_odds_ratio)


top10 <- frequency_wide %>%
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10


top10 %>%
  arrange(-log_odds_ratio) %>%
  select(word, log_odds_ratio, president)

# º≠∑Œ ¥Ÿ∏• πÊ«‚¿∏∑Œ ∏∑¥Î ±◊∑°«¡ ±◊∏Æ±‚
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

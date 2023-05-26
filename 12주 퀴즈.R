library(readr)
raw_speeches <- read_csv("speeches_presidents.csv")


# ÀüÃ³¸®
library(dplyr)
library(stringr)
speeches <- raw_speeches %>%
  filter(president %in% c("ÀÌ¸í¹Ú", "³ë¹«Çö")) %>%
  mutate(value = str_replace_all(value, "[^°¡-ÆR]", " "),
         value = str_squish(value))
speeches


# ¸í»ç ÃßÃâ
library(tidytext)
library(KoNLP)
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches


# ¿¬¼³¹®º° ´Ü¾î ºóµµ ±¸ÇÏ±â
frequency <- speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)
frequency


# ·Î±× ¿ÀÁîºñ¸¦ ±¸ÇÏ±â À§ÇÑ pivoting: long formÀ» wide formÀ¸·Î º¯È¯
library(tidyr)
frequency_wide <- frequency %>%
  pivot_wider(names_from = president, # º¯¼ö¸íÀ¸·Î ¸¸µé °ª
              values_from = n, # º¯¼ö¿¡ Ã¤¿ö ³ÖÀ» °ª
              values_fill = list(n = 0)) # °áÃøÄ¡ 0À¸·Î º¯È¯
frequency_wide


# ·Î±× ¿ÀÁîºñ ±¸ÇÏ±â
frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(((ÀÌ¸í¹Ú + 1) / (sum(ÀÌ¸í¹Ú + 1))) /
                                ((³ë¹«Çö + 1) / (sum(³ë¹«Çö + 1)))))
frequency_wide

# »ó´ëÀûÀ¸·Î Áß¿äÇÑ ´Ü¾î ÃßÃâ

frequency_wide$president <- ifelse(frequency_wide$log_odds_ratio > 0, "lee", "roh")

top10 <- frequency_wide %>%
  group_by(president) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10

# ¸·´ë ±×·¡ÇÁ ±×¸®±â
library(ggplot2)
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = president)) +
                  geom_col() +
                  coord_flip () +
                  labs(x = NULL, y= NULL)


#### TF-IDF

# µ¥ÀÌÅÍ ºÒ·¯¿À±â
library(readr)
raw_speeches <- read_csv("speeches_presidents.csv")
# ÀüÃ³¸®
library(dplyr)
library(stringr)
speeches <- raw_speeches %>%
  mutate(value = str_replace_all(value, "[^°¡-ÆR]", " "),
         value = str_replace_all(value, "¹Ú±ÙÇý", " "),
         value = str_replace_all(value, "°³°³ÀÎ", "°³ÀÎ"),
         value = str_squish(value))
speeches


# ¸í»ç ±âÁØ ÅäÅ«È­
library(tidytext)
library(KoNLP)
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches

# ´Ü¾î ºóµµ ±¸ÇÏ±â
frequecy <- speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)
frequecy

# TF-IDF ±¸ÇÏ±â
frequecy <- frequecy %>%
  bind_tf_idf(term = word, # ´Ü¾î
              document = president, # ÅØ½ºÆ® ±¸ºÐ º¯¼ö
              n = n) %>% # ´Ü¾î ºóµµ
  arrange(-tf_idf)
frequecy

# »ó´ëÀûÀ¸·Î Áß¿äÇÑ ´Ü¾î ÃßÃâ
top10 <- frequecy %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)
head(top10)

# ¸·´ë±×·¡ÇÁ
library(ggplot2)
ggplot(top10, aes(x = reorder_within(word, tf_idf, president),
                  y = tf_idf,
                  fill = president)) +
  geom_col(show.legend = F) +
  coord_flip () +
  facet_wrap(~ president, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL)


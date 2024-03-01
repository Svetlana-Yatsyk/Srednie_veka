library(mallet)
library(dplyr)
library(tidyr)

topic_words <- mallet.topic.words(topic.model, normalized = T)

# Создаем словарь из списка уникальных лемм
vocab <- unique(lemm_clean$lemma)

# Преобразуем матрицу topic_words_matrix в датафрейм
tidy_beta <- as.data.frame(topic_words) %>%
  tibble::rownames_to_column(var = "term_id") %>%
  pivot_longer(cols = -term_id, names_to = "topic", values_to = "beta") %>%
  mutate(
    # Сдвигаем term_id на 1, потому что индексация в R начинается с 1
    term = vocab[as.numeric(term_id) + 1], 
    # Преобразуем имена столбцов в номера топиков
    topic = as.numeric(gsub("V", "", topic))
  ) %>%
  select(-term_id, topic, term, beta)

# Проверяем, есть ли NA в колонке topic
sum(is.na(tidy_beta$topic))

# сравниваем топики----
# Фильтруем и преобразуем данные для сравнения топиков
beta_spread <- tidy_beta %>%
  filter(topic %in% c(7, 40)) %>%
  mutate(topic = paste0("topic_", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic_24 > .001 | topic_18 > .001) %>%
  mutate(log_ratio = log2(topic_7 / topic_40))

# Подготавливаем данные для визуализации
beta_log_ratio <- beta_spread %>%
  mutate(sign = case_when(log_ratio > 0 ~ "positive",
                          log_ratio < 0 ~ "negative")) %>%
  select(term, log_ratio, sign) %>% 
  arrange(desc(abs(log_ratio))) %>% 
  slice_head(n = 10)

# Визуализация
ggplot(beta_log_ratio, aes(reorder(term, log_ratio), log_ratio, fill = sign)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Термин", y = "log2 (beta_24 / beta_18)") +
  coord_flip() +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red"))


library(dplyr)
library(purrr)

load("lemm_clean.Rdata")

# сохраняю из датафрейма леммы обратно в доки
grouped <- lemm_clean %>%
  group_by(doc_id) %>%
  summarise(lemmas = paste(lemma, collapse = " ")) %>%
  ungroup()

# Сохранение каждой группы в отдельный текстовый файл
walk2(grouped$doc_id, grouped$lemmas, ~writeLines(.y, paste0(.x, ".txt")))

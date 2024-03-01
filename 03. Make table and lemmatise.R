library(tidyr)
library(tidytext)
library(dplyr)
library(stringr)
library(purrr)
library(udpipe)
library(stopwords)
library(reshape2)
library(wordcloud)
library(showtext)

files <- list.files("cleaned_txts")
file_paths <- paste0("cleaned_txts/", files) 
#мак видит все 1877 файлов, а виндоус только 1473 

all_texts <- map(file_paths, readLines)
names(all_texts) <- files

# преобразуем список в таблицу, выносим год, выпуск, имя автора, название в отдельный столбец----
texts_tbl <- all_texts %>% 
  stack() %>% 
  rename(text = values) %>% 
  transmute(file_name = str_remove(ind, ".txt"), 
            text = text) %>%
  separate(file_name, into = c("Year", "Issue", "Author", "Title"), sep = "_", extra = "merge") %>%
  mutate(
    Issue = str_extract(Issue, "^[0-9]+(?:\\([0-9]+(-[0-9]+)?\\))?"), # Учитываем наличие или отсутствие номера в скобках
    Title = str_replace_all(Title, "_", " ") # Заменяем подчеркивания на пробелы в названии
  ) %>%
  mutate(article_id = row_number()) %>% # Добавляем столбец id
  select(article_id, Year, Issue, Author, Title, text) # Ставим столбец id на первое место

# функция для определения гендера автора
determine_gender <- function(author_surname) {
  if (author_surname %in% c("Редколлегия", "Коллектив авторов") | str_detect(author_surname, ",")) {
    return("—")
    # Проверка окончаний для женщин
  } else if (any(str_ends(author_surname, c("ина", "ова", "ая", "ёва", "ева")))) {
    return("ж")
    # Проверка окончаний для мужчин
  } else if (any(str_ends(author_surname, c("ин", "ов", "ий", "ев", "ёв")))) {
    return("м")
    # Во всех остальных случаях не ставим ничего
  } else {
    return("")
  }
}

# добавляем гендер в таблицу
texts_tbl$Gender <- sapply(texts_tbl$Author, determine_gender)

# то, что автоматически не определилось, правим руками и мерджим
add_gender <- read.csv("~/Documents/RESEARCH/rus_hist/SV_topic_modelling_local/gendered.csv")

# объединение датафреймов
texts_gendered <- texts_tbl %>%
  left_join(add_gender, by = c("article_id" = "id"))

texts_gendered <- texts_gendered %>%
  select(-Author) %>% # удаление старого столбца Author
  rename(Author = Author_new) # его замена новым (да, это тяжеловесно)

# texts_gendered <- texts_gendered %>%
#   select(-Gender.x, -Issue.x)  %>%
#   rename(Gender = Gender.y, Issue = Issue.y)

metadata_only <- texts_gendered %>%
  select(-text)

write.csv(metadata_only, "metadata.csv")

#texts_tbl <- read.csv("texts_tbl_full.csv")
#texts_tbl <- texts_gendered

# лемматизируем----
#russian_gsd <- udpipe_load_model(file = "~/Documents/RESEARCH/rus_hist/SV_topic_modelling/russian-gsd-ud-2.5-191206.udpipe")
russian_syntagrus <- udpipe_load_model(file = "~/Documents/RESEARCH/rus_hist/SV_topic_modelling_local/udpipe_models/russian-syntagrus-ud-2.5-191206.udpipe")

syntagrus_lemmatised <- udpipe_annotate(russian_syntagrus, 
                                texts_tbl$text, 
                                doc_id = texts_tbl$article_id) %>% 
  as_tibble() %>% 
  mutate(lemma = tolower(lemma))

#save(syntagrus_lemmatised, file = "data/lemm_dirty.Rdata")
write.csv(syntagrus_lemmatised, "syntagrus_lemmatised.csv")
# 6 560 000 слов до чистки

# причесываем -------
# удалить стоп-слова и мусор
sw_ru <- stopwords("ru")

lemm_clean <- syntagrus_lemmatised %>% 
  filter(nchar(lemma) > 2) %>% 
  filter(!lemma %in% sw_ru) %>% 
  select(doc_id, lemma, upos) %>% 
  mutate(lemma = case_when(str_detect(lemma, "божия") ~ "божий",
                           str_detect(lemma, "отц") ~ "отец",
                           str_detect(lemma, "уколово|уколовый") ~ "уколова",
                           str_detect(lemma, "бойцова") ~ "бойцов",
                           str_detect(lemma, "увар|уварова") ~ "уваров",
                           str_detect(lemma, "сванидз|сванидзй") ~ "сванидзе",
                           str_detect(lemma, "тогоев|тогоевой") ~ "тогоева", 
                           str_detect(lemma, "ястребицкий") ~ "ястребицая", 
                           str_detect(lemma, "симоние") ~ "симония",
                           str_detect(lemma, "неусыхину|неусыхина|неусыхиный") ~ "неусыхин",
                           str_detect(lemma, "христианска|христиэ|христь") ~ "христианский",
                           str_detect(lemma, "христофора") ~ "христофор",
                           str_detect(lemma, "христ") ~ "христос",
                           str_detect(lemma, "абрамсона") ~ "абрамсон",
                           str_detect(lemma, "аббатис|аббатисс") ~ "аббатисса",
                           str_detect(lemma, "абзаец") ~ "абзац",
                           str_detect(lemma, "убликация") ~ "публикация",
                           #str_detect(lemma, "убликация") ~ "публикация",
                           #str_detect(lemma, "убликация") ~ "публикация",
                           #str_detect(lemma, "убликация") ~ "публикация",
                           #str_detect(lemma, "убликация") ~ "публикация",
                           #str_detect(lemma, "тсрмин") ~ "термин",
                           #многое еще можно исправить
                           .default = lemma)) %>% 
  filter(!str_detect(lemma, "[[:punct:]]")) %>% 
  filter(upos %in% c("ADJ", "NOUN", "PROPN"))

save(lemm_clean, file = "data/lemm_clean.Rdata")

#load("~/Sveta/SV_from_Github/data/lemm_clean.Rdata")

# считаем частотность
text_count <- lemm_clean %>% 
  group_by(doc_id, lemma) %>% 
  count(lemma)

#удаляем слова, которые встречаются 1 раз
total <- text_count %>% 
  group_by(lemma) %>% 
  summarise(total = sum(n))

text_count_pruned <- text_count %>% 
  left_join(total) %>% 
  filter(total > 1) %>% 
  select(-total)

# преобразует датафрейм в разреженную матрицу----
# Каждая строка соответствует документу (doc_id), 
# каждый столбец - лемме (lemma), 
# а значения в ячейках - количеству вхождений леммы в документ (n).
text_dtm <- text_count_pruned %>% 
  cast_sparse(doc_id, lemma, n) 

save(text_dtm, file = "data/Sparse.Rdata")

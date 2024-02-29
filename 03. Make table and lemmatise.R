library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(udpipe)
library(stopwords)
library(reshape2)
library(wordcloud)
library(showtext)

files <- list.files("cleaned_txts")
file_paths <- paste0("cleaned_txts/", files) #мак видит все 1877 файлов, а виндоус только 1473 

all_texts <- map(file_paths, readLines)
names(all_texts) <- files

# преобразуем список в таблицу, выносим год, выпуск, имя автора, название в отдельный столбец
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

# Добавляем гендер в таблицу
texts_tbl$Gender <- sapply(texts_tbl$Author, determine_gender)

# То, что автоматически не определилось, правим руками и мерджим
# Загрузка CSV-файла
add_gender <- read.csv("gendered.csv")

# Объединение датафреймов
texts_gendered <- texts_tbl %>%
  left_join(select(add_gender, -Issue), by = c("article_id" = "id"))

texts_gendered <- texts_gendered %>%
  select(-Author) %>% # удаление старого столбца Author
  rename(Author = Author_new) # его замена новым (да, это тяжеловесно)

metadata_only <- texts_gendered %>%
  select(-text)

write.csv(metadata_only, "metadata.csv")

# 'everything()' в 'select' включает все оставшиеся столбцы в исходном порядке


# лемматизируем
russian_gsd <- udpipe_load_model(file = "~/Documents/RESEARCH/rus_hist/SV_topic_modelling/russian-gsd-ud-2.5-191206.udpipe")
russian_syntagrus <- udpipe_load_model(file = "~/Documents/RESEARCH/rus_hist/SV_topic_modelling/udpipe_models/russian-syntagrus-ud-2.5-191206.udpipe")

syntagrus_lemmatised <- udpipe_annotate(russian_syntagrus, 
                                texts_tbl$text, 
                                doc_id = texts_tbl$id) %>% 
  as_tibble() %>% 
  mutate(lemma = tolower(lemma))

write_csv(gsd_lemmatised, "gsd_lemmatised.csv")

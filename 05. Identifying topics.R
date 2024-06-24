library(mallet)
library(stringr)
library(tidyverse)
library(tidytext)
library(udpipe)
library(ggwordcloud)
library(ggplot2)
library(dplyr)

num_topics <- 60 # Количество тем
num_iterations <- 2000 # Количество итераций

# Подготовка корпуса
my_texts <- character()

# Список файлов в папке corpus в вашей рабочей директории
file_list <- list.files("corpus_lemmatised/", full.names = TRUE)

stop_words <- "./stopwords_ru.txt"

for(i in 1:length(file_list)){
  
  # Чтение текста
  loaded_file <- readLines(file_list[i], warn = FALSE)
  loaded_file <- paste(loaded_file, collapse = "\n")
  
  # Присваиваем имя тексту (используем имена файлов, удаляя "corpus/" и ".txt")
  text_name <- gsub(pattern = "corpus/|.txt", replacement = "", x = file_list[i])
  
  # Преобразовываем текст в нижний регистр и добавляем в my_texts
  my_texts <- c(my_texts, tolower(loaded_file))
  names(my_texts)[length(my_texts)] <- text_name
  
  print(i)  
}

clean_text <- function(texts) {
  cleaned_texts <- sapply(texts, function(text) {
    text <- gsub("\\b\\w{1,3}\\b", "", text) # удалим слова короче 3 букв включительно
    text <- gsub("\\s+", " ", text) # Replace multiple spaces with a single space
    return(text)
  })
  return(cleaned_texts)
}

# Apply the text cleaning function
my_texts <- clean_text(my_texts)


# Подготовка текстов к моделированию тем: Импорт текстов в формат Mallet
text.instances <- mallet.import(text.array = my_texts, 
                                stoplist = stop_words, # Удаление стоп-слов
                                id.array = names(my_texts))

# Определение переменных
topic.model60 <- MalletLDA(num.topics=num_topics, alpha.sum = 1, beta = 0.1)

# Загрузка документов для моделирования
topic.model60$loadDocuments(text.instances)

# Подготовка параметров модели
topic.model60$setAlphaOptimization(20, 50)

# Обучение модели
topic.model60$train(num_iterations)

# Расчет распределения тем по документам
doc.topics <- mallet.doc.topics(topic.model60, smoothed=TRUE, normalized=TRUE)

# Расчет распределения слов по темам
topic.words <- mallet.topic.words(topic.model60, smoothed=TRUE, normalized=TRUE)

# Инициализация dataframe для хранения топ-слов по темам
top_words <- data.frame()
firstwords <- character()

# Цикл для визуализации топ-слов по темам
for(i in 1:num_topics){
  words.per.topic <- mallet.top.words(topic.model60, word.weights = topic.words[i,], num.top.words = 20)
  words.per.topic$topic <- i
  top_words <- rbind(top_words, words.per.topic)
  firstwords[i] <- paste(words.per.topic$term[1:5], collapse = " ")
}

# Визуализация таблицы топ-слов по темам
View(top_words)
write.csv(top_words, "top_words_60_topics.csv")
# visualize the first five words per topic
names(firstwords) <- paste("Topic", 1:length(firstwords))
firstwords

### Wordcloud visualization------

# prepare the plot
p1 <- ggplot(
  top_words,
  aes(label = term, size = weight)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~topic)

# show it
p1

# save it
ggsave(p1, filename = "60_Topics_wordcloud_2000iterations.png", scale = 4, bg = "white")

### Alternative visualization (barcharts)

p2 <- top_words %>%
  mutate(term = reorder_within(term, weight, topic)) %>%
  ggplot(aes(weight, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# show it
p2

# save it
ggsave(p2, filename = "60Topics_barchart.png", scale = 2)

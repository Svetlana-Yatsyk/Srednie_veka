library(mallet)
library(stringr)
library(tidyverse)
library(tidytext)
library(udpipe)
library(ggwordcloud)
library(ggplot2)
library(dplyr)

num_topics <- 40 # Количество тем
num_iterations <- 2000 # Количество итераций

# Подготовка корпуса
my_texts <- character()

# Список файлов в папке corpus в вашей рабочей директории
file_list <- list.files("corpus_lemmatised/", full.names = TRUE)

stop_words <- "путь_к_файлу/stopwords_ru.txt" # Укажите правильный путь к файлу со стоп-словами

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

# Подготовка текстов к моделированию тем: Импорт текстов в формат Mallet
text.instances <- mallet.import(text.array = my_texts, 
                                stoplist = stop_words, # Удаление стоп-слов
                                id.array = names(my_texts))

# Определение переменных для модели тем
topic.model <- MalletLDA(num.topics=num_topics, alpha.sum = 1, beta = 0.1)

# Загрузка документов для моделирования тем
topic.model$loadDocuments(text.instances)

# Подготовка параметров модели
topic.model$setAlphaOptimization(20, 50)

# Обучение модели
topic.model$train(num_iterations)

# Расчет распределения тем по документам
doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)

# Расчет распределения слов по темам
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)

# Инициализация dataframe для хранения топ-слов по темам
top_words <- data.frame()
firstwords <- character()

# Цикл для визуализации топ-слов по темам
for(i in 1:num_topics){
  words.per.topic <- mallet.top.words(topic.model, word.weights = topic.words[i,], num.top.words = 20)
  words.per.topic$topic <- i
  top_words <- rbind(top_words, words.per.topic)
  firstwords[i] <- paste(words.per.topic$term[1:5], collapse = " ")
}

# Визуализация таблицы топ-слов по темам
View(top_words)
write.csv(top_words, "top_words_40_topics.csv")
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
ggsave(p1, filename = "40_Topics_wordcloud_2000iterations.png", scale = 4, bg = "white")

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
ggsave(p2, filename = "Topics_barchart.png", scale = 2)

### Cluster the documents per topic

# first assign names that correspond to:
# the first five words of the topics
colnames(doc.topics) <- firstwords
# the titles of the documents
rownames(doc.topics) <- names(my_texts) # to make them look better, remove "corpus" from the names

# visualize an heatmap and save it to a file
png(filename = "heatmap_40_topics_2000_iterarions.png", width = 4000, height = 4000)
heatmap(doc.topics, margins = c(50,50), cexRow = 2, cexCol = 2)
dev.off()

# simplify the visualization 

# start by changing variable type
doc.topics <- as.data.frame(doc.topics)

# create a variable that contains the groups (i.e. the books)
groups_tmp <- rownames(doc.topics)
groups_tmp <- strsplit(groups_tmp, "_")
groups_tmp <- sapply(groups_tmp, function(x) paste(x[1:3], collapse = "_"))

# add it to the dataframe
doc.topics$group <- groups_tmp

# calculate mean for each topic probability per group
doc.topics.simple <- doc.topics %>% 
  group_by(group) %>%
  summarise(across(everything(), mean))

# re-convert the format to matrix
groups_tmp <- doc.topics.simple$group
doc.topics.simple$group <- NULL
doc.topics.simple <- as.matrix(doc.topics.simple)
rownames(doc.topics.simple) <- groups_tmp

# visualize another heatmap and save it to a file
png(filename = "heatmap_simple_40_topics_2000_iterarions.png", width = 1500, height = 1500)
heatmap(doc.topics.simple, margins = c(50,50), cexRow = 2, cexCol = 2)
dev.off()
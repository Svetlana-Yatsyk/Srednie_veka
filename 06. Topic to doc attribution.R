library(mallet)

# Получаем веса топиков для каждого документа
doc_topics <- mallet.doc.topics(topic.model, normalized = T)

# Преобразуем веса топиков в таблицу данных (data frame)
doc_topics_df <- as.data.frame(doc_topics)

# Добавляем идентификаторы документов в таблицу данных, если они у вас есть
doc_ids <- 1:nrow(doc_topics_df) # Или любой другой вектор идентификаторов
doc_topics_df$doc_id <- doc_ids

# Находим доминирующий топик для каждого документа
doc_topics_df$dominant_topic <- apply(doc_topics_df[, -ncol(doc_topics_df)], 1, which.max)

# Просмотр таблицы с весами топиков и доминирующим топиком для каждого документа
head(doc_topics_df)

# Сохранение результатов в CSV файл, если нужно
#write.csv(doc_topics_df, "document_topics.csv", row.names = FALSE)

# вытаскиваем инфо про три главных топика для каждой статьи -----

# Функция для нахождения индексов трех наиболее вероятных топиков
top_three_topics <- function(weights) {
  # Сортируем веса и берем индексы трех наиболее тяжелых
  indices <- order(weights, decreasing = TRUE)[1:3]
  return(indices)
}

# Применяем функцию ко всем документам
top_topics <- apply(doc_topics, 1, top_three_topics)

# Создаем таблицу данных с doc_id и тремя столбцами для топиков
top_topics_df <- data.frame(doc_id = doc_ids,
                            topic_1 = top_topics[1, ],
                            topic_2 = top_topics[2, ],
                            topic_3 = top_topics[3, ])

# Просмотр таблицы с doc_id и тремя наиболее вероятными топиками
head(top_topics_df)

# Сохраняем таблицу в CSV файл
write.csv(top_topics_df, "top_three_topics_per_doc.csv", row.names = FALSE)

# Слияние top_topics_df с metadata
merged_df <- merge(top_topics_df, metadata, by.x = "doc_id", by.y = "article_id", all.x = TRUE)

articles_and_topics <- merged_df %>%
  select(-X)

# Просмотр результата слияния
head(articles_and_topics)

# Сохраняем результат в CSV файл, если это необходимо
write.csv(articles_and_topics, "articles_and_topics.csv", row.names = FALSE)

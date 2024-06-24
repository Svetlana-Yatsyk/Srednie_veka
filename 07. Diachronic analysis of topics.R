library(dplyr)
library(ggplot2)

# Агрегируем данные по годам и топикам
topic_trends <- articles_and_topics %>%
  count(Year, topic_1) %>%
  group_by(topic_1) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

# Строим Ч/Б график популярности каждого топика во времени с отдельными графиками для каждого топика
# ggplot(topic_trends, aes(x = Year, y = proportion, group = topic_1)) +
#   geom_line() +
#   facet_wrap(~ topic_1, scales = "free_y") +
#   labs(x = "Year", y = "Proportion of Documents") +
#   theme_minimal() +
#   theme(legend.position = "none")

topic_trends_2 <- articles_and_topics %>%
  count(Year, topic_1) %>%
  mutate(frequency = n / sum(n))

# Строим график популярности каждого топика во времени
ggplot(topic_trends_2, aes(x = Year, y = frequency, group = topic_1)) +
  geom_point(aes(color = as.factor(topic_1))) + # Точки для отображения данных
  geom_line(aes(color = as.factor(topic_1)), alpha = 0.3) + # Линии для тенденций
  facet_wrap(~ topic_1, scales = "free_y") + # Создаем отдельные графики для каждого топика
  labs(x = "Year", y = "Frequency", color = "Topic") +
  theme_minimal() +
  theme(legend.position = "none")


# # пытаюсь сгладить разницу между топиками----
# # Считаем общее количество документов для каждой темы
# topic_counts <- articles_and_topics %>% 
#   group_by(topic_1) %>% 
#   summarise(total_docs = n())
# 
# # Объединяем исходные данные с общим количеством документов по темам
# articles_and_topics <- articles_and_topics %>% 
#   left_join(topic_counts, by = "topic_1")
# 
# # Рассчитываем взвешенную частоту
# articles_and_topics <- articles_and_topics %>%
#   mutate(topic_1 = 1 / total_docs)


# главный автор каждого топика---
# Считаем количество документов для каждой комбинации топика и автора
author_counts <- articles_and_topics %>%
  group_by(topic_1, Author) %>%
  summarise(count = n(), .groups = 'drop')

# Находим самого активного автора для каждого топика
most_active_authors <- author_counts %>%
  arrange(desc(count)) %>%
  group_by(topic_1) %>%
  slice(1) %>%
  ungroup()

# Результат
print(most_active_authors)
write.csv(most_active_authors, "most_active_authors_30topics.csv")

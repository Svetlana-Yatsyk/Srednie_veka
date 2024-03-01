library(mallet)
library(dendextend)
library(RColorBrewer)

topic_words <- mallet.topic.words(topic.model, normalized = TRUE)

# функция для расчета расстояния Хеллингера
CalcHellingerDist <- function(x) {
  # Количество топиков
  K <- nrow(x)
  # Матрица расстояний
  dist_mx <- matrix(0, nrow=K, ncol=K)
  for(i in 1:K) {
    for(j in i:K) {
      if(i != j) {
        # Расчёт расстояния Хеллингера
        dist_mx[i,j] <- dist_mx[j,i] <- sqrt(sum((sqrt(x[i,]) - sqrt(x[j,]))^2))/sqrt(2)
      }
    }
  }
  return(dist_mx)
}

# Рассчитываем расстояние Хеллингера для матрицы topic_words
dist_mx <- CalcHellingerDist(topic_words)

# Используем иерархическую кластеризацию
dclust <- hclust(as.dist(dist_mx), method = "ward.D")

# Преобразуем в дендрограмму
ddend <- as.dendrogram(dclust)

# Выбираем цветовую палитру
pal8 <- RColorBrewer::brewer.pal(8, "Dark2")

# Отображаем дендрограмму с кластерами топиков
ddend %>% 
  set("branches_k_color", k = 15, value = pal8) %>% 
  set("labels_col", k=15, value = pal8) %>% 
  set("branches_lwd", 2) %>%
  plot(horiz = FALSE, main = "Тематические кластеры")
# Предполагаем, что texts_tbl - это ваш датафрейм
library(dplyr)

# Убедимся, что рабочая директория установлена в нужное место, где будут сохраняться файлы
# setwd("путь_к_вашей_рабочей_директории")

# Проходим по каждой строке датафрейма
for (i in 1:nrow(texts_tbl)) {
  # Получаем article_id и текст для текущей строки
  article_id <- as.character(texts_tbl$article_id[i])
  text <- texts_tbl$text[i]
  
  # Создаем имя файла, соответствующее article_id, с расширением .txt
  file_name <- paste0(article_id, ".txt")
  
  # Записываем текст в файл
  writeLines(text, file_name)
}
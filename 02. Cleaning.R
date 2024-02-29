library(tidyverse)
library(stringr)

# Функция для очистки текста
clean_text <- function(text) {
  # Если в конце строки стоит "-", приклеиваем ее к следующей без добавления пробела
  text <- gsub("-\n", "", text)
  # Остальные переходы строк занменяем на пробел
  text <- gsub("\n", " ", text)
  
  # Удаляем знакм препинания, цифры, специальные символы и латиницу
  text <- gsub("[^А-Яа-я]+", " ", text)
  
  # избавляемся от заглавных букв
  text <- tolower(text)
  
  # удаляем лишние пробелы
  text <- gsub("\\s+", " ", text)
  
  # Удаляем слова короче 3 символов
  text <- gsub("\\b[А-Яа-я]{1,2}\\b", "", text)
  
  # Снова удаляем лишние пробелы
  text <- gsub("\\s+", " ", text)
  text <- trimws(text) # Удаление пробелов в начале и конце строки
  
  return(text)
}


# Функция для обработки и сохранения файлов
process_files <- function(input_dir, output_dir) {
  # Создание выходной директории, если она не существует
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Получение списка всех txt
  files <- list.files(input_dir, recursive = TRUE, pattern = "\\.txt$", full.names = TRUE)
  
  for (file_path in files) {
    text <- read_lines(file_path) %>% paste(collapse = "\n")
    cleaned_text <- clean_text(text)
    
    # Новый путь для сохранения файла в папку "cleaned texts"
    # Используем basename для извлечения имени, игнорируя структуру папок
    output_file_name <- basename(file_path)
    new_file_path <- file.path(output_dir, output_file_name)
    
    # Запись очищенного текста в новый файл
    write_lines(cleaned_text, new_file_path)
    print(paste("Файл сохранен:", new_file_path))
  }
}


# Пути к папкам с инпутом и аутпутом
input_dir <- "постатейно"
output_dir <- "cleaned_txts"

# Запуск функции
process_files(input_dir, output_dir)

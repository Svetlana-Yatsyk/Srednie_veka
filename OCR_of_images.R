library(tesseract)

# Пути к двум папкам с PNG файлами
png_dirs <- c("~/Documents/RESEARCH/rus_hist/SV_topic_modelling/62", "~/Documents/RESEARCH/rus_hist/SV_topic_modelling/67")
# Пути к выходным текстовым файлам
output_txt_files <- c("62.txt", "67.txt")

# Функция для выполнения OCR на всех PNG файлах в директории и сохранения результатов в одном текстовом файле
process_png_directory <- function(png_dir, output_txt_file) {
  # Получаем список всех PNG файлов в директории
  png_files <- list.files(png_dir, pattern = "\\.png$", full.names = TRUE)
  
  # Инициализируем переменную для хранения агрегированного текста
  aggregated_text <- ""
  
  # Цикл по каждому PNG файлу для выполнения OCR
  for (png_file in png_files) {
    # Выполнение OCR на изображении
    ocr_result <- ocr(png_file, engine = tesseract("eng+rus+lat"))
    
    # Добавление результата OCR к агрегированному тексту
    aggregated_text <- paste(aggregated_text, ocr_result, sep = "\n")
  }
  
  # Определение полного пути к выходному текстовому файлу
  output_file_path <- file.path(output_txt_file)
  
  # Сохранение агрегированного текста в текстовый файл
  writeLines(aggregated_text, con = output_file_path)
  
  cat("OCR results for directory", png_dir, "saved to:", output_file_path, "\n")
}

# Обработка каждой директории и сохранение результатов в соответствующий текстовый файл
for (i in seq_along(png_dirs)) {
  process_png_directory(png_dirs[i], output_txt_files[i])
}

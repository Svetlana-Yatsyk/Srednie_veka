library(tesseract)
library(magrittr)
library(pdftools)

pdf_dir <- "archive"
txt_dir <- "txt"

# Рекурсивный поиск pdf в директории и поддиректориях
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)

# Делаем список всех pdf
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

# Проходимся по каждому файлу
for (pdf_file in pdf_files) {
  # транскрибируем
  ocr_result <- pdf_ocr_text(
    pdf_file,
    opw = "",
    upw = "",
    dpi = 300,
    language = "eng+rus+lat",
    options = NULL
  )
  
  # Создаем txt с аналогичным названием
  relative_path <- dirname(gsub(pdf_dir, "", pdf_file))
  output_dir_path <- file.path(txt_dir, relative_path)
  output_file_path <- file.path(output_dir_path, output_file)
  
  # Сохраняем транскрипцию в txt
  writeLines(ocr_result, con = output_file_path)
  
  cat("OCR result for", pdf_file, "saved to:", output_file_path, "\n")
}
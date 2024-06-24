library(udpipe)
library(data.table)

# lemmatization
russian_model <- udpipe_download_model(language = "russian")
latin_model <- udpipe_download_model(language = "latin")

# Load the model
ud_model_ru <- udpipe_load_model(russian_model$file_model)

lemmatize_text_udpipe <- function(text, model) {
  annotated <- udpipe_annotate(model, x = text)
  dt <- as.data.table(annotated)
  return(paste(dt$lemma, collapse=" "))
}

process_files_in_folder_ru <- function(folder_path, output_folder) {
  for (filename in list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)) {
    # Read file
    text <- tolower(readLines(filename, warn = FALSE))
    text <- paste(text, collapse=" ")
    
    # Clean and lemmatize text
    cleaned_text <- clean_text(text)
    lemmatized_text <- lemmatize_text_udpipe(cleaned_text, ud_model_ru)
    
    # Write to output file
    output_file_path <- file.path(output_folder, basename(filename))
    writeLines(lemmatized_text, output_file_path)
  }
}

base_dir <- "~/Documents/RESEARCH/rus_hist/SV_topic_modelling/cleaned_txts/"
output_dir <- "~/Documents/RESEARCH/rus_hist/SV_topic_modelling/lemmatised_txts/"

# Ensure the output directory exists
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Process files in each subdirectory
for (subdir in list.dirs(base_dir, full.names = TRUE, recursive = TRUE)) {
  process_files_in_folder_ru(subdir, output_dir)
}
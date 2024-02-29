#install.packages('devtools')
#install.packages('stopwords')
# devtools::install_github("cpsievert/LDAvis")
#install.packages("pdftools")
#install.packages("tesseract")
#install.packages("magick")
#install.packages("text")
#install.packages("udpipe")
#install.packages("ldatuning")
# install.packages("ggstream")

# Load the libraries
library(pdftools)
library(stringr)
library(magrittr)
library(tesseract)

library(tm)
library(text)
library(stopwords)
library(udpipe)

library(ggplot2)
library(ggstream)
library(dplyr)
library(LDAvis)

library(purrr)
library(tidytext)

library(ldatuning)
library(topicmodels)
library(magick)


# LDA ----
## Preparation ----
txt_files_folder <- "~/Documents/RESEARCH/rus_hist/SV_topic_modelling/lemmatised_txts/"

# List all TXT files
txt_files <- list.files(txt_files_folder, pattern = "\\.txt$", full.names = TRUE)

# Read the text content from the TXT files
text_content <- sapply(txt_files, function(file) {
  readLines(file, warn = FALSE, encoding = "UTF-8")
})

# Create a corpus from the TXT files
corpus <- Corpus(VectorSource(text_content))

# Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

## Choosing the optimal number of topics ----

# Set the seed for reproducibility (so that each time we run the LDA models, 
# the random initialization of topics assignments is the same.)
set.seed(0211)

# Define the number of topics you want to test
n_topics <- c(8, 12, 16, 20, 24, 28)

# Run LDA models for different numbers of topics
text_lda_compare <- map(n_topics, function(k) {
  lda_model <- LDA(x = dtm, k = k, control = list(seed = 0211))
  return(list(model = lda_model, perplexity = perplexity(lda_model)))
})

# Extract perplexity values (the lower the better)
perplexity_values <- map_dbl(text_lda_compare, ~ .x$perplexity)

# Creating a data frame for plotting
model_comparison <- data.frame(
  NumberOfTopics = n_topics,
  Perplexity = perplexity_values
)

# Plotting
library(ggplot2)
ggplot(model_comparison, aes(x = NumberOfTopics, y = Perplexity)) +
  geom_line() +
  geom_point() +
  labs(title = "Perplexity of LDA Models by Number of Topics",
       x = "Number of Topics",
       y = "Perplexity")


## Topic modeling itself----
number_of_topics <- 30
lda_model <- LDA(dtm, k = number_of_topics)

# View topics and terms (adjust as needed)
topics <- topics(lda_model)
terms(lda_model, 30)

top_words <- terms(lda_model, 30)
top_words_df <- as.data.frame(top_words)
write.csv(top_words_df, file = "top10_words_per_topic_30topics.csv", row.names = FALSE)

## Visualisation ----

# Convert the DTM to a format that LDAvis understands
dtm_ldavis <- as.matrix(dtm)
row.names(dtm_ldavis) <- 1:nrow(dtm_ldavis)

K <- 30 # adjust according to N of topics  

# Extract the topic-term matrix
phi <- posterior(lda_model)$terms

# Ensure phi has topics as rows and terms as columns
if (nrow(phi) != K) {
  phi <- t(phi)
}

# Extract the document-topic matrix
theta <- posterior(lda_model)$topics

# Ensure theta has documents as rows and topics as columns
if (ncol(theta) != K) {
  theta <- t(theta)
}

# Compute the frequency of each term in the corpus
freq <- colSums(dtm_ldavis)

# Create the JSON object for LDAvis
json_ldavis <- createJSON(phi = phi, theta = theta, doc.length = rowSums(dtm_ldavis), vocab = colnames(dtm_ldavis), term.frequency = freq)

# Load the JSON object into LDAvis
ldavis <- serVis(json_ldavis, out.dir = 'ldavis_output_top-15', open.browser = TRUE)


# Topic to docs attribution ---- 
# Get the topic probability matrix (documents x topics)
topics_matrix <- as.matrix(posterior(lda_model)$topics)

# Find the dominant topic for each document
dominant_topic <- apply(topics_matrix, 1, which.max)

# Combine the filenames and their dominant topics
file_names <- list.files(path = "~/Documents/RESEARCH/rus_hist/SV_topic_modelling/lemmatised_txts", pattern = ".txt$", full.names = TRUE)
dominant_topics_df <- data.frame(FileName = file_names, DominantTopic = dominant_topic)

# Save to CSV
write.csv(dominant_topics_df, "dominant_topics_per_article_30topics.csv", row.names = FALSE)

# View the data frame
print(dominant_topics_df)

# Looking for trends----
# Data extraction
# Convert Filename column to character type
dominant_topics_df$FileName <- as.character(dominant_topics_df$FileName)
# Get the year
dominant_topics_df$Year <- as.numeric(sapply(strsplit(dominant_topics_df$FileName, "/"), function(x) substr(tail(x, 1), 1, 4)))

yearly_topic_distribution <- dominant_topics_df %>%
  group_by(Year, DominantTopic) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(yearly_topic_distribution, aes(x = Year, y = Count, fill = as.factor(DominantTopic))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_discrete(name = "Dominant Topic") +
  labs(title = "Dominant Topic Trends Over Years",
       x = "Year",
       y = "Number of Cases") +
  theme_minimal()

ggsave("Filtered Dominant Topic Trends Over Years.png")
# For ggstream

ggplot(yearly_topic_distribution, aes(x = Year, y = Count, group = DominantTopic, color = as.factor(DominantTopic))) +
  geom_line() + 
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#FF5733", "#33FF99", "#A633FF", "#FF33A6", "#33A6FF", "#A6AA46", "#FFA633", "#FF33A6", "#33FFA6", "#A6FF33", "#FFA633")) +
  labs(title = "Topic Representation Over Years",
       x = "Year",
       y = "Count",
       color = "Topics") +
  theme_minimal() +
  theme(legend.position = "bottom")
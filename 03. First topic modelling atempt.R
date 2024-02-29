### Topic modeling with R

# This is an R script file, created by Simone
# Everything written after an hashtag is a comment
# Everything else is R code
# To activate the code, place the cursor on the corresponding line
# (or highlight multiple lines/pieces of code) 
# ...and press Ctrl+Enter (or Cmd+Enter for Mac)
# (the command will be automatically copy/pasted into the console)

# before everything starts: check the working directory!
setwd("~/Documents/RESEARCH/rus_hist/SV_topic_modelling")

# required packages:
# install.packages("mallet")
# install.packages("tidyverse")
# install.packages("ggwordcloud")

# load the packages
library(mallet)
library(stringr)
library(tidyverse)
library(ggwordcloud)
library(ggplot2)

### Define variables
# here you define the variables for your topic modeling
num_topics <- 20 # number of topics
num_iterations <- 2000 # number of iterations. В этом отличие от package topic_models
len_split <- 20000 # length of the split texts (they will be the actual documents to work on)

# Prepare the corpus
my_texts <- character()

# data preparation : lemmatization with pymystem3 and stop_words in python
file_list <- list.files("txt", full.names = T)

stop_words <- "~/Documents/RESEARCH/rus_hist/ulysses_topic_modelling/stopwords_ru.txt"


for(i in 1:length(file_list)){
  
  # read text
  loaded_file <- readLines(file_list[i], warn = F)
  loaded_file <- paste(loaded_file, collapse = "\n")
  
  # tokenize
  tokenized_text <- unlist(strsplit(loaded_file, "\\W"))
  tokenized_text <- tokenized_text[which(tokenized_text != "")]
  
  # then save the name of the text
  # (we can re-use the names saved in the list_files variable, by deleting the "corpus/" at the beginning)
  text_name <- gsub(pattern = "corpus/|.txt", replacement = "", x = file_list[i])
  
  # get the number of times we can split it
  split_dim <- trunc(length(tokenized_text)/len_split)
  
  # then do the actual splitting
  tokenized_text_split <- split(tokenized_text, ceiling(seq_along(tokenized_text)/len_split))
  
  # last part will be shorter than the set length, so better merge it with the previous one
  tokenized_text_split[[length(tokenized_text_split)-1]] <- c(tokenized_text_split[[length(tokenized_text_split)-1]], tokenized_text_split[[length(tokenized_text_split)]])
  tokenized_text_split <- tokenized_text_split[-length(tokenized_text_split)]
  
  # then collapse back the split texts into a single string 
  tokenized_text_split <- unlist(lapply(tokenized_text_split, function(x) paste(x, collapse = " ")))
  
  # finally we perform a loop on the split texts to incrementally save all in just one variable 
  for(n in 1:length(tokenized_text_split)){
    # put to lowercase
    my_texts <- c(my_texts, tolower(tokenized_text_split[n]))
    # assign names
    names(my_texts)[length(my_texts)] <- paste(text_name, n, sep = "_")
  }
  print(i)
  
}

# Define a function for text cleaning
clean_text <- function(texts) {
  cleaned_texts <- sapply(texts, function(text) {
    text <- gsub("\n", " ", text) # Replace newlines with spaces
    text <- gsub("-\\s?|\\s?-", "", text) # Remove "- " and " -"
    text <- gsub("\\b\\w{1,2}\\b", "", text) # Remove words shorter than 3 letters
    text <- gsub("\\b[a-zA-Z]{1,}\\b", "", text) # Remove all Latin-script words
    text <- gsub("\\s+", " ", text) # Replace multiple spaces with a single space
    return(text)
  })
  return(cleaned_texts)
}

# Apply the text cleaning function to your texts
my_cleaned_texts <- clean_text(my_texts)

# Preparation of texts for topic model: Import texts into Mallet format
text.instances <- mallet.import(text.array = my_cleaned_texts, 
                                stoplist = stop_words, # Removes the stopwords (i.e., function words)
                                id.array = names(my_texts))

# Define all variables for the topic model (It's advised not to change alpha and beta without understanding their impact)
topic.model <- MalletLDA(num.topics=num_topics, alpha.sum = 1, beta = 0.1)

# Load documents for topic modeling
topic.model$loadDocuments(text.instances)

# Prepare topic models' features (Values for alpha optimization can be left as defaults)
topic.model$setAlphaOptimization(20, 50) # Optimization step for alpha parameter

# Create the topic models by training
topic.model$train(num_iterations)

# Calculate topic distribution per document
doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)

# Calculate word distribution per topic
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)

# Initialize a dataframe to store the top words per topic
top_words <- data.frame()
firstwords <- character()

# Loop through each topic to visualize the top words
for(i in 1:num_topics){
  words.per.topic <- mallet.top.words(topic.model, word.weights = topic.words[i,], num.top.words = 20)
  words.per.topic$topic <- i
  top_words <- rbind(top_words, words.per.topic)
  firstwords[i] <- paste(words.per.topic$term[1:5], collapse = " ")
}

# Visualize the table of top words per topic
View(top_words)

# visualize the first five words per topic
names(firstwords) <- paste("Topic", 1:length(firstwords))
firstwords

### Wordcloud visualization

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
ggsave(p1, filename = "20_Topics_wordcloud_2000iterations.png", scale = 4)

### Alternative visualization (barcharts)

#p2 <- top_words %>%
  #mutate(term = reorder_within(term, weight, topic)) %>%
  #ggplot(aes(weight, term, fill = factor(topic))) +
  #geom_col(show.legend = FALSE) +
  #facet_wrap(~ topic, scales = "free") +
  #scale_y_reordered()

# show it
#p2

# save it
#ggsave(p2, filename = "Topics_barchart.png", scale = 2)

### Cluster the documents per topic

# first assign names that correspond to:
# the first five words of the topics
colnames(doc.topics) <- firstwords
# the titles of the documents
rownames(doc.topics) <- names(my_texts) # to make them look better, remove "corpus" from the names

# visualize an heatmap and save it to a file
png(filename = "heatmap_12_topics_2000_iterarions.png", width = 4000, height = 4000)
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
png(filename = "heatmap_simple_12_topics_2000_iterarions.png", width = 1500, height = 1500)
heatmap(doc.topics.simple, margins = c(50,50), cexRow = 2, cexCol = 2)
dev.off()


# пытаюсь визуализировать динамику---

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggridges)

# Create a data frame with the document-topic proportions and years
doc_topics <- data.frame(doc_id = rownames(doc.topics), doc.topics)
doc_years <- data.frame(doc_id = names(my_texts), file_name = gsub(".*/|_\\d+.*", "", names(my_texts)))
doc_years$year <- gsub("_.*", "", doc_years$file_name)
doc_data <- left_join(doc_topics, doc_years, by = "doc_id")

# Melt the data frame to long format for visualization
melted_data <- doc_data %>%
  gather(topic, proportion, -doc_id, -year) %>%
  arrange(year)

# удаляю лишнее из столбца "год" и разделяю дублирующиеся года
topics_over_years <- subset(melted_data, !(grepl("group", topic) | grepl("file_name", topic)))
topics_over_years$year <- gsub("2017-2018", "2017", topics_over_years$year)
topics_over_years$year <- gsub("2015-2016", "2015", topics_over_years$year)

subset_rows <- topics_over_years[topics_over_years$year == 2017, ]

# Создание дубликатов и замена значения year
subset_rows$year <- 2018

# Объединение дубликатов с исходной таблицей
topics_over_years_upd <- rbind(topics_over_years, subset_rows)

# нужно нормализовать proportion!
topics_over_years_upd <- topics_over_years_upd %>%
  mutate(normalized_proportion = (proportion - min(proportion)) / (max(proportion) - min(proportion)))

# Фильтрация по интересующей теме
topic_of_interest <- "историк.русский.феодализм.социальный.феодальный"
filtered_data <- topics_over_years_upd %>%
  filter(topic == topic_of_interest)

# Построение графика
ggplot(filtered_data, aes(x = year, y = normalized_proportion)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Normalized Proportion",
    title = paste("Presence of Topic:", topic_of_interest)
  )
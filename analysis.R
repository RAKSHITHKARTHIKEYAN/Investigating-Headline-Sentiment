#install.packages(c("tidyverse", "tm", "textclean", "tidytext", "textdata", "sentimentr"))

library(tidyverse)
library(tm)
library(textclean)
library(tidytext)
library(textdata)
library(sentimentr)

# Load data
data <- read.csv("MN-DS-news-classification.csv")
head(data)

# Clean headlines and content (basic preprocessing)
data$headline_clean <- tolower(data$title) %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

data$content_clean <- tolower(data$content) %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()



# Function to classify headlines as 'Question', 'Sensationalist', or 'Declarative'
classify_headline_extended <- function(headline) {
  headline <- tolower(headline)  # Convert to lowercase to standardize
  
  # Rule 1: Question-Based Headlines
  if (grepl("\\?", headline)) {
    return("Question")  # Headlines with a '?' are considered questions
  }
  
  # Rule 2: Sensationalist Headlines (with an extended list of emotional expressions and clickbait phrases)
  sensational_keywords <- c("shocking", "explosive", "unbelievable", "must-read", 
                            "crazy", "revealed", "surprising", "amazing", "you won't believe", 
                            "scandal", "rumor", "leaked", "jaw-dropping", "mind-blowing", 
                            "bizarre", "truth about", "the untold story", "the real reason", 
                            "you’ll never guess")
  
  # Check for any sensational keywords or clickbait patterns in the headline
  if (any(sapply(sensational_keywords, function(x) grepl(x, headline)))) {
    return("Sensationalist")  # Headlines with sensational keywords
  }
  
  # Rule 3: Declarative Headlines
  return("Declarative")  # Everything else is treated as declarative
}

# Load the dataset
# Replace "news_data.csv" with your actual dataset file path

head(data)
# Apply classification function to the dataset
# Assuming the dataset has a 'headline' column
data$headline_type <- sapply(data$title, classify_headline_extended)

# View the first few rows of the dataset with classified headlines
head(data)


# Apply classification function to the dataset
# Assuming the dataset has a 'headline' column
data$headline_type <- sapply(data$title, classify_headline_extended)

# View the first few rows of the dataset with classified headlines
head(data)

# Analyze the distribution of headline types
headline_distribution <- table(data$headline_type)
print("Headline Type Distribution:")
print(headline_distribution)


# Load necessary libraries
library(caret)
library(e1071)

# Data inspection
print(dim(data))  # Check dimensions
print(colnames(data))  # Ensure necessary columns exist

str(data)
# Feature creation

# Install syuzhet for sentiment analysis
#install.packages("syuzhet")
library(syuzhet)

# Compute sentiment scores for the content column
data$sentiment_score <- get_sentiment(data$title, method = "afinn")

features <- data[, c("headline_type", "sentiment_score", "content_clean"), drop = FALSE]
features <- na.omit(features)  # Remove missing rows

# Verify features
print(dim(features))  # Should have non-zero rows and columns
print(head(features))  # Inspect first rows




# Map headline types to numeric values
data$headline_type_numeric <- as.numeric(factor(data$headline_type, 
                                                levels = c("Declarative", "Sensationalist", "Question")))

# Target variable
target <- data$headline_type_numeric

str(data)


# Visualize the headline types with a bar plot
library(ggplot2)


# Time-based analysis: Are certain types of headlines more common over time?
# Convert 'date' column to Date type (assuming there's a 'date' column in the dataset)
data$date <- as.Date(data$date, format = "%Y-%m-%d")


# Data Visualizations ----

# Group by headline type and date, then summarize counts
library(dplyr)
time_based_analysis <- data %>%
  group_by(date, headline_type) %>%
  summarize(count = n(), .groups = "drop")



# Line plot showing trends in headline types over time
ggplot(
  time_based_analysis, aes(x = date, y = count, color = headline_type, group = headline_type)) +
  geom_line(size = 1) +
  labs(title = "Trends in Headline Types Over Time",
       x = "Date",
       y = "Count",
       color = "Headline Type") +
  theme_minimal()

# Source-based analysis: Which sources use more sensationalist or question-based headlines?
# Assuming the dataset has a 'source' column
source_analysis <- data %>%
  group_by(source, headline_type) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(desc(count))

# Bar plot for headline types by source (top 10 sources)
top_sources <- source_analysis %>%
  filter(source %in% head(unique(data$source), 10))  # Get top 10 sources for simplicity

ggplot(top_sources, aes(x = source, y = count, fill = headline_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Headline Types by Source (Top 10 Sources)",
       x = "Source",
       y = "Count",
       fill = "Headline Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compare word lengths for different headline types
data$word_count <- sapply(strsplit(as.character(data$headline), " "), length)


# Filter sensationalist headlines and summarize data
sensationalist_data <- data %>%
  filter(headline_type == "Sensationalist") %>%
  group_by(source) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(desc(count))


# Install the wordcloud package if not installed
# install.packages("wordcloud")

# Filter sensationalist headlines
sensationalist_headlines <- data %>%
  filter(headline_type == "Sensationalist") %>%
  pull(title)

# Preprocess text and generate a word cloud
library(wordcloud)
library(tm)
corpus <- Corpus(VectorSource(sensationalist_headlines))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Create a word cloud
wordcloud(words = corpus, max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Reds"))















# ML ----


# Load required libraries
library(caret)
library(e1071)
library(dplyr)
library(tm)

news_data <- data

news_data$headline_type <- as.factor(news_data$headline_type)  # Convert to factor

# Preprocess the `title` column for text analysis
news_data$title <- tolower(news_data$title)                     # Convert to lowercase
news_data$title <- removePunctuation(news_data$title)           # Remove punctuation
news_data$title <- removeNumbers(news_data$title)               # Remove numbers
news_data$title <- stripWhitespace(news_data$title)             # Remove extra spaces

# Create a Document-Term Matrix (DTM) from `title`
dtm <- DocumentTermMatrix(Corpus(VectorSource(news_data$title)))

# Remove sparse terms to avoid too many columns
dtm <- removeSparseTerms(dtm, 0.99)

# Convert the DTM to a dataframe
dtm_df <- as.data.frame(as.matrix(dtm))
colnames(dtm_df) <- make.names(colnames(dtm_df), unique = TRUE)  # Ensure unique column names

# Combine DTM with the target variable
features <- cbind(dtm_df, headline_type = news_data$headline_type)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(features$headline_type, p = 0.7, list = FALSE)
train_data <- features[trainIndex, ]
test_data <- features[-trainIndex, ]

# Logistic Regression Model
logistic_model <- glm(headline_type ~ ., data = train_data, family = binomial)

# Predict with Logistic Regression
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")
logistic_predicted_classes <- ifelse(logistic_predictions > 0.5, levels(features$headline_type)[1], 
                                     levels(features$headline_type)[2])

# Evaluate Logistic Regression
confusion_matrix_logistic <- confusionMatrix(as.factor(logistic_predicted_classes), test_data$headline_type)
print("Logistic Regression Confusion Matrix:")
print(confusion_matrix_logistic)

# SVM Model
svm_model <- svm(headline_type ~ ., data = train_data, kernel = "linear", cost = 1)

# Predict with SVM
svm_predictions <- predict(svm_model, newdata = test_data)

# Evaluate SVM
confusion_matrix_svm <- confusionMatrix(svm_predictions, test_data$headline_type)
print("SVM Confusion Matrix:")
print(confusion_matrix_svm)

# Accuracy for Logistic Regression
accuracy_logistic <- sum(logistic_predicted_classes == test_data$headline_type) / nrow(test_data)
print(paste("Accuracy of Logistic Regression: ", round(accuracy_logistic * 100, 2), "%"))

# Accuracy for SVM
accuracy_svm <- sum(svm_predictions == test_data$headline_type) / nrow(test_data)
print(paste("Accuracy of SVM: ", round(accuracy_svm * 100, 2), "%"))




# Sentiment Analysis on Amazon Reviews

# Install packages
install.packages("tm")

# Load library
library(tm)

# Load the amazon review data and explore
amazon <- northeastern_class
amazon_reviews <- as.character(amazon$Body)
class(amazon_reviews)

# Load the positive and negative lexicon data and explore

head(positive.lexicon)
class(positive.lexicon)
head(negative.lexicon)
class(negative.lexicon)

# Remove stop words, punctuations, numbers from all the reviews after coverting it to a Corpus and inspect
reviews_corpus <- Corpus(VectorSource(amazon_reviews))
inspect(reviews_corpus)
filtered_corpus_no_stopwords <- tm_map(reviews_corpus, removeWords, stopwords('english'))
inspect(filtered_corpus_no_stopwords)
filtered_corpus_no_puncts <- tm_map(filtered_corpus_no_stopwords, removePunctuation)
inspect(filtered_corpus_no_puncts)
filtered_corpus_no_numbers <- tm_map(filtered_corpus_no_puncts, removeNumbers)
inspect(filtered_corpus_no_numbers)
filtered_corpus_no_whitespace <- tm_map(filtered_corpus_no_numbers, stripWhitespace)
inspect(filtered_corpus_no_whitespace)
filtered_corpus_to_lower <- tm_map(filtered_corpus_no_whitespace, content_transformer(tolower))
inspect(filtered_corpus_to_lower)
corpus <- tm_map(filtered_corpus_to_lower, removeWords, c("amazon", "echo", "alexa"))
inspect(corpus)

# Load the stop words text file and explore
stop_words <- stopwords_en
head(stop_words)
class(stop_words)

# Remove stop words of the external file from the corpus and whitespaces again and inspect
stopwords_vec <- as.data.frame(stop_words)
class(stopwords_vec)
final_corpus_no_stopwords <- tm_map(corpus, removeWords, stopwords_vec[,1]) 
inspect(final_corpus_no_stopwords)
final_corpus <- tm_map(final_corpus_no_stopwords, stripWhitespace)
inspect(final_corpus)

# Character representation of the corpus of first review
final_corpus[[1]]$content
class(final_corpus)

# Stem the words to their root of all reviews present in the corpus
#install.packages('SnowballC')
stemmed_corpus <- tm_map(final_corpus, stemDocument)

# Building a term document matrix of the stemmed corpus
TDM_corpus <- TermDocumentMatrix(stemmed_corpus)
TDM_corpus
inspect(TDM_corpus[1:100, 1:5])
findFreqTerms(TDM_corpus, 5)                    ##terms occurring with a minimum frequency of 5

install.packages('tokenizers')
library(tokenizers)

# Calculating the count and percentage of total positive and negative words in each review and
# Labeling each review as either negative or positive
total_pos_count <- 0
total_neg_count <- 0
pos_count_vector <- c()
neg_count_vector <- c()
for(i in 1:500){
  corpus_words<- list(tokenize_words(stemmed_corpus[[i]]$content))
  print(intersect(unlist(corpus_words), unlist(positive.lexicon))) ## positive words in current review
  pos_count <- length(intersect(unlist(corpus_words), unlist(positive.lexicon)))
  print(intersect(unlist(corpus_words), unlist(negative.lexicon))) ## negative words in current review
  neg_count <- length(intersect(unlist(corpus_words), unlist(negative.lexicon)))
  if(pos_count>neg_count){
    print("It's a positive review")
  } else{
    print("It's a negative review")
  }
  total_count_for_current_review <- pos_count + neg_count ## current positive and negative count
  pos_percentage <- (pos_count*100)/total_count_for_current_review
  neg_percentage <- (neg_count*100)/total_count_for_current_review
  print(pos_percentage)                          ## current positive percentage
  print(neg_percentage)                          ## current negtive percentage
  total_pos_count <- total_pos_count + pos_count ## overall positive count
  total_neg_count <- total_neg_count + neg_count ## overall negative count
  pos_count_vector <- append(pos_count_vector, pos_count)
  neg_count_vector <- append(neg_count_vector, neg_count)
}

# Calculating overall percentage of positive and negative words of all the reviews
total_pos_count                                  ## overall positive count
total_neg_count                                  ## overall negative count
total_count <- total_pos_count + total_neg_count
overall_positive_percentage <- (total_pos_count*100)/total_count
overall_negative_percentage <- (total_neg_count*100)/total_count
overall_positive_percentage                      ## overall positive percentage
overall_negative_percentage                      ## overall negative percentage

# Visualization of positive and negative word count for all the reviews
review_count_frame <- data.frame(matrix(c(pos_count_vector, neg_count_vector), nrow = 500, ncol = 2))
colnames(review_count_frame) <- c("Positive Word Count", "Negative Word Count")
barplot(review_count_frame$`Positive Word Count`, ylab = "Count", xlab = "Reviews from 1 to 500",  
        main = "Positive word count of Reviews", col="lightblue")
barplot(review_count_frame$`Negative Word Count`, ylab = "Count", xlab = "Reviews from 1 to 500",  
        main = "Negative word count of Reviews", col="lightblue")

# Visualization of Overall positive and negative reviews
percent_vec <- c(overall_positive_percentage, overall_negative_percentage)
percent_frame <- as.data.frame(percent_vec)
rownames(percent_frame) <- c("Positive Reviews","Negative Reviews")
colnames(percent_frame) <- c("Percentage")
percentage <- t(percent_frame)
barplot(percentage, ylab = "Percentage", 
        main = "Sentiment Analysis of Amazon Reviews", col="lightblue")
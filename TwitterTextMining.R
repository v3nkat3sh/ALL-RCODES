# install.packages("devtools")
# install.packages("rjson")
# install.packages("bit64")
# install.packages("httr")
# install.packages("plyr")
# install.packages("twitteR")

# sessionInfo()
api_key <- "hdDAc1EfPK3luMJacAmPA5kkG"
api_secret <- "QxSKhnhVl1Qnk0KVx8cjDlfiQmdUPRtFf1Kj5aBVa4j2pcVu6D"
access_token <- "2598434120-4rcpUjTYkI6DJwSYree60EYABHAP0H6V3bXPgyY"
access_token_secret <- "3OyGcOZUPbYYALLRCxRAxXtdOl4H8trtUqVdD3Azytgpg"

#INSTALL RTOOLS
find_rtools()
# install.packages("rtools")
# devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0",version="0.6.1")
library(devtools)
library(plyr)
library(twitteR)

# install.packages("httr")
# install.packages("twitteR")
# library(twitteR)
# library(devtools) #if not installed, do that obviously

#A restart of R might be necessary if you previously had httr installed.
# library(httr)
# setup_twitter_oauth(consumerKey, consumerSecret, accessKey, accessSecret)

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)



tweets=searchTwitter('#rape',n=1000)
df <- do.call("rbind", lapply(tweets, as.data.frame))
# install.packages("C:/Users/Admin/Downloads/twitteR_1.1.8.tar.gz",repos=NULL, type="source",dependencies = TRUE)
# install.packages("C:/Users/Admin/Downloads/plyr_1.8.2.tar.gz",repos=NULL, type="source",dependencies = TRUE)
# install.packages("C:/Users/Admin/Downloads/httr_0.6.1.tar.gz",repos=NULL, type="source",dependencies = TRUE)
# install.packages('tm')
library(tm)
text <- df$text

review_source <- VectorSource(text)
corpus <- Corpus(review_source)

corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,content_transformer(tolower))
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,removeWords,stopwords("english"))

dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

frequency <- colSums(dtm2)
frequency <- sort(frequency,decreasing=TRUE)
head(frequency,n=200)
#install.packages("wordcloud")
library("wordcloud")

trunc_freq <- frequency[2:100]
frequency[1]
words <- names(frequency)
wordcloud(words[1:50],frequency[1:50])
names_freq <- names(frequency)
plot(factor(names(trunc_freq)),trunc_freq)
names(frequency)
plot(frequency)
frequency
# install.packages("ggplot2")

freq_data <- data.frame(names(trunc_freq),trunc_freq)
names(freq_data)


View(freq_data)

# type in URL- https://apps.twitter.com/
# then Sign in
# click on crate New App
# Fill up the details - click on create your twitter Application
# http://machinelearningblogs.com

#Now go to Key and Access Tokens Tab.
# Note down the Consumer Key (API Key) and Consumer Secret (API Secret).

# Then click on Create my access Token
#Note down 	Access Token and 	Access Token Secret.



install.packages("devtools")
install.packages("rjson")
install.packages("bit64")
install.packages("httr")
install.packages("plyr")
install.packages("twitteR")

api_key<-"n68jhH8YTBzFR7pozGLigLVvg"
api_secret<-"sfLqH4uC616JabFRbkaQd4GawIv4iNldP4XgXESe5pr6TzDdup"
access_token<-"23561023-Wd3MInEATsinDzmXteHSS5QDqhKuWpCviITv0r7Nc"
access_token_secret<-"MqkdJToRqrVWX9VCQMo3QL4YJtmHttSDaaKSg2tLWexn6"

#find_rtools()
install.packages("rtools")
devtools::install_github("jrowen/twitteR",ref="aauth_httr_1_0",version="0.6.1")
library(devtools)
library(twitteR)
library(httr)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets=searchTwitter('#OmPuri',n=100)
tweets=searchTwitter('#Sasikala',n=100)
tweets=searchTwitter('#Jallikattu',n=100)

df<-do.call("rbind",lapply(tweets,as.data.frame))


install.packages("tm")

library(tm)

text<-df$text
review_source<-VectorSource(text)
corpus<-Corpus(review_source)
#corpus<-tm_map(corpus,content_transformer(fucntion(x) iconv(x, sub='byte')))
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,content_transformer(tolower))
corpus<-tm_map(corpus,stripWhitespace)
corpus<-tm_map(corpus,removeWords,stopwords("english"))

# write function to remove links


t.tdm<-TermDocumentMatrix(corpus)
t.m<-as.matrix(t.tdm)
t.v<-sort(rowSums(t.m),decreasing = TRUE)
t.d<-data.frame(word=names(t.v),freq=t.v)

install.packages("wordcloud")
library("wordcloud")

#pal2<-brewer.pal(8,"Dark2")

png("D:/R/test2.png",width=3280,height = 4280)
wordcloud(t.d$word,t.d$freq,scale=c(8,.2),min.freq = 3,max.words=Inf,random.order = FALSE,rot.per=.15)
dev.off()

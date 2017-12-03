##Sentiment Analysis##

install.packages('sentimentr')
#or
#library(devtools)
#install_github('trinker/sentimentr')

library(sentimentr)
library(dplyr)

#aggregated sentiment analysis

sentiment_by('My life has become terrible since I met you and lost money', by = NULL)

#sentence-level sentiment analysis

sentiment('I am not very happy. He is very happy')

#extracting sentiment terms

'My life has become terrible since I met you and lost money' %>% extract_sentiment_terms()

#highlighting

'My life has become terrible since I met you and lost money. But I still have got a little hope left in me' %>% 
sentiment_by(by = NULL) %>%
highlight()

----------------------------------------
##Using RSentiment package
install.packages('RSentiment') 
library(RSentiment)

#Remove NAs and Blanks Rows int the responses column
Data<-data_27112017[!(is.na(data_27112017$Q001_999) | data_27112017$Q001_999==""), ]

##Calculate the sentiment score and type

Data1<-Data[1:100,]
Data1$Sentiment<-calculate_sentiment(Data1$Q001_999)

---------------------------------------------------------
##Perfectly Working script (Output based on this code)
library(qdap)
install.packages('magrittr')
library(magrittr)
df5<-data_27112017 %$% polarity(Q001_999, ResponseID)
df5<-as.data.frame(df5)
str(df5)

df6<-df5
df6$all.pos.words<-as.character(df6$all.pos.words)
df6$all.neg.words<-as.character(df6$all.neg.words)

write.csv(df6,'Sentiment_Analysis_Navin_11302017.csv')

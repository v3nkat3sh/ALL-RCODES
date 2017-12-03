# 1
# retrieved the titles of the XKCD web comics 
# install.packages("RXKCD")
# install.packages("tm")
# install.packages("wordcloud")
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)
path <- system.file("xkcd", package = "RXKCD")
datafiles <- list.files(path)
xkcd.df <- read.csv(file.path(path, datafiles))
xkcd.corpus <- Corpus(DataframeSource(data.frame(xkcd.df[, 3])))
xkcd.corpus <- tm_map(xkcd.corpus, removePunctuation)
xkcd.corpus <- tm_map(xkcd.corpus, tolower)
xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, stopwords("english")))
xkcd.corpus <- tm_map(xkcd.corpus, PlainTextDocument)
tdm <- TermDocumentMatrix(xkcd.corpus)
tdm
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=1280,height=800)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()

# 2
# install.packages("XML")
require(XML)
require(tm)
require(wordcloud)
require(RColorBrewer)
u = "http://cran.r-project.org/web/packages/available_packages_by_date.html"
t = readHTMLTable(u)[[1]]
ap.corpus <- Corpus(DataframeSource(data.frame(as.character(t[,3]))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, tolower)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
ap.corpus <- tm_map(ap.corpus, PlainTextDocument)
ap.tdm <- TermDocumentMatrix(ap.corpus)
ap.tdm
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

# TRY STEMMING/LEMMATIZING
# TRY ON A DIFFERENT DATASET
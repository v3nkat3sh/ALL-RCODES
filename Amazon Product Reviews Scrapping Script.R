library('rvest')
library('httr')
library('XML')

Web <- "https://www.amazon.com/product-reviews/B01B25PKB0/ref=cm_cr_getr_d_paging_btm_3?ie=UTF8&showViewpoints=3&pageNumber=10"
Data <- data.frame()

for(N in 1:601){
Web <- gsub(" ","",gsub("=2",paste("=",as.character(N)),Web))
Page_No <- c(N,N,N,N,N,N,N,N,N,N)
Webpage <- read_html(Web)
Node_Ratings <- html_nodes(Webpage,"#cm_cr-review_list .review-rating")  
Web_Ratings <- html_attr(Node_Ratings,"class")
Rating <- as.numeric(substr(Web_Ratings,27,27))


Node_Review <- html_nodes(Webpage,".review-format-strip+ .review-data")  
Review <- html_text(Node_Review)

Node_Date <- html_nodes(Webpage,"#cm_cr-review_list .review-date")  
Date <- html_text(Node_Date)

Node_Title <- html_nodes(Webpage,".a-color-base")  
Title <- paste(Page_No,html_text(Node_Title),sep='---')

Z <- cbind(Date,Title,Review,Rating)
Data <- rbind(Data,Z)}

write.csv(Data,'Data.csv')



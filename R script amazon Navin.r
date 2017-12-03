library('rvest')
library('httr')
library('XML')

Web <- "https://www.amazon.com/Apple-42mm-Smart-Watch-Aluminum/product-reviews/B00WUKULAC/ref=cm_cr_arp_d_paging_btm_4?ie=UTF8&reviewerType=avp_only_reviews&showViewpoints=1&sortBy=helpful&pageNumber=4"
Data <- data.frame()

for(N in 1:250){
WebN <- gsub(" ","",gsub("=4",paste("=",as.character(N)),Web))
Page_No <- c(N,N,N,N,N,N,N,N,N,N)
Webpage <- read_html(WebN)
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



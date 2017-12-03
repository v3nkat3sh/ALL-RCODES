#install.packages("XML")
#install.packages("rvest")

library(XML)
library(rvest)

url <- "https://www.glassdoor.co.in/Job/india-oracle-jobs-SRCH_IL.0,5_IN115_KE6,12_IP"
final_roles<- c()
final_links <- c()
final_locations <- c()
final_duration <- c()

for (i in 1:5){
  mainUrl <- paste0(url, i, ".htm")
  suppressWarnings(download.file(mainUrl, "temp.html"))
  
  temp <- read_html("temp.html")
  
  temp1 <- htmlParse(temp)
  
  roles <- xpathSApply(temp1, "//div[@class = 'flexbox']/div/a[@class = 'jobLink']", xmlValue)
  link <- paste0("https://www.glassdoor.co.in",xpathSApply(temp1, "//div[@class = 'flexbox']/div/a[@class = 'jobLink']/@href"))
  loc <- xpathSApply(temp1, "//div[@class = 'flexbox empLoc']/div", xmlValue)
  days <- xpathSApply(temp1, "//div[@class = 'flexbox empLoc']/span[@class = 'hideHH nowrap']", xmlValue)
  
  final_roles <- c(final_roles, roles)
  final_links <- c(final_links, link)
  final_locations <- c(final_locations, loc)
  final_duration <- c(final_duration, days)
  
  
}

final_data <- data.frame(final_roles,final_locations,final_duration,final_links)

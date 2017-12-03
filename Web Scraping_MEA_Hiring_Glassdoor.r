##Data Cleaning and merging
library(rvest)
library(httr)
library(XML)
library(plyr)

##Required Function for extratcting numbers
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

##Retreving the weblinks
Weblinks <- data.frame()
####
for (i in 709:nrow(MEA_Links)){
  Webpage <- read_html(as.character(MEA_Links$Links[i]))
  Nodes <- html_nodes(Webpage,'.jobLink')
  Weblink <- data.frame()
  Weblink <- html_attr(Nodes,'href')
  WeblinkM <- 'https://www.glassdoor.co.in' 
  WeblinkA <- paste0(WeblinkM,Weblink)
  WeblinkA <- data.frame(WeblinkA[-c(seq(1,length(WeblinkA),2))])
  colnames(WeblinkA) <- "Weblinks"
  ifelse(length(Weblink)==0,N <- 1,Weblinks <- rbind(Weblinks,WeblinkA))
}

###Retreving information from the data
JOBS <- data.frame()

###(run from this)
for(i in 766:nrow(Weblinks)){
  Job <- data.frame()
  Webpage <- read_html(as.character(Weblinks$Weblinks[i]))
  Jobtitle <- html_nodes(Webpage,'#HeroHeaderModule .strong')
  Company <- html_nodes(Webpage,'.strong.ib')
  Location <- html_nodes(Webpage,'#HeroHeaderModule .subtle')
  Date_Posted <- html_nodes(Webpage,'.nowrap')
  Job_Description <- html_nodes(Webpage,'.desc')
  
  ifelse((length(Jobtitle)==0),Jobtitle <- c(NA),
         Jobtitle <- html_text(Jobtitle[1]))
  
  ifelse ((length(Company) == 0) , Company <- c(NA),
          Company <- html_text(Company))
  
  ifelse((length(Location)==0),Location <- c(NA),
         Location <- html_text(Location))
  
  ifelse((length(Date_Posted)==0),Date_Posted <- c(NA),
         Date_Posted <- (as.factor(html_text(Date_Posted[1]))))
  
  ifelse((length(Job_Description)==0),Job_Description <- c(NA),
         Job_Description <- html_text(Job_Description))
  
  Link <- Weblinks$Weblinks[i]
  
  JOB <- data.frame(Jobtitle,Company,Location,Date_Posted,Job_Description,Link)
  JOBS <- rbind(JOBS,JOB)
}

###Providing the dates
JOBS$Date_Posted <- numextract(JOBS$Date_Posted)  
JOBS$Date_Posted <- Sys.Date()-(as.numeric(JOBS$Date_Posted))

###Converting it to CSV
write.csv(JOBS,'JOBS.csv')
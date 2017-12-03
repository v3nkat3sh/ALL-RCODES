####Bengaluru
library('rvest')
library('httr')
library('XML')
library('RCurl')

ContinueLoop = TRUE
while(ContinueLoop){

Link <- "https://in.bookmyshow.com/buytickets/kabali-bengaluru/movie-bang-ET00039091-MT/20160722" ### Link Should be provided based on the movie
Web <- read_html(Link)
Node <- html_nodes(Web,'.__venue-name')

##Length of Node indicates the number of theaters the ticket booking has opened 

if(length(Node) >= 1) {
  
    system('CMD /C "ECHO Book kabali tickets as soon as possible in innovative multiplex"', 
           invisible=FALSE, wait=FALSE)
    browseURL(Link)
    ContinueLoop = FALSE
    shell.exec(Link)    
  }
 }

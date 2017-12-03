#Loading the rvest package
library('rvest')
library('httr')
library('XML')

#Specifying the url for desired website to be scrapped
url_website <- "https://www.linkedin.com/jobs/search?keywords=Ibm&location=Worldwide&locationId=OTHERS.worldwide&trk=jobs_jserp_search_button_execute&searchOrigin=JSERP&applyLogin="

#Reading the HTML code from the website
webpage <- read_html(url_website)

#Using CSS selectors to scrap the Company Name section
Company_Name_html <- html_nodes(webpage,'company-name-text')

#Converting the Company Name to text
Company_Name_data <- html_text(Company_Name_html)

#Let's have a look at the rankings
head(Company_Name_data)

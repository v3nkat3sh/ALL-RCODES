###### UK Monthly, OCC REPORTING CODE ######
JulytoSep<-Microsoft_International_UK_Report_September_2016[,c(1:28)]
Oct<-Microsoft_International_UK_Report_October_2016[,c(1:28)]
Nov<-Microsoft_International_UK_Report_November_2016[,c(1:28)]
Combined_Data<- rbind(JulytoSep,Oct,Nov)
Combined_Data$FY<-"FY17"

##Pulling all the Playstation entries initially itself without any category / subcategory specifics or limits###
##Deleting the Playstation mention records from the larger dataset itself##
Playstation_Rows1<-grep("Playstation",Combined_Data$Product.Campaign)
Playstation_Rows2<-grep("PlayStation",Combined_Data$Product.Campaign)
Playstation_Records<-Combined_Data[c(Playstation_Rows1,Playstation_Rows2),]

###Defining Dataset without Playstation_Records##
Data1<-Combined_Data[-c(Playstation_Rows1,Playstation_Rows2),]

###Reclassify all Surface entries at Subcategory level from Tablet PC to Desktops & Notebooks###
Surface_Rows_Reclassification<-grep('Surface', Data1$Product.Campaign)
Data1$Subcategory[Surface_Rows_Reclassification]<-'PC Laptops/Notebooks'

#SUbsetting only the required categories
Category_Broadcasting<-subset(Data1,Category == "Broadcasting")
Category_ComputerComponentsPeripherals<-subset(Data1,Category == "Computer Components/Peripherals")
Category_Computer_Hardware_Excl_Network<-subset(Data1,Category == "Computer Hardware (Excl. Network)")
Category_Computer_Software<-subset(Data1,Category == "Computer Software")
Category_Media_Publishing<-subset(Data1,Category == "MEDIA AND PUBLISHING")
Category_Other_Services<-subset(Data1,Category == "Other Services")
Category_Telecommunications<-subset(Data1,Category == "TELECOMMUNICATIONS")

#rbinding all the required categories into one dataframe
Data<-rbind(Category_Broadcasting,Category_ComputerComponentsPeripherals,Category_Computer_Hardware_Excl_Network,Category_Computer_Software,Category_Media_Publishing,Category_Other_Services,Category_Telecommunications)

##Classification Starts
##Segregating Advertisers into MAGSA & Others
##MAGS (Microsoft/Apple/Google/Samsung) Advertisers
Microsoft_Adv_row <-grep('Microsoft',Data$Advertiser)
Microsoft_adv <- Data[Microsoft_Adv_row,]
Apple_Adv_row <-grep('Apple',Data$Advertiser)
Apple_adv <- Data[Apple_Adv_row,]
Google_Adv_row <-grep('Google',Data$Advertiser)
Google_adv <- Data[Google_Adv_row,]
Samsung_Adv_row <-grep('Samsung',Data$Advertiser)
Samsung_adv <- Data[Samsung_Adv_row,]
#Amazon_Adv_row <-grep('Amazon',Data$Advertiser)
#Amazon_adv <- Data[Amazon_Adv_row,]

##Creating a new column Classification and moving all the own spends
Microsoft_Own_Spends<-cbind(Data[Microsoft_Adv_row,],'Microsoft Own')
colnames(Microsoft_Own_Spends)[30] <- 'Classification'

Apple_Own_Spends<-cbind(Data[Apple_Adv_row,],'Apple Own')
colnames(Apple_Own_Spends)[30] <- 'Classification'

Google_Own_Spends<-cbind(Data[Google_Adv_row,],'Google Own')
colnames(Google_Own_Spends)[30] <- 'Classification'

Samsung_Own_Spends<-cbind(Data[Samsung_Adv_row,],'Samsung Own')
colnames(Samsung_Own_Spends)[30] <- 'Classification'

#Amazon_Own_Spends<-cbind(Data[Amazon_Adv_row,],'Amazon Own')
#colnames(Amazon_Own_Spends)[30] <- 'Classification'

##Combining all the own spends
Own_Spends<-rbind(Microsoft_Own_Spends,Apple_Own_Spends,Google_Own_Spends,Samsung_Own_Spends)

##Own Spends
MAGS_Advertisers<-Data[c(Microsoft_Adv_row,Apple_Adv_row,Google_Adv_row,Samsung_Adv_row),]

#Other Advertisers
Other_Advertisers<-Data[-c(Microsoft_Adv_row,Apple_Adv_row,Google_Adv_row,Samsung_Adv_row),]

#CoPartners and Third Party Classification# 
#CoPartners# 
##Microsoft Copartner
M1 <- grepl('Microsoft',Other_Advertisers$Co.op.Partner.1)
M2 <- grepl('Microsoft',Other_Advertisers$Co.op.Partnet.2)
M3 <- grepl('Microsoft',Other_Advertisers$Co.op.Partner.3)

##Apple Copartner
A1 <- grepl('Apple',Other_Advertisers$Co.op.Partner.1)
A2 <- grepl('Apple',Other_Advertisers$Co.op.Partnet.2)
A3 <- grepl('Apple',Other_Advertisers$Co.op.Partner.3)

##Google Copartner
G1 <- grepl('Google',Other_Advertisers$Co.op.Partner.1)
G2 <- grepl('Google',Other_Advertisers$Co.op.Partnet.2)
G3 <- grepl('Google',Other_Advertisers$Co.op.Partner.3)

##Samsung Copartner
S1 <- grepl('Samsung',Other_Advertisers$Co.op.Partner.1)
S2 <- grepl('Samsung',Other_Advertisers$Co.op.Partnet.2)
S3 <- grepl('Samsung',Other_Advertisers$Co.op.Partner.3)

##Amazon Copartner
#Amz1 <- grepl('Amazon',Other_Advertisers$Co.op.Partner.1)
#Amz2 <- grepl('Amazon',Other_Advertisers$Co.op.Partnet.2)
#Amz3 <- grepl('Amazon',Other_Advertisers$Co.op.Partner.3)

#Classification# 
Classification<-ifelse((((M1 == T|M2 == T|M3 == T)&(A1==F & A2==F & A3== F & G1== F & G2== F & G3 == F & S1 == F & S2 == F & S3 == F ))),"Microsoft Copartner",
                       ifelse((((A1 == T|A2 == T|A3 == T)&(M1==F & M2==F & M3== F & G1== F & G2== F & G3 == F & S1 == F & S2 == F & S3 == F ))),"Apple Copartner",
                              ifelse((((S1 == T|S2 == T|S3 == T)&(M1==F & M2==F & M3== F & G1== F & G2== F & G3 == F & A1 == F & A2 == F & A3 == F ))),"Samsung Copartner",
                                     ifelse((((G1 == T|G2 == T|G3 == T)&(M1==F & M2==F & M3== F & A1== F & A2== F & A3 == F & S1 == F & S2 == F & S3 == F ))),"Google Copartner","Third Party")
                              )))

Copartners_ThirdParty <- cbind(Other_Advertisers,Classification)

##Split CoPartners and ThirdParty for Simplicity Sake
ThirdParty_Spends<-subset(Copartners_ThirdParty,Classification == 'Third Party')
CoPartner_Spends<-subset(Copartners_ThirdParty,Classification != 'Third Party')

###################################################################################
Prefinal_Data<-rbind(Own_Spends,Copartners_ThirdParty)

##PC/Tab/Phone Column creation using the subcategory entries
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Telecom Hardware"]<-'Phone'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Tablets"]<-'Tablet'

Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "PC Desktop/Workstation"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "PC Laptops/Notebooks"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "PC Range (Multi-Formats)"]<-'PC'

Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Operating Systems"]<-'OS'

Prefinal_Data$Business_Categories[Prefinal_Data$Category == "Virtual Reality"]<-'Virtual Reality'

Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Wearable"]<-'Wearables'

Prefinal_Data$Business_Categories[is.na(Prefinal_Data$Business_Categories)]<-"Others"

#####################################################################################################################################
##Bucketing the form factor series for SP and Tablets (Galaxy Series, Lumia Series, IPhone series etc.)
##Create a new column called Product_Series with 'NA' in it
Prefinal_Data$Product_Series<-NA
Surface_Series_Rows<-grep("Surface",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Surface_Series_Rows]<-'Surface Series'

Alienware_Series_Rows<-grep("Alienware",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Alienware_Series_Rows]<-'Dell Alienware Series'

Aspire_Series_Rows<-grep("Aspire",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Aspire_Series_Rows]<-'Acer Aspire Series'

Chromebook_Series_Rows<-grep("Chromebook",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Chromebook_Series_Rows]<-'Google Chromebook Series'

Chromebox_Series_Rows<-grep("Chromebox",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Chromebox_Series_Rows]<-'Google Chromebook Series'

Elite_Series_Rows<-grep("Elite",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Elite_Series_Rows]<-'HP Elite Series'

Pavilion_Series_Rows<-grep("Pavilion",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Pavilion_Series_Rows]<-'HP Pavilion Series'

Spectre_Series_Rows<-grep("Spectre",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Spectre_Series_Rows]<-'HP Spectre Series'

Ideapad_Series_Rows<-grep("Ideapad",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Ideapad_Series_Rows]<-'Lenovo Ideapad Series'

IdeaPad_Series_Rows<-grep("IdeaPad",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[IdeaPad_Series_Rows]<-'Lenovo Ideapad Series'

Inspiron_Series_Rows<-grep("Inspiron",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Inspiron_Series_Rows]<-'Dell Inspiron Series'

Latitude_Series_Rows<-grep("Latitude",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Latitude_Series_Rows]<-'Dell Latitude Series'

MacBook_Series_Rows<-grep("MacBook",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[MacBook_Series_Rows]<-'Apple MacBook Series'

Precision_Series_Rows<-grep("Precision",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Precision_Series_Rows]<-'Dell Precision Series'

Predator_Series_Rows<-grep("Predator",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Predator_Series_Rows]<-'Acer Predator Series'

Thinkpad_Series_Rows<-grep("Thinkpad",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Thinkpad_Series_Rows]<-'Lenovo Thinkpad Series'

ZenBook_Series_Rows<-grep("ZenBook",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[ZenBook_Series_Rows]<-'Asus ZenBook Series'

Zenbook_Series_Rows<-grep("Zenbook",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Zenbook_Series_Rows]<-'Asus ZenBook Series'

XPS_Series_Rows<-grep("XPS",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[XPS_Series_Rows]<-'Dell XPS Series'

Envy_Series_Rows<-grep("Envy",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Envy_Series_Rows]<-'HP Envy Series'


##~Tablet
iPad_Series_Rows<-grep("iPad",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[iPad_Series_Rows]<-'iPad Series'

Galaxy_Series_Rows<-grep("Galaxy",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Galaxy_Series_Rows]<-'Galaxy Series'

Kindle_Series_Rows<-grep("Kindle",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Kindle_Series_Rows]<-'Kindle Series'

Pixel_Series_Rows<-grep("Pixel",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Pixel_Series_Rows]<-'Pixel Series'

Yoga_Series_Rows<-grep("Yoga",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Yoga_Series_Rows]<-'Yoga Series'

##~Phone
iPhone_Series_Rows<-grep("iPhone",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[iPhone_Series_Rows]<-'iPhone Series'

Iphone_Series_Rows<-grep("Iphone",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Iphone_Series_Rows]<-'iPhone Series'

Lumia_Series_Rows<-grep("Lumia",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Lumia_Series_Rows]<-'Lumia Series'

Nexus_Series_Rows<-grep("Nexus",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Nexus_Series_Rows]<-'Nexus Series'

Prefinal_Data$Product_Series[is.na(Prefinal_Data$Product_Series)]<-'Others'

#####################################################################################################################################

Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Microsoft Own']<-'Microsoft'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Microsoft Copartner']<-'Microsoft'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Apple Own']<-'Apple'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Apple Copartner']<-'Apple'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Google Own']<-'Google'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Google Copartner']<-'Google'
#Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Amazon Own']<-'Amazon'
#Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Amazon Copartner']<-'Amazon'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Samsung Own']<-'Samsung'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Samsung Copartner']<-'Samsung'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Third Party']<-'Third Party'

Prefinal_Data$Up_Classification[is.na(Prefinal_Data$Up_Classification)]<-'Others'

#####################################################################################################################################
##Media Week Zero Clean-up on Prefinal Data
##Subset the Media Week Zero entries
FY17_Media_Week_Zero<-subset(Prefinal_Data, Media.Week == 0)
FY17_Data_wo_MWZero<-subset(Prefinal_Data, Media.Week != 0)

##Reclassify all media week zeros into media week 52 along with the dependent variables
FY17_Media_Week_Zero$Quarter<-'Q4'
FY17_Media_Week_Zero$MonthNum<-12
FY17_Media_Week_Zero$Year<-2016
FY17_Media_Week_Zero$Media.Week<-52

##Append the Reclassified media weeks into the Prefinal without media week zeros dataset
Final_Data<-rbind(FY17_Data_wo_MWZero,FY17_Media_Week_Zero)

#####################################################################################################################################

##Adding Date column using the Media Week and Year Column variables with monday as start day of the week
Final_Data$Date_WeekStarting<-as.Date(paste("1", Final_Data$Media.Week, Final_Data$Year, sep = "-"), format = "%w-%W-%Y")
Final_Data$Date_WeekEnding<-as.Date(paste("1", Final_Data$Media.Week, Final_Data$Year, sep = "-"), format = "%w-%W-%Y")+6

#####################################################################################################################################

##Write the Final Data file for further analysis##
setwd('D:/WORK/XBOX & OCC in R/OCC/INTERNATIONAL/UK/OUTPUT')
write.csv(Final_Data, file = paste("OCC_Monthly_UK_Report_Output",Sys.Date(),"csv",sep = '.'))

##End of Code##

##############################################################################























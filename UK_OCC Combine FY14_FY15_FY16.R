###### UK Monthly, OCC REPORTING CODE ######
FY15_Data<-UK_FY15[,c(1:28)]
FY16_Data<-UK_FY16[,c(1:28)]
FY17_Data<-UK_FY17_Jul16.Mar17v2[,c(1:28)]
FY15_Data$FY<-"FY15"
FY16_Data$FY<-"FY16"
FY17_Data$FY<-"FY17"

##Media Week Zero Clean-up on the year data
##Subset the Media Week Zero entries
FY15_Media_Week_Zero<-subset(FY15_Data, Media.Week == 0)
FY15_Data_wo_MWZero<-subset(FY15_Data, Media.Week != 0)

##Reclassify all media week zeros into media week 52 along with the dependent variables
FY15_Media_Week_Zero$Quarter<-'Q4'
FY15_Media_Week_Zero$MonthNum<-12
FY15_Media_Week_Zero$Year<-2014
FY15_Media_Week_Zero$Media.Week<-52

##Append the Reclassified media weeks into the Prefinal without media week zeros dataset
FY15_Data<-rbind(FY15_Data_wo_MWZero,FY15_Media_Week_Zero)

##Media Week Zero Clean-up on the year data
##Subset the Media Week Zero entries
FY16_Media_Week_Zero<-subset(FY16_Data, Media.Week == 0)
FY16_Data_wo_MWZero<-subset(FY16_Data, Media.Week != 0)

##Reclassify all media week zeros into media week 52 along with the dependent variables
FY16_Media_Week_Zero$Quarter<-'Q4'
FY16_Media_Week_Zero$MonthNum<-12
FY16_Media_Week_Zero$Year<-2015
FY16_Media_Week_Zero$Media.Week<-52

##Append the Reclassified media weeks into the Prefinal without media week zeros dataset
FY16_Data<-rbind(FY16_Data_wo_MWZero,FY16_Media_Week_Zero)

##Media Week Zero Clean-up on the year data
##Subset the Media Week Zero entries
FY17_Media_Week_Zero<-subset(FY17_Data, Media.Week == 0)
FY17_Data_wo_MWZero<-subset(FY17_Data, Media.Week != 0)

##Reclassify all media week zeros into media week 52 along with the dependent variables
FY17_Media_Week_Zero$Quarter<-'Q4'
FY17_Media_Week_Zero$MonthNum<-12
FY17_Media_Week_Zero$Year<-2016
FY17_Media_Week_Zero$Media.Week<-52

##Append the Reclassified media weeks into the Prefinal without media week zeros dataset
FY17_Data<-rbind(FY17_Data_wo_MWZero,FY17_Media_Week_Zero)

Combined_Data<- rbind(FY15_Data, FY16_Data, FY17_Data)

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
Amazon_Adv_row <-grep('Amazon',Data$Advertiser)
Amazon_adv <- Data[Amazon_Adv_row,]

##Creating a new column Classification and moving all the own spends
Microsoft_Own_Spends<-cbind(Data[Microsoft_Adv_row,],'Microsoft Own')
colnames(Microsoft_Own_Spends)[30] <- 'Classification'

Apple_Own_Spends<-cbind(Data[Apple_Adv_row,],'Apple Own')
colnames(Apple_Own_Spends)[30] <- 'Classification'

Google_Own_Spends<-cbind(Data[Google_Adv_row,],'Google Own')
colnames(Google_Own_Spends)[30] <- 'Classification'

Samsung_Own_Spends<-cbind(Data[Samsung_Adv_row,],'Samsung Own')
colnames(Samsung_Own_Spends)[30] <- 'Classification'

Amazon_Own_Spends<-cbind(Data[Amazon_Adv_row,],'Amazon Own')
colnames(Amazon_Own_Spends)[30] <- 'Classification'

##Combining all the own spends
Own_Spends<-rbind(Microsoft_Own_Spends,Apple_Own_Spends,Google_Own_Spends,Samsung_Own_Spends,Amazon_Own_Spends)
Own_Spends$OCT<-'Own'

##Own Spends
MAGSA_Advertisers<-Data[c(Microsoft_Adv_row,Apple_Adv_row,Google_Adv_row,Samsung_Adv_row,Amazon_Adv_row),]

#Other Advertisers
Other_Advertisers<-Data[-c(Microsoft_Adv_row,Apple_Adv_row,Google_Adv_row,Samsung_Adv_row,Amazon_Adv_row),]

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
Amz1 <- grepl('Amazon',Other_Advertisers$Co.op.Partner.1)
Amz2 <- grepl('Amazon',Other_Advertisers$Co.op.Partnet.2)
Amz3 <- grepl('Amazon',Other_Advertisers$Co.op.Partner.3)

#Classification# 
Classification<-ifelse(((((M1 == T|M2 == T|M3 == T)&(A1==F & A2==F & A3== F & G1== F & G2== F & G3 == F & S1 == F & S2 == F & S3 == F  & Amz1 == F & Amz2 == F & Amz3 == F )))),"Microsoft Copartner",
                       ifelse(((((A1 == T|A2 == T|A3 == T)&(M1==F & M2==F & M3== F & G1== F & G2== F & G3 == F & S1 == F & S2 == F & S3 == F & Amz1 == F & Amz2 == F & Amz3 == F)))),"Apple Copartner",
                              ifelse(((((S1 == T|S2 == T|S3 == T)&(M1==F & M2==F & M3== F & G1== F & G2== F & G3 == F & A1 == F & A2 == F & A3 == F & Amz1 == F & Amz2 == F & Amz3 == F)))),"Samsung Copartner",
                                     ifelse(((((Amz1 == T|Amz2 == T|Amz3 == T)&(M1==F & M2==F & M3== F & G1== F & G2== F & G3 == F & A1 == F & A2 == F & A3 == F & S1 == F & S2 == F & S3 == F)))),"Amazon Copartner",
                                            ifelse(((((G1 == T|G2 == T|G3 == T)&(M1==F & M2==F & M3== F & A1== F & A2== F & A3 == F & S1 == F & S2 == F & S3 == F& Amz1 == F & Amz2 == F & Amz3 == F )))),"Google Copartner","Third Party")
                                     ))))

Copartners_ThirdParty <- cbind(Other_Advertisers,Classification)

##Split CoPartners and ThirdParty for Simplicity Sake
ThirdParty_Spends<-subset(Copartners_ThirdParty,Classification == 'Third Party')
ThirdParty_Spends$OCT<-'Third Party'

CoPartner_Spends<-subset(Copartners_ThirdParty,Classification != 'Third Party')
CoPartner_Spends$OCT<-'Copartner'

###################################################################################
####Duplicating the two main brand entry spends in ThirdParty
####(i.e. eg. ATT has spend for $30k and has co-partner with Apple & Samsung, it should count as $30k for Apple C0-Partner, and $30k for Samsung Co-partner)
###By CoPartner 1,2,3 separately###

Apple_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Apple')
Apple_Coop1$Classification<-'Apple Copartner'
Samsung_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Samsung')
Samsung_Coop1$Classification<-'Samsung Copartner'
Microsoft_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Microsoft')
Microsoft_Coop1$Classification<-'Microsoft Copartner'
Google_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Google')
Google_Coop1$Classification<-'Google Copartner'
Amazon_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Amazon')
Amazon_Coop1$Classification<-'Amazon Copartner'
Dup_Coop1_Records<-rbind(Apple_Coop1,Samsung_Coop1,Microsoft_Coop1,Google_Coop1,Amazon_Coop1)

Apple_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Apple')
Apple_Coop2$Classification<-'Apple Copartner'
Samsung_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Samsung')
Samsung_Coop2$Classification<-'Samsung Copartner'
Microsoft_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Microsoft')
Microsoft_Coop2$Classification<-'Microsoft Copartner'
Google_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Google')
Google_Coop2$Classification<-'Google Copartner'
Amazon_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Amazon')
Amazon_Coop2$Classification<-'Amazon Copartner'
Dup_Coop2_Records<-rbind(Apple_Coop2,Samsung_Coop2,Microsoft_Coop2,Google_Coop2,Amazon_Coop2)

Apple_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Apple')
Apple_Coop3$Classification<-'Apple Copartner'
Samsung_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Samsung')
Samsung_Coop3$Classification<-'Samsung Copartner'
Microsoft_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Microsoft')
Microsoft_Coop3$Classification<-'Microsoft Copartner'
Google_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Google')
Google_Coop3$Classification<-'Google Copartner'
Amazon_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Amazon')
Amazon_Coop3$Classification<-'Amazon Copartner'
Dup_Coop3_Records<-rbind(Apple_Coop3,Samsung_Coop3,Microsoft_Coop3,Google_Coop3,Amazon_Coop3)

##All duplications classified and combined##
Dup_3levels_Records<-rbind(Dup_Coop1_Records,Dup_Coop2_Records, Dup_Coop3_Records)
Dup_3levels_Records$OCT<-'Copartner'

###Remove all the Dup records without redundancy from the Third Party entries###
Apple_rows1_Remove<-grep('Apple Inc.',ThirdParty_Spends$Co.op.Partner.1)
Microsoft_rows1_Remove<-grep('Microsoft',ThirdParty_Spends$Co.op.Partner.1)
Samsung_rows1_Remove<-grep('Samsung',ThirdParty_Spends$Co.op.Partner.1)
Google_rows1_Remove<-grep('Google',ThirdParty_Spends$Co.op.Partner.1)
Amazon_rows1_Remove<-grep('Amazon',ThirdParty_Spends$Co.op.Partner.1)

Apple_rows2_Remove<-grep('Apple Inc.',ThirdParty_Spends$Co.op.Partnet.2)
Microsoft_rows2_Remove<-grep('Microsoft',ThirdParty_Spends$Co.op.Partnet.2)
Samsung_rows2_Remove<-grep('Samsung',ThirdParty_Spends$Co.op.Partnet.2)
Google_rows2_Remove<-grep('Google',ThirdParty_Spends$Co.op.Partnet.2)
Amazon_rows2_Remove<-grep('Amazon',ThirdParty_Spends$Co.op.Partnet.2)

Apple_rows3_Remove<-grep('Apple Inc.',ThirdParty_Spends$Co.op.Partner.3)
Microsoft_rows3_Remove<-grep('Microsoft',ThirdParty_Spends$Co.op.Partner.3)
Samsung_rows3_Remove<-grep('Samsung',ThirdParty_Spends$Co.op.Partner.3)
Google_rows3_Remove<-grep('Google',ThirdParty_Spends$Co.op.Partner.3)
Amazon_rows3_Remove<-grep('Amazon',ThirdParty_Spends$Co.op.Partner.3)

Coops_To_Remove<-c(Apple_rows1_Remove,Microsoft_rows1_Remove,Samsung_rows1_Remove,Google_rows1_Remove,Apple_rows2_Remove,Microsoft_rows2_Remove,Samsung_rows2_Remove,Google_rows2_Remove,Apple_rows3_Remove,Microsoft_rows3_Remove,Samsung_rows3_Remove,Google_rows3_Remove,Amazon_rows3_Remove,Amazon_rows2_Remove,Amazon_rows1_Remove)

ThirdParty_Pure_Spends<-ThirdParty_Spends[-Coops_To_Remove,]
ThirdParty_Pure_Spends$OCT<-'Third Party'

###################################################################################
Prefinal_Data<-rbind(Own_Spends,CoPartner_Spends, Dup_3levels_Records,ThirdParty_Pure_Spends)

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

###~PC

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
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Amazon Own']<-'Amazon'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Amazon Copartner']<-'Amazon'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Samsung Own']<-'Samsung'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Samsung Copartner']<-'Samsung'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Third Party']<-'Third Party'

Prefinal_Data$Up_Classification[is.na(Prefinal_Data$Up_Classification)]<-'Others'

#####################################################################################################################################

##Adding Date column using the Media Week and Year Column variables with monday as start day of the week
Prefinal_Data$Date_WeekStarting<-as.Date(paste("1", Prefinal_Data$Media.Week, Prefinal_Data$Year, sep = "-"), format = "%w-%W-%Y")
Prefinal_Data$Date_WeekEnding<-as.Date(paste("1", Prefinal_Data$Media.Week, Prefinal_Data$Year, sep = "-"), format = "%w-%W-%Y")+6

#####################################################################################################################################

##Write the Final Data file for further analysis##
setwd('D:/WORK/XBOX & OCC in R/OCC/INTERNATIONAL/UK/OUTPUT')
write.csv(Prefinal_Data, file = paste("Combined_15_16_17_Final_Data_With_Historical",Sys.Date(),"csv",sep = '.'))

##End of Code##
#####################################################################################################################################



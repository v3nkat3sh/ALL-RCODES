#########OCC in R Final Code Historical Data by Parts###################
####Defining / Fetching the Data
###FY16 Data
TVFY16 <- FY16_FT09_SMGFT_YTDDetailTV[,c(1:28)]
RadioFY16<-FY16_FT09_SMGFT_YTDDetailRadio[,c(1:28)]
OnlineVideoFY16 <- FY16_FT09_SMGFT_YTDDetailOnlineVideo[,c(1:28)]
OnlineDisplayFY16 <- FY16_FT09_SMGFT_YTDDetailOnlineDisplay[,c(1:28)]
PrintFY16 <-FY16_FT09_SMGFT_YTDDetailPrint[,c(1:28)]
FY16_Data <- rbind(TVFY16,RadioFY16,OnlineVideoFY16,OnlineDisplayFY16,PrintFY16)
FY16_Data$FY<-"FY16"

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

##########################

###FY15 Data
TVFY15 <- FY15_FT09_SMGFT_YTDDetailTV[,c(1:28)]
RadioFY15<-FY15_FT09_SMGFT_YTDDetailRadio[,c(1:28)]
OnlineVideoFY15 <- FY15_FT09_SMGFT_YTDDetailOnlineVideo[,c(1:28)]
OnlineDisplayFY15 <- FY15_FT09_SMGFT_YTDDetailOnlineDisplay[,c(1:28)]
PrintFY15 <-FY15_FT09_SMGFT_YTDDetailPrint[,c(1:28)]
FY15_Data <- rbind(TVFY15,RadioFY15,OnlineVideoFY15,OnlineDisplayFY15,PrintFY15)
FY15_Data$FY<-"FY15"

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

###FY14 Data
TVFY14 <- FY14_FT09_SMGFT_YTDDetailTV[,c(1:28)]
RadioFY14<-FY14_FT09_SMGFT_YTDDetailRadio[,c(1:28)]
OnlineVideoFY14 <- FY14_FT09_SMGFT_YTDDetailOnlineVideo[,c(1:28)]
OnlineDisplayFY14 <- FY14_FT09_SMGFT_YTDDetailOnlineDisplay[,c(1:28)]
PrintFY14<-FY14_FT09_SMGFT_YTDDetailPrint[,c(1:28)]
FY14_Data <- rbind(TVFY14,RadioFY14,OnlineVideoFY14,OnlineDisplayFY14,PrintFY14)
FY14_Data$FY<-"FY14"

##Media Week Zero Clean-up on the year data
##Subset the Media Week Zero entries
FY14_Media_Week_Zero<-subset(FY14_Data, Media.Week == 0)
FY14_Data_wo_MWZero<-subset(FY14_Data, Media.Week != 0)

##Reclassify all media week zeros into media week 52 along with the dependent variables
FY14_Media_Week_Zero$Quarter<-'Q4'
FY14_Media_Week_Zero$MonthNum<-12
FY14_Media_Week_Zero$Year<-2013
FY14_Media_Week_Zero$Media.Week<-52

##Append the Reclassified media weeks into the Prefinal without media week zeros dataset
FY14_Data<-rbind(FY14_Data_wo_MWZero,FY14_Media_Week_Zero)

############################################################################################################

###Run the code as per requirement, either combined data or data YoY
##Combining data
Combined_Data<-rbind(FY16_Data, FY15_Data, FY14_Data)

##Remove all the B2B entries and retain only Consumer entries
#For Combined data
Consumer_Entries<-subset(Combined_Data,BusVsCons == "Consumer")
#For FY16
Consumer_Entries<-subset(FY16_Data,BusVsCons == "Consumer")
#For FY15
Consumer_Entries<-subset(FY15_Data,BusVsCons == "Consumer")
#For FY14
Consumer_Entries<-subset(FY14_Data,BusVsCons == "Consumer")

##Pulling all the Playstation entries initially itself without any category / subcategory specifics or limits###
##Deleting the Playstation mention records from the larger dataset itself##
Playstation_Rows1<-grep("Playstation",Consumer_Entries$Product.Campaign)
Playstation_Rows2<-grep("PlayStation",Consumer_Entries$Product.Campaign)
Playstation_Records<-Consumer_Entries[c(Playstation_Rows1,Playstation_Rows2),]

###Defining Dataset without Playstation_Records##
Data1<-Consumer_Entries[-c(Playstation_Rows1,Playstation_Rows2),]

###Reclassify all Surface entries at Subcategory level from Tablet PC to Desktops & Notebooks###
Surface_Rows_Reclassification<-grep('Surface', Data1$Product.Campaign)
Data1$Subcategory[Surface_Rows_Reclassification]<-'Desktops & Notebooks'

#SUbsetting only the required categories
Category_InternetRelatedServices<-subset(Data1,Category == "Internet Related Services")
Category_Associations<-subset(Data1,Category == "Associations")
Category_Computers<-subset(Data1,Category == "Computers")
Category_CreditCards<-subset(Data1,Category == "Credit Cards")
Category_GeneralComputer<-subset(Data1,Category == "General Computer")
Category_InternetContentProviders<-subset(Data1,Category == "Internet Content Providers")
Category_Software<-subset(Data1,Category == "Software")
Category_SoftwareOperatingSystems<-subset(Data1,Category == "Software-Operating Systems")
Category_SubscriptionVideoServices<-subset(Data1,Category == "Subscription Video Services")
Category_DepartmentStores<-subset(Data1,Category == "Department Stores")
Category_PeripheralsOther<-subset(Data1,Category == "Peripherals (Other)")
Category_ComputerRelatedServices<-subset(Data1,Category == "Computer Related Services")
Category_ConsumerElectronics<-subset(Data1,Category == "Consumer Electronics")
Category_Image<-subset(Data1,Category == "Image")
Category_WirelessCommunications<-subset(Data1,Category == "Wireless Communications")
Category_Retail<-subset(Data1,Category == "Retail")
Category_Education<-subset(Data1,Category == "Education")

#rbinding all the required categories into one dataframe
Data<-rbind(Category_InternetRelatedServices,Category_Associations,Category_Computers,Category_CreditCards,Category_GeneralComputer,Category_InternetContentProviders,Category_Software,Category_SoftwareOperatingSystems,Category_SubscriptionVideoServices,Category_DepartmentStores,Category_PeripheralsOther,Category_ComputerRelatedServices,Category_ConsumerElectronics,Category_Image,Category_WirelessCommunications,Category_Retail,Category_Education)

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
####Duplicating the two main brand entry spends in ThirdParty
####(i.e. eg. ATT has spend for $30k and has co-partner with Apple & Samsung, it should count as $30k for Apple C0-Partner, and $30k for Samsung Co-partner)
###By CoPartner 1,2,3 separately###

Apple_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Apple Inc.')
Apple_Coop1$Classification<-'Apple Copartner'
Samsung_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Samsung')
Samsung_Coop1$Classification<-'Samsung Copartner'
Microsoft_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Microsoft')
Microsoft_Coop1$Classification<-'Microsoft Copartner'
Google_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Google')
Google_Coop1$Classification<-'Google Copartner'
Dup_Coop1_Records<-rbind(Apple_Coop1,Samsung_Coop1,Microsoft_Coop1,Google_Coop1)

Apple_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Apple Inc.')
Apple_Coop2$Classification<-'Apple Copartner'
Samsung_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Samsung')
Samsung_Coop2$Classification<-'Samsung Copartner'
Microsoft_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Microsoft')
Microsoft_Coop2$Classification<-'Microsoft Copartner'
Google_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Google')
Google_Coop2$Classification<-'Google Copartner'
Dup_Coop2_Records<-rbind(Apple_Coop2,Samsung_Coop2,Microsoft_Coop2,Google_Coop2)

Apple_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Apple Inc.')
Apple_Coop3$Classification<-'Apple Copartner'
Samsung_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Samsung')
Samsung_Coop3$Classification<-'Samsung Copartner'
Microsoft_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Microsoft')
Microsoft_Coop3$Classification<-'Microsoft Copartner'
Google_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Google')
Google_Coop3$Classification<-'Google Copartner'
Dup_Coop3_Records<-rbind(Apple_Coop3,Samsung_Coop3,Microsoft_Coop3,Google_Coop3)

##All duplications classified and combined##
Dup_3levels_Records<-rbind(Dup_Coop1_Records,Dup_Coop2_Records, Dup_Coop3_Records)

###Remove all the Dup records without redundancy from the Third Party entries###
Apple_rows1_Remove<-grep('Apple Inc.',ThirdParty_Spends$Co.op.Partner.1)
Microsoft_rows1_Remove<-grep('Microsoft',ThirdParty_Spends$Co.op.Partner.1)
Samsung_rows1_Remove<-grep('Samsung',ThirdParty_Spends$Co.op.Partner.1)
Google_rows1_Remove<-grep('Google',ThirdParty_Spends$Co.op.Partner.1)

Apple_rows2_Remove<-grep('Apple Inc.',ThirdParty_Spends$Co.op.Partnet.2)
Microsoft_rows2_Remove<-grep('Microsoft',ThirdParty_Spends$Co.op.Partnet.2)
Samsung_rows2_Remove<-grep('Samsung',ThirdParty_Spends$Co.op.Partnet.2)
Google_rows2_Remove<-grep('Google',ThirdParty_Spends$Co.op.Partnet.2)

Apple_rows3_Remove<-grep('Apple Inc.',ThirdParty_Spends$Co.op.Partner.3)
Microsoft_rows3_Remove<-grep('Microsoft',ThirdParty_Spends$Co.op.Partner.3)
Samsung_rows3_Remove<-grep('Samsung',ThirdParty_Spends$Co.op.Partner.3)
Google_rows3_Remove<-grep('Google',ThirdParty_Spends$Co.op.Partner.3)

Coops_To_Remove<-c(Apple_rows1_Remove,Microsoft_rows1_Remove,Samsung_rows1_Remove,Google_rows1_Remove,Apple_rows2_Remove,Microsoft_rows2_Remove,Samsung_rows2_Remove,Google_rows2_Remove,Apple_rows3_Remove,Microsoft_rows3_Remove,Samsung_rows3_Remove,Google_rows3_Remove)

ThirdParty_Pure_Spends<-ThirdParty_Spends[-Coops_To_Remove,]

###################################################################################
Prefinal_Data<-rbind(Own_Spends,CoPartner_Spends, Dup_3levels_Records,ThirdParty_Pure_Spends)

##PC/Tab/Phone Column creation using the subcategory entries
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Smartphone"]<-'Phone'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Tablet PC"]<-'Tablet'

Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktop - Intel/AMD"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Notebook - Intel/AMD"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Ultrabook"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktops & Notebooks"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktops & Notebooks & Servers"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Notebook - Other Non Intel/AMD"]<-'PC'

Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Operating Systems"]<-'OS'

##These below two on category level should be just before wearables
Prefinal_Data$Business_Categories[Prefinal_Data$Category == "Peripherals (Other)"]<-'Peripherals'
Prefinal_Data$Business_Categories[Prefinal_Data$Category == "Consumer Electronics"]<-'Consumer Electronics'
###

Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Wearable Electronics"]<-'Wearables'

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
#Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Amazon Own']<-'Amazon'
#Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Amazon Copartner']<-'Amazon'
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
setwd('D:/WORK/XBOX & OCC in R/OCC/HISTORICAL DATA_FY14,15 &16 DATA/OUTPUTS')
write.csv(Prefinal_Data, file = paste("Combined_FY14_15_16_Final_Data_Historical",Sys.Date(),"csv",sep = '.'))

##End of Code##
#####################################################################################################################################















##############################################################################
####Just do not use this code since the historical data is not inline with this and the exclusivity has been taken care only from the pov of the 3 brands there####
##Cleaning the copartner and Thirdparty entries##
##If copartners are more than 2 then treat it as Third Party##
###Example here is when Copartner 1 is Samsung and Copartner 2 is Walmart
##Changing only the three columns replacing blanks with NA so that we can use is.na function for counting NAs

#CoPartner_Spends$Co.op.Partner.1[CoPartner_Spends$Co.op.Partner.1 == ''] <- NA
#CoPartner_Spends$Co.op.Partnet.2[CoPartner_Spends$Co.op.Partnet.2 == ''] <- NA
#CoPartner_Spends$Co.op.Partner.3[CoPartner_Spends$Co.op.Partner.3 == ''] <- NA

##Count the NA in the three columns and then process
#CoPartner_Spends$NA_Count<-rowSums(is.na(CoPartner_Spends[,26:28]))

##Rewrite the classification using the NAs count for wrong copartners to TP
#CoPartner_Spends$Classification[CoPartner_Spends$NA_Count == 1]<-'Third Party'
#CoPartner_Spends$Classification[CoPartner_Spends$NA_Count == 0]<-'Third Party'

#CoPartner_Spends<-CoPartner_Spends[,1:29]
##Combining the Own, CoPartner & Third Party Spends, Prefinal datafile##
#Prefinal_Data<-rbind(Own_Spends,CoPartner_Spends, ThirdParty_Spends)

#####################################################################################################################################

##########Presently below not needed###########
#####Incase we are in need to use LookUp Tab Only then use the below code
##LookUpTab Classification by Hardware
#DataForLookUpTab<-rbind(Own_Spends, Copartners_ThirdParty)
##Creating a new new column called collated into this data
#DataForLookUpTab$Collated<-paste(DataForLookUpTab$Advertiser,DataForLookUpTab$Category,DataForLookUpTab$Subcategory,DataForLookUpTab$Product.Campaign)

##Convert the DataForLookUpTab$Collated and the LookUpTabforR$Collated into the same class of variables chr/string
#DataForLookUpTab$Collated<-as.character(DataForLookUpTab$Collated)
#LookUpTab.for.R$Collated<-as.character(LookUpTab.for.R$Collated)

##Use the Stri_replace_all_fixed function to remove all the white spaces in the collated variables on both the worksheets above
#library(stringi)
#DataForLookUpTab$Collated<-stri_replace_all_fixed(DataForLookUpTab$Collated, " ", "")
#LookUpTab.for.R$Collated<-stri_replace_all_fixed(LookUpTab.for.R$Collated, " ", "")

##Using join function to use the lookup tab and library plyr
#library(plyr)
#FinalData<-join(DataForLookUpTab,LookUpTab.for.R, by = 'Collated')

###Write into a CSV file
##Choose as per requirement
##For Combined Data
#write.csv(FinalData,file = "CombinedData.csv")
##For FY16 Data
#write.csv(FinalData,file = "FY16_OCC_Classification.csv")
##For FY15 Data
#write.csv(FinalData,file = "FY15_OCC_Classification.csv")
##For FY14 Data
#write.csv(FinalData,file = "FY14_OCC_Classification.csv")

#####################################################################################################
##End of Code##
#####################################################################################################
##Subsets if required by brands at a combined level for all the three years
##By Brand Copartner Classification mentions
#Microsoft_Combined_3years<-subset(FinalData, Classification == c('Microsoft Own','Microsoft Copartner','Third Party'))
#Microsoft_Combined_3years_Final<-Microsoft_Combined_3years[,1:29]
#Apple_Combined_3years<-subset(FinalData, Classification == c('Apple Own','Apple Copartner','Third Party'))
#Apple_Combined_3years_Final<-Apple_Combined_3years[,1:29]
#Google_Combined_3years<-subset(FinalData, Classification == c('Google Own','Google Copartner','Third Party'))
#Google_Combined_3years_Final<-Google_Combined_3years[,1:29]
#Samsung_Combined_3years<-subset(FinalData, Classification == c('Samsung Own','Samsung Copartner','Third Party'))
#Samsung_Combined_3years_Final<-Samsung_Combined_3years[,1:29]
#Amazon_Combined_3years<-subset(FinalData, Classification == c('Amazon Own','Amazon Copartner','Third Party'))
#Amazon_Combined_3years_Final<-Amazon_Combined_3years[,1:29]

#write.csv(Microsoft_Combined_3years_Final,file = "Microsoft Brand Classification Combined.csv")
#write.csv(Apple_Combined_3years_Final,file = "Apple Brand Classification Combined.csv")
#write.csv(Google_Combined_3years_Final,file = "Google Brand Classification Combined.csv")
#write.csv(Samsung_Combined_3years_Final,file = "Samsung Brand Classification Combined.csv")
#write.csv(Amazon_Combined_3years_Final,file = "Amazon Brand Classification Combined.csv")

##By Hardware Classification (An output obtained from the LookUpTab)
#Microsoft_HWsubset<-subset(FinalData, All.Up.Brand == 'Microsoft')
#Microsoft_HWClassification<-Microsoft_HWsubset[,c(1:28,31:37)]
#Apple_HWsubset<-subset(FinalData, All.Up.Brand == 'Apple')
#Apple_HWClassification<-Apple_HWsubset[,c(1:28,31:37)]
#Google_HWsubset<-subset(FinalData, All.Up.Brand == 'Google')
#Google_HWClassification<-Google_HWsubset[,c(1:28,31:37)]
#Samsung_HWsubset<-subset(FinalData, All.Up.Brand == 'Samsung')
#Samsung_HWClassification<-Samsung_HWsubset[,c(1:28,31:37)]
#Amazon_HWsubset<-subset(FinalData, All.Up.Brand == 'Amazon')
#Amazon_HWClassification<-Amazon_HWsubset[,c(1:28,31:37)]

#write.csv(Microsoft_HWClassification,file = "Microsoft Classification by hardware.csv")
#write.csv(Apple_HWClassification,file = "Apple Classification by hardware.csv")
#write.csv(Google_HWClassification,file = "Google Classification by hardware.csv")
#write.csv(Samsung_HWClassification,file = "Samsung Classification by hardware.csv")
#write.csv(Amazon_HWClassification,file = "Amazon Classification by hardware.csv")

#####################################################################################################################################
##Media Week Zero Clean-up on Prefinal Data
##Subset the Media Week Zero entries
#Media_Week_Zero<-subset(Prefinal_Data, Media.Week == 0)
#Prefinal_Data_wo_MWZero<-subset(Prefinal_Data, Media.Week != 0)

##Split the Media Week Zero into 1 & 52
#Media_Week1<-subset(Media_Week_Zero,MonthNum == 1 )
#Media_Week52<-subset(Media_Week_Zero,MonthNum == 12 )

##Reclassify the media week accordingly
#Media_Week1$Media.Week<-1
#Media_Week52$Media.Week<-52

##Append the Reclassified media weeks into the Prefinal without media week zeros dataset
#Final_Data<-rbind(Prefinal_Data_wo_MWZero,Media_Week1,Media_Week52)
#levels(factor(Final_Data$Media.Week))
#####################################################################################################################################

#####################################################################################################
##End of All Codes##
#####################################################################################################
##Rough Useful Codes
#levels(factor(Combined_Data[,"BusVsCons"]))
#Business_Entries<-subset(Combined_Data, BusVsCons == "Business")

#Test_OOH<-subset(Consumer_Entries, Category == "Out Of Home Communications")
#levels(factor(Test_OOH[,"Advertiser"]))

#levels(factor(Category_Data$Category))

##(This code did not work)Remaining_Entries<-subset(Consumer_Entries,Category != c("Associations","Computers","Credit Cards","General Computer","Internet Content Providers","Software","Software-Operating Systems","Subscription Video Services","Department Stores","Peripherals (Other)","Computer Related Services","Consumer Electronics","Image","Internet Related Services","Wireless Communications","Retail","Education"))
##(This code did not work)Subsetting_Categories<-subset(Consumer_Entries,Category == c("Associations","Computers","Credit Cards","General Computer","Internet Content Providers","Software","Software-Operating Systems","Subscription Video Services","Department Stores","Peripherals (Other)","Computer Related Services","Consumer Electronics","Image","Internet Related Services","Wireless Communications","Retail","Education"))
##(This code did not work)Remaining_Data<-Consumer_Entries[!Category_Data] 
###Keep in mind the (5,4)(5,4)rule during ifelse statements (KISS)
##sum(ThirdParty_Spends$AdSpend)
##str(DataForLookUpTab)## Gives a lot of details about the df
##dir.create(paste("Final_Weekly_Datafile", Sys.Date(), sep = "_")) (code to create a folder)


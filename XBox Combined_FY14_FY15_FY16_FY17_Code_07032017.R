###### XBOX COLLATED CODE ######
####Defining / Fetching the Data
###FY17 Data
TVFY17 <- FY17_FT09_SMGFT_YTDDetailTV[,c(1:28,44)]
RadioFY17<-FY17_FT09_SMGFT_YTDDetailRadio[,c(1:28,44)]
OnlineVideoFY17 <- FY17_FT09_SMGFT_YTDDetailOnlineVideo[,c(1:28,45)]
OnlineDisplayFY17 <- FY17_FT09_SMGFT_YTDDetailOnlineDisplay[,c(1:28,45)]
PrintFY17 <-FY17_FT09_SMGFT_YTDDetailPrint[,c(1:28,44)]
MobileFY17<-FY17_FT09_SMGFT_YTDDetailMobile[,c(1:28,45)]
FY17_Data <- rbind(TVFY17,RadioFY17,OnlineVideoFY17,OnlineDisplayFY17,PrintFY17, MobileFY17)
FY17_Data$FY<-"FY17"

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

################################################################################################################
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

###########################################################################################################

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

##Combining data
Combined_Data<-rbind(FY17_Data, FY16_Data, FY15_Data, FY14_Data)

##Remove all the B2B entries and retain only Consumer entries
#For Combined data
Consumer_Entries<-subset(Combined_Data,BusVsCons == "Consumer")

###Creating a new column called classification as the 30th column with NA entries###
Consumer_Entries$Classification<-NA

##Pulling all the Playstation entries initially itself without any category / subcategory specifics or limits###
Playstation_Rows1<-grep("Playstation",Consumer_Entries$Product.Campaign)
Playstation_Rows2<-grep("PlayStation",Consumer_Entries$Product.Campaign)
Playstation_Records<-Consumer_Entries[c(Playstation_Rows1,Playstation_Rows2),]

Playstation_Records$Classification<-'Sony Console Related'

###Defining Dataset without Playstation_Records##
##Provision for any other exclusive entries in future##
Data1<-Consumer_Entries[-c(Playstation_Rows1,Playstation_Rows2),]

###Defining the Gaming/Software Games/Toys & Games subset###

Data2<-subset(Data1,c(Category == 'Gaming' | Category == 'Software-Games'|Category == 'Toys & Games'))

##Pulling all the XBox Live/XBox Live Gold entries from Gaming$OnlineGames separately and classifying as Console spends ###
XBox_Live_Gold_Rows<-grep("Xbox Live Gold",Data2$Product.Campaign)
XBox_Live_Rows<-grep("Xbox Live",Data2$Product.Campaign)
XBox_Live_Records<-Data2[c(XBox_Live_Gold_Rows,XBox_Live_Rows),]

XBox_Live_Records$Classification<-'Microsoft Console Related'

##Pulling all the XBox Live/XBox Live Gold entries from Gaming$OnlineGames separately and classifying as Console spends ###
Handheld_Rows1<-grep("Game Player - Handheld", Data2$Subcategory)
Handheld_Rows2<-grep("Handheld Game Cartridges", Data2$Subcategory)

Handheld_Records<-Data2[c(Handheld_Rows1,Handheld_Rows2),]

##Data without the XboxLive and Handheld records
Data<-Data2[-c(XBox_Live_Gold_Rows,XBox_Live_Rows, Handheld_Rows1,Handheld_Rows2),]

###Check for the factors/levels in the defined new Data subset##
##Remove the '#' before the below code to do the check
#levels(factor(Data$Category))

###Segregating Advertisers into MSN & Others
##MSN (Microsoft/Sony/Nintendo) Advertisers
Microsoft_Adv_row <-grep('Microsoft',Data$Advertiser)
Microsoft_adv <- Data[Microsoft_Adv_row,] 
Sony_Adv_row <-grep('Sony', Data$Advertiser)
Sony_adv <- Data[Sony_Adv_row,]
Nintendo_Adv_row <-grep('Nintendo',Data$Advertiser)
Nintendo_adv <- Data[Nintendo_Adv_row,]

##Other Advertisers
Other_Advertisers <- Data[-c(Microsoft_Adv_row,Sony_Adv_row,Nintendo_Adv_row),]
Other_Advertisers<-Other_Advertisers[,1:30]

##Classifications using MSN Advertisers
###Consoles Related Classification
Microsoft_Console_row <- grep('Video Game Console/Accessories',Microsoft_adv$Subcategory)
Microsoft_adv$Classification[Microsoft_Console_row]<-'Microsoft Console Related'

Nintendo_Console_row <- grep('Video Game Console/Accessories',Nintendo_adv$Subcategory)
Nintendo_adv$Classification[Nintendo_Console_row]<-'Nintendo Console Related'

Sony_Console_row <- grep('Video Game Console/Accessories',Sony_adv$Subcategory)
Sony_adv$Classification[Sony_Console_row]<-'Sony Console Related'

###Games Classification
###MSN Advertisers are the classified own spends for all the three brands now
Microsoft_adv$Classification[Microsoft_adv$Subcategory !='Video Game Console/Accessories']<-"Microsoft Own Games"
Sony_adv$Classification[Sony_adv$Subcategory !='Video Game Console/Accessories']<-"Sony Own Games"
Nintendo_adv$Classification[Nintendo_adv$Subcategory !='Video Game Console/Accessories']<-"Nintendo Own Games"
MSN_Advertisers<-rbind(Microsoft_adv,Nintendo_adv,Sony_adv)

##CoPartner and Third Party Classification
##Microsoft Copartner
M1 <- grepl('Microsoft',Other_Advertisers$Co.op.Partner.1)
M2 <- grepl('Microsoft',Other_Advertisers$Co.op.Partnet.2)
M3 <- grepl('Microsoft',Other_Advertisers$Co.op.Partner.3)

##Sony Copartner
S1 <- grepl('Sony',Other_Advertisers$Co.op.Partner.1)
S2 <- grepl('Sony',Other_Advertisers$Co.op.Partnet.2)
S3 <- grepl('Sony',Other_Advertisers$Co.op.Partner.3)

##Nintendo Copartner
N1 <- grepl('Nintendo',Other_Advertisers$Co.op.Partner.1)
N2 <- grepl('Nintendo',Other_Advertisers$Co.op.Partnet.2)
N3 <- grepl('Nintendo',Other_Advertisers$Co.op.Partner.3)

B1 <- is.na(Other_Advertisers$Co.op.Partner.1)
B2 <- is.na(Other_Advertisers$Co.op.Partnet.2)
B3 <- is.na(Other_Advertisers$Co.op.Partner.3)

##Copartner and Third Party Classifications
### using nested if-else

Classification <- ifelse(((M1 == T|M2 == T|M3 == T)&(S1==F & S2==F & S3== F & N1== F & N2== F & N3 == F)),"Microsoft Games Copartner",
                         ifelse(((S1 == T|S2 == T|S3 == T)&(M1== F & M2== F & M3==F & N1== F & N2== F & N3 == F)),"Sony Games Copartner",
                                ifelse(((N1 == T|N2 == T|N3 == T)&(M1== F & M2== F & M3==F & S1== F & S2== F & S3 == F)),"Nintendo Games Copartner","Third Party / Others")
                         )
)

Copartners_TP <- cbind(Other_Advertisers,Classification)

##Combining all the classified data and the PlayStation Records##
Prefinal_Data<-rbind(MSN_Advertisers, Copartners_TP, Playstation_Records, XBox_Live_Records)

##Broader Classification bucketing by brand##
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Microsoft Console Related']<-'Microsoft'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Microsoft Own Games']<-'Microsoft'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Microsoft Games Copartner']<-'Microsoft'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Sony Console Related']<-'Sony'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Sony Own Games']<-'Sony'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Sony Games Copartner']<-'Sony'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Nintendo Console Related']<-'Nintendo'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Nintendo Own Games']<-'Nintendo'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Nintendo Games Copartner']<-'Nintendo'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Third Party / Others']<-'Third Party / Others'

##Broader Classification bucketing by OCT##
Prefinal_Data$OCT[Prefinal_Data$Classification == 'Microsoft Console Related']<-'Own'
Prefinal_Data$OCT[Prefinal_Data$Classification == 'Microsoft Own Games']<-'Own'
Prefinal_Data$OCT[Prefinal_Data$Classification == 'Microsoft Games Copartner']<-'Copartner'
Prefinal_Data$OCT[Prefinal_Data$Classification == 'Sony Console Related']<-'Own'
Prefinal_Data$OCT[Prefinal_Data$Classification == 'Sony Own Games']<-'Own'
Prefinal_Data$OCT[Prefinal_Data$Classification == 'Sony Games Copartner']<-'Copartner'
Prefinal_Data$OCT[Prefinal_Data$Classification == 'Nintendo Console Related']<-'Own'
Prefinal_Data$OCT[Prefinal_Data$Classification == 'Nintendo Own Games']<-'Own'
Prefinal_Data$OCT[Prefinal_Data$Classification == 'Nintendo Games Copartner']<-'Copartner'
Prefinal_Data$OCT[Prefinal_Data$Classification == 'Third Party / Others']<-'Third Party / Others'

##Broader Classification bucketing by GC##
Prefinal_Data$Business_Categories[Prefinal_Data$Classification == 'Microsoft Console Related']<-'Console'
Prefinal_Data$Business_Categories[Prefinal_Data$Classification == 'Microsoft Own Games']<-'Games'
Prefinal_Data$Business_Categories[Prefinal_Data$Classification == 'Microsoft Games Copartner']<-'Games'
Prefinal_Data$Business_Categories[Prefinal_Data$Classification == 'Sony Console Related']<-'Console'
Prefinal_Data$Business_Categories[Prefinal_Data$Classification == 'Sony Own Games']<-'Games'
Prefinal_Data$Business_Categories[Prefinal_Data$Classification == 'Sony Games Copartner']<-'Games'
Prefinal_Data$Business_Categories[Prefinal_Data$Classification == 'Nintendo Console Related']<-'Console'
Prefinal_Data$Business_Categories[Prefinal_Data$Classification == 'Nintendo Own Games']<-'Games'
Prefinal_Data$Business_Categories[Prefinal_Data$Classification == 'Nintendo Games Copartner']<-'Games'
Prefinal_Data$Business_Categories[Prefinal_Data$Classification == 'Third Party / Others']<-'Games'

#####################################################################################################################################
##Adding the TacoBell and Pizza Hut Data (Already processed datafile)
Final_Data<-rbind(Prefinal_Data, PizzaHut_and_TacoBell_1Jun14_thru_19Jan2017)

#####################################################################################################################################

###Extra Column for Path Specification (Later can be integrated with OCC Path)
Final_Data$Path<-'XBox'

##Write the interim file without adding in the Taco / Doritos data (Msft Console Copartner)
setwd('D:/CAMPAIGN/XBOX & OCC in R/XBOX/HISTORICAL DATA_FY14,15 &16 DATA/OUTPUTS')
write.csv(Final_Data, file = paste("XBOX_Combined_Report_Output_FY14_15_16_17",Sys.Date(),"csv",sep = '.'))



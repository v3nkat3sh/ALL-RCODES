###### XBOX WEEKLY REPORTING CODE ######
####Datafile Uploading###
TV <- FT09_SMGFT_YTDDetailTV[,c(1:28)]
Radio<-FT09_SMGFT_YTDDetailRadio[,c(1:28)]
OnlineVideo <- FT09_SMGFT_YTDDetailOnlineVideo[,c(1:28)]
OnlineDisplay <- FT09_SMGFT_YTDDetailOnlineDisplay[,c(1:28)]
Print <-FT09_SMGFT_YTDDetailPrint[,c(1:28)]
Mobile<-FT09_SMGFT_YTDDetailMobile[,c(1:28)]
Combined_Data <- rbind(TV,Radio,OnlineVideo,OnlineDisplay,Print,Mobile)

##Remove all the Business entries and retain only Consumer entries
#For Combined data
Consumer_Entries<-subset(Combined_Data,BusVsCons == "Consumer")

###Creating a new column called classification as the 29th column with NA entries###
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
Other_Advertisers<-Other_Advertisers[,1:28]

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
                                ifelse(((N1 == T|N2 == T|N3 == T)&(M1== F & M2== F & M3==F & S1== F & S2== F & S3 == F)),"Nintendo Games Copartner","Third Party Games")
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
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Third Party Games']<-'Third Party Games'

#####################################################################################################################################
##Media Week Zero Clean-up on Prefinal Data
##Subset the Media Week Zero entries
Media_Week_Zero<-subset(Prefinal_Data, Media.Week == 0)
Prefinal_Data_wo_MWZero<-subset(Prefinal_Data, Media.Week != 0)

##Split the Media Week Zero into 1 & 52
Media_Week1<-subset(Media_Week_Zero,MonthNum == 1 )
Media_Week52<-subset(Media_Week_Zero,MonthNum == 12 )

##Reclassify the media week accordingly
Media_Week1$Media.Week<-1
Media_Week52$Media.Week<-52

##Append the Reclassified media weeks into the Prefinal without media week zeros dataset
Final_Data<-rbind(Prefinal_Data_wo_MWZero,Media_Week1,Media_Week52)

Final_Data$Week_Starting_Date<-as.Date(paste("1", Final_Data$Media.Week, Final_Data$Year, sep = "-"), format = "%w-%W-%Y")
Final_Data$Week_Ending_Date<-as.Date(paste("1", Final_Data$Media.Week, Final_Data$Year, sep = "-"), format = "%w-%W-%Y")+6

#####################################################################################################################################
##Write the interim file without adding in the Taco / Doritos data (Msft Console Copartner)
setwd('D:/WORK/XBOX & OCC in R/XBOX/WEEKLY FY17/OUTPUTS')
write.csv(Final_Data, file = paste("XBOX_Weekly_Report_Output",Sys.Date(),"csv",sep = '.'))

#####################################################################################################################################
###Code for the Taco/Doritos separate datafile and classification and then rbind with the Prefinal data





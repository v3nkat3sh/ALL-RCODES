##UK Xbox Monthly##
####Datafile Uploading###
JulytoJune <- UK_FY16[,c(1:28)]
JulytoSeptember<-Microsoft_International_UK_Report_September_2016[,c(1:28)]
October2016_Data<-Microsoft_International_._UK_Report_October_2016[,c(1:28)]

##Change the AdSpend column to factor from present num to match the JulytoJune file format else NA will get generated
JulytoSeptember <- transform(JulytoSeptember, AdSpend = as.factor(AdSpend))
October2016_Data <- transform(October2016_Data, AdSpend = as.factor(AdSpend))
Combined_Data <- rbind(JulytoJune,JulytoSeptember,October2016_Data)

##Remove all the Business entries and retain only Consumer entries
#For Combined data
#Consumer_Entries<-subset(Combined_Data,BusVsCons == "Consumer")

###Creating a new column called classification as the 29th column with NA entries###
Combined_Data$Classification<-NA

##Pulling all the Playstation entries initially itself without any category / subcategory specifics or limits###
Playstation_Rows1<-grep("Playstation",Combined_Data$Product.Campaign)
Playstation_Rows2<-grep("PlayStation",Combined_Data$Product.Campaign)
Playstation_Rows<-rbind(Playstation_Rows1,Playstation_Rows2)
Playstation_Records<-Combined_Data[c(Playstation_Rows1,Playstation_Rows2),]

Playstation_Records$Classification<-'Sony Console Related'

###Defining Dataset without Playstation_Records##
##Provision for any other exclusive entries in future##
Data1<-Combined_Data[-c(Playstation_Rows1,Playstation_Rows2),]

###Defining the Computer Games/Online Gaming subset (here it is at a subcategory level)###

Data2<-subset(Data1,Category == 'LEISURE AND ENTERTAINMENT')

##Pulling all the XBox Live/XBox Live Gold entries from Gaming$OnlineGames separately and classifying as Console spends ###
#XBox_Live_Gold_Rows<-grep("Xbox Live Gold",Data2$Product.Campaign)
#XBox_Live_Rows<-grep("Xbox Live",Data2$Product.Campaign)
#XBox_Live_Records<-Data2[c(XBox_Live_Gold_Rows,XBox_Live_Rows),]

#XBox_Live_Records$Classification<-'Microsoft Console Related'

##Data without the XboxLive records
Data<-Data2
#Data<-Data2[-c(XBox_Live_Gold_Rows,XBox_Live_Rows),]

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
MSN_Advertisers<-rbind(Microsoft_adv,Sony_adv, Nintendo_adv)

##Other Advertisers
Other_Advertisers <- Data[-c(Microsoft_Adv_row,Sony_Adv_row,Nintendo_Adv_row),]
Other_Advertisers<-Other_Advertisers[,1:28]

##Classifications using MSN Advertisers
###Consoles Related Classification
Microsoft_Console_row <- grep('Xbox',Microsoft_adv$Product.Campaign)
Microsoft_Consoles<-Microsoft_adv[Microsoft_Console_row,]
Microsoft_Consoles$Classification<-'Microsoft Console Related'

Nintendo_Console_row <- grep('Wii',Nintendo_adv$Product.Campaign)
Nintendo_Consoles<-Nintendo_adv[Nintendo_Console_row,]
Nintendo_Consoles$Classification<-'Nintendo Console Related'

Consoles<-rbind(Playstation_Records,Microsoft_Consoles,Nintendo_Consoles)
GamesRecords<-MSN_Advertisers[-c(Playstation_Rows1,Playstation_Rows2,Microsoft_Console_row,Nintendo_Console_row),]

##Already classified at Playstation level##
#Sony_Console_row <- grep('Video Game Console/Accessories',Sony_adv$Subcategory)
#Sony_adv$Classification[Sony_Console_row]<-'Sony Console Related'

###Games Classification
###MSN Advertisers are the classified own spends for all the three brands now
MicrosoftGames<-grep('Microsoft',GamesRecords$Advertiser)
GamesRecords$Classification[MicrosoftGames]<-"Microsoft Own Games"

SonyGames<-grep('Sony',GamesRecords$Advertiser)
GamesRecords$Classification[SonyGames]<-"Sony Own Games"

NintendoGames<-grep('Nintendo',GamesRecords$Advertiser)
GamesRecords$Classification[NintendoGames]<-"Nintendo Own Games"

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

##Combining all the classified data ##
Prefinal_Data<-rbind(Consoles, Copartners_TP, GamesRecords)

Prefinal_Data$Starting_Week_Date<-as.Date(paste("1", Prefinal_Data$Media.Week, Prefinal_Data$Year, sep = "-"), format = "%w-%W-%Y")
Prefinal_Data$Ending_Week_Date<-as.Date(paste("1", Prefinal_Data$Media.Week, Prefinal_Data$Year, sep = "-"), format = "%w-%W-%Y")+6
#####################################################################################################################################

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
##Write the interim file without adding in the Taco / Doritos data (Msft Console Copartner)
setwd('D:/WORK/XBOX & OCC in R/XBOX/UK MONTHLY/OUTPUTS')
write.csv(Prefinal_Data, file = paste("XBOX_UK_Monthly_Report_Output",Sys.Date(),"csv",sep = '.'))
#####################################################################################################################################






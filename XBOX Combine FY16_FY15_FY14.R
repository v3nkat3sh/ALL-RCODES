####Defining / Fetching the Data
###FY14 Data
TVFY14 <- FT09_SMGFT_YTDDetailTV.FY14[,c(1:18,26:28)]
OthersFY14 <- US.FY14.XBOX[,c(1:18,26:28)]
RadioFY14 <- FT09_SMGFT_YTDDetailRadio_FY14[,c(1:18,26:28)]
FY14_Data <- rbind(TVFY14,OthersFY14,RadioFY14)

##FY15 Data
FY15_Data <- US.FY15.Raw.venky.final[,c(1:10,12:19,27:29)]

##FY16 Data
FY16_Data <- US.FY16.Raw.Data.Sheet[,c(1:10,12:19,27:29)]

##Rbind'ing the three years data as Data

Data<-rbind(FY14_Data,FY15_Data,FY16_Data)

##Xbox CLassifications 
###Segregating Advertisers into MSN & Others
##MSN (Microsoft/Sony/Nintendo) Advertisers
Microsoft_Adv_row <-grep('Microsoft',Data$Advertiser)
Microsoft_adv <- Data[Microsoft_Adv_row,] 
Sony_Adv_row <-grep('Sony', Data$Advertiser)
Sony_adv <- Data[Sony_Adv_row,]
Nintendo_Adv_row <-grep('Nintendo',Data$Advertiser)
Nintendo_adv <- Data[Nintendo_Adv_row,]
MSN_Advertisers <- Data[c(Microsoft_Adv_row,Sony_Adv_row,Nintendo_Adv_row),]

##Other Advertisers
Other_Advertisers <- Data[-c(Microsoft_Adv_row,Sony_Adv_row,Nintendo_Adv_row),]

##Classifications using MSN Advertisers
###Microsoft Console
Microsoft_Console_row <- grep('Video Game Console/Accessories',Microsoft_adv$Subcategory)
Microsoft_Console <- cbind(Microsoft_adv[Microsoft_Console_row,],'Microsoft Console')
colnames(Microsoft_Console)[22] <- 'Classification' 

###Microsoft Handheld
Microsoft_Handheld_row <-grep('Handheld',Microsoft_adv$Subcategory)
Microsoft_Handheld <- cbind(Microsoft_adv[Microsoft_Handheld_row,],'Microsoft Handheld')
colnames(Microsoft_Handheld)[22] <- 'Classification' 

###Microsoft Games
Microsoft_Games_Category_row1 <-grep('Gaming',Microsoft_adv$Category)
Microsoft_Games_Category_row2<-grep('Software-Games',Microsoft_adv$Category)
Microsoft_Games_Category_row3<-grep('Toys & Games',Microsoft_adv$Category)

Microsoft_Games_Category <- Microsoft_adv[c(Microsoft_Games_Category_row1,Microsoft_Games_Category_row2,Microsoft_Games_Category_row3),]
Microsoft_Games_row <- grep('Video Game Console/Accessories',Microsoft_Games_Category$Subcategory)
Microsoft_Games <- cbind(Microsoft_Games_Category[-(Microsoft_Games_row),],'Microsoft Games')
colnames(Microsoft_Games)[22] <- 'Classification' 

###Sony Console
Sony_Console_row <- grep('Video Game Console/Accessories',Sony_adv$Subcategory)
Sony_Console <- cbind(Sony_adv[Sony_Console_row,],'Sony Console')
colnames(Sony_Console)[22] <- 'Classification' 

###Sony Handheld
Sony_Handheld_row <-grep('Handheld',Sony_adv$Subcategory)
Sony_Handheld <- cbind(Sony_adv[Sony_Handheld_row,],'Sony Handheld')
colnames(Sony_Handheld)[22] <- 'Classification' 

###Sony Games
Sony_Games_Category_row1 <-grep('Gaming',Sony_adv$Category)
Sony_Games_Category_row2<-grep('Software-Games',Sony_adv$Category)
Sony_Games_Category_row3<-grep('Toys & Games',Sony_adv$Category)

Sony_Games_Category <- Sony_adv[c(Sony_Games_Category_row1,Sony_Games_Category_row2,Sony_Games_Category_row3),]
Sony_Games_row <- grep('Video Game Console/Accessories',Sony_Games_Category$Subcategory)
Sony_Games <- cbind(Sony_Games_Category[-(Sony_Games_row),],'Sony Games')
colnames(Sony_Games)[22] <- 'Classification'

###Nintendo Console
Nintendo_Console_row <- grep('Video Game Console/Accessories',Nintendo_adv$Subcategory)
Nintendo_Console <- cbind(Nintendo_adv[Nintendo_Console_row,],'Nintendo Console')
colnames(Nintendo_Console)[22] <- 'Classification' 

###Nintendo Handheld
Nintendo_Handheld_row <-grep('Handheld',Nintendo_adv$Subcategory)
Nintendo_Handheld <- cbind(Nintendo_adv[Nintendo_Handheld_row],'Nintendo Handheld')
colnames(Nintendo_Handheld)[22] <- 'Classification' 

###Nintendo Games
Nintendo_Games_Category_row1 <-grep('Gaming',Nintendo_adv$Category)
Nintendo_Games_Category_row2<-grep('Software-Games',Nintendo_adv$Category)
Nintendo_Games_Category_row3<-grep('Toys & Games',Nintendo_adv$Category)

Nintendo_Games_Category <- Nintendo_adv[c(Nintendo_Games_Category_row1,Nintendo_Games_Category_row2,Nintendo_Games_Category_row3),]
Nintendo_Games_row <- grep('Video Game Console/Accessories',Nintendo_Games_Category$Subcategory)
Nintendo_Games <- cbind(Nintendo_Games_Category[-(Nintendo_Games_row),],'Nintendo Games')
colnames(Nintendo_Games)[22] <- 'Classification'

##CoPartner and Third Party Classification
Co_cat_games <-grep('Gaming',Other_Advertisers$Category)
Co_Cat_SG<-grep('Software-Games',Other_Advertisers$Category)
Co_Cat_TG<-grep('Toys & Games',Other_Advertisers$Category)

Copartner_category <- Other_Advertisers[c(Co_cat_games,Co_Cat_SG,Co_Cat_TG),]

##Microsoft Copartner
M1 <- grepl('Microsoft',Copartner_category$Co.op.Partner.1)
M2 <- grepl('Microsoft',Copartner_category$Co.op.Partnet.2)
M3 <- grepl('Microsoft',Copartner_category$Co.op.Partner.3)

##Sony Copartner
S1 <- grepl('Sony',Copartner_category$Co.op.Partner.1)
S2 <- grepl('Sony',Copartner_category$Co.op.Partnet.2)
S3 <- grepl('Sony',Copartner_category$Co.op.Partner.3)

##Nintendo Copartner
N1 <- grepl('Nintendo',Copartner_category$Co.op.Partner.1)
N2 <- grepl('Nintendo',Copartner_category$Co.op.Partnet.2)
N3 <- grepl('Nintendo',Copartner_category$Co.op.Partner.3)

B1 <- is.na(Copartner_category$Co.op.Partner.1)
B2 <- is.na(Copartner_category$Co.op.Partnet.2)
B3 <- is.na(Copartner_category$Co.op.Partner.3)

##Copartner and Third Party Classifications
### using nested if-else

Classification <- ifelse(((M1 == T|M2 == T|M3 == T)&(S1==F & S2==F & S3== F & N1== F & N2== F & N3 == F)),"Microsoft Copartner",
                         ifelse(((S1 == T|S2 == T|S3 == T)&(M1== F & M2== F & M3==F & N1== F & N2== F & N3 == F)),"Sony Copartner",
                                ifelse(((N1 == T|N2 == T|N3 == T)&(M1== F & M2== F & M3==F & S1== F & S2== F & S3 == F)),"Nitendo Copartner","Third Party")
                         )
)

Copartners_TP <- cbind(Copartner_category,Classification)

Final_Data <- rbind(Microsoft_Games,Microsoft_Console,Sony_Games,Sony_Handheld,Sony_Console,
                    Nintendo_Games,Nintendo_Console,Copartners_TP)
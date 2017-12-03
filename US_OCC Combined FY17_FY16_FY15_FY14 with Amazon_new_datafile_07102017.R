###### US_OCC Combined FY17_FY16_FY15_FY14 with Amazon_new_datafile_07102017 ######

###LOAD THE FULL DATASET FUll_FY14151617####
##Remove all the Business entries and retain only Consumer entries

Consumer_Entries<-subset(FUll_FY14151617,BusVsCons == "Consumer")
Consumer_Entries<-Consumer_Entries[,2:31]

##Pulling all the Playstation entries initially itself without any category / subcategory specifics or limits###
##Deleting the Playstation mention records from the larger dataset itself##
Playstation_Rows1<-grep("Playstation",Consumer_Entries$Product.Campaign)
Playstation_Rows2<-grep("PlayStation",Consumer_Entries$Product.Campaign)
Playstation_Records<-Consumer_Entries[c(Playstation_Rows1,Playstation_Rows2),]

###Defining Dataset without Playstation_Records##
Data1<-Consumer_Entries[-c(Playstation_Rows1,Playstation_Rows2),]

###Reclassify all Surface entries at Subcategory level from Tablet PC to Desktops & Notebooks###
Surface_Rows_Reclassification<-grep('Surface', Data1$Product.Campaign)
Data1$Subcategory[Surface_Rows_Reclassification]<-'Desktops & Laptops'
------------------------------------------------------------------------------------------------------------------
###Reclassify the selected records entries of Amazon Records which are all under Retail Category ###
Amazon_records<-subset(Data1,Advertiser == "Amazon")
Data1_minus_Amazon_records<-subset(Data1, Advertiser !="Amazon")

Amazon_Echo_Reclassification<-grep('Echo',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Echo_Reclassification]<-'Consumer Electronics'
Amazon_records$Subcategory[Amazon_Echo_Reclassification]<-'Other Consumer Electronics'

Amazon_Alexa_Reclassification<-grep('Amazon Alexa',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Alexa_Reclassification]<-'Software'
Amazon_records$Subcategory[Amazon_Alexa_Reclassification]<-'Cloud/Virtualization Services'

Amazon_Coin_Reclassification<-grep('Amazon Coin',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Coin_Reclassification]<-'Credit Cards'
Amazon_records$Subcategory[Amazon_Coin_Reclassification]<-'Digital Payment Systems'

Amazon_Appstore_Reclassification<-grep('Amazon Appstore',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Appstore_Reclassification]<-'Software'
Amazon_records$Subcategory[Amazon_Coin_Reclassification]<-'Multiple Software'

Amazon_Dash_Reclassification<-grep('Amazon Dash',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Dash_Reclassification]<-'Consumer Electronics'
Amazon_records$Subcategory[Amazon_Dash_Reclassification]<-'Other Consumer Electronics'

Amazon_dynamodb_Reclassification<-grep('Amazon DynamoDB',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_dynamodb_Reclassification]<-'Software'
Amazon_records$Subcategory[Amazon_dynamodb_Reclassification]<-'Cloud/Virtualization Services'

Amazon_Fire_Tablet_Reclassification<-grep('Amazon Fire Tablet',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Fire_Tablet_Reclassification]<-'Computers'
Amazon_records$Subcategory[Amazon_Fire_Tablet_Reclassification]<-'Tablet PC'

Amazon_Music_Reclassification<-grep('Amazon Music Unlimited',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Music_Reclassification]<-'Internet Content Providers'
Amazon_records$Subcategory[Amazon_Music_Reclassification]<-'Music Download'

Amazon_Payments_Reclassification<-grep('Amazon Payments',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Payments_Reclassification]<-'Credit Cards'
Amazon_records$Subcategory[Amazon_Payments_Reclassification]<-'Digital Payment Systems'

Amazon_Cellphones_Reclassification<-grep('Cell Phones',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Cellphones_Reclassification]<-'Wireless Communications'
Amazon_records$Subcategory[Amazon_Cellphones_Reclassification]<-'Smartphone'

Amazon_TV_Reclassification<-grep('TV',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_TV_Reclassification]<-'Consumer Electronics'
Amazon_records$Subcategory[Amazon_TV_Reclassification]<-'Multiple TV Types'

##levels(factor(Amazon_records$Subcategory))

##Bringing back the Amazon Records and Data1_minus_Amazon_Records back to Data1##
Data1<-rbind(Amazon_records, Data1_minus_Amazon_records)

--------------------------------------------------------------------------------------------------------------
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
##MAGSA (Microsoft/Apple/Google/Samsung & Amazon) Advertisers
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
colnames(Microsoft_Own_Spends)[31] <- 'Classification'

Apple_Own_Spends<-cbind(Data[Apple_Adv_row,],'Apple Own')
colnames(Apple_Own_Spends)[31] <- 'Classification'

Google_Own_Spends<-cbind(Data[Google_Adv_row,],'Google Own')
colnames(Google_Own_Spends)[31] <- 'Classification'

Samsung_Own_Spends<-cbind(Data[Samsung_Adv_row,],'Samsung Own')
colnames(Samsung_Own_Spends)[31] <- 'Classification'

Amazon_Own_Spends<-cbind(Data[Amazon_Adv_row,],'Amazon Own')
colnames(Amazon_Own_Spends)[31] <- 'Classification'

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
CoPartner_Spends<-subset(Copartners_ThirdParty,Classification != 'Third Party')
CoPartner_Spends$OCT<-'Copartner'

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
Amazon_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Amazon')
Amazon_Coop1$Classification<-'Amazon Copartner'
Dup_Coop1_Records<-rbind(Apple_Coop1,Samsung_Coop1,Microsoft_Coop1,Google_Coop1,Amazon_Coop1)

Apple_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Apple Inc.')
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

Apple_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Apple Inc.')
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
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Smartphone"]<-'Phone'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Tablet PC"]<-'Tablet'

Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktop - Intel/AMD"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Notebook - Intel/AMD"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Ultrabook"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktops & Notebooks"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktops & Notebooks & Servers"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Notebook - Other Non Intel/AMD"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktop - Macintosh"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktops & Laptops"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktop - Other (Non Intel/AMD)"]<-'PC'

Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Operating Systems"]<-'OS'

##These below two on category level should be just before wearables
Prefinal_Data$Business_Categories[Prefinal_Data$Category == "Peripherals (Other)"]<-'Peripherals / Accessories'
Prefinal_Data$Business_Categories[Prefinal_Data$Category == "Consumer Electronics"]<-'Consumer Electronics'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Electronic & Appliance Stores"]<-'Consumer Electronics'
###

Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Wearable Electronics"]<-'Wearables'

Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Internet Broadcast/Streaming Media"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Education/Training Software"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Search Engine/Portal"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "On-line Services"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "E-Mail Services"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Online Shopping"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Music Download"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Automobile Content Sites"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Mobile Apps"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Software Suite"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Document Management"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Other Software"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Multiple Software"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Cloud/Virtualization Services"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Other Services"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Digital Payment Systems"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "IPTV Service"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Computer Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "High Speed Internet"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Social Networking Sites"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Online Entertainment"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Ringtones and Other Downloads"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Digital Media Downloads"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Music & Video Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Sporting Good Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Supermarkets"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Toy Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Bookstores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Shoe Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Specialty Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Clothing Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Home Furnishings"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Pet Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Bedding & Bath Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Auto Parts Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Office Supply Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Department Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Wireless Phone Accessories"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Internet Related Services Image"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Food & Beverage"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Appliances"]<-'Consumer Apps & Services'

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

Fire_Series_Rows<-grep("Fire",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Fire_Series_Rows]<-'Fire Series'

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
setwd('D:/CAMPAIGN/XBOX & OCC in R/OCC/HISTORICAL DATA_FY14,15 &16 DATA/OUTPUTS/FY14151617')
write.csv(Prefinal_Data, file = paste("Final_Processed_FY14151617_Output",Sys.Date(),"csv",sep = '.'))

##End of Code##
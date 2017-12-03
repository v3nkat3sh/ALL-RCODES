write.csv(UK.FY17.Raw.Data, 'old.csv')
write.csv(Combined_Data,'full.data.csv')
Combined_Data<-UK_FY17_Jul16.Mar17v2[,c(1:28)]

FY17_Data<-FY17_Till_March[,c(1:28)]
levels(factor(Data$Co.op.Partner.1))


EDU<-subset(OCC_Weekly_Report_Output.2017.05.18,Category == "Education")
factor(levels = EDU$Advertiser)

write.csv(EDU, 'EDU.csv')
amaa<-subset(Australia_.FY17_Jul16.Apr17, Advertiser == 'Amazon')

Combined_Data<-Australia_.FY17_Jul16.Apr17[,c(1:28)]
Data1<-Data1[,c(1:28)]

Apple_File<-subset(His_Lat, Up_Classification == 'Apple')
Microsoft_File<-subset(His_Lat, Up_Classification == 'Microsoft')
Amazon_File<-subset(His_Lat, Up_Classification == 'Amazon')
Google_File<-subset(His_Lat, Up_Classification == 'Google')
Samsung_File<-subset(His_Lat, Up_Classification == 'Samsung')

All_Compete<-rbind(Apple_File,Amazon_File,Google_File,Samsung_File)

write.csv(Apple_File,'Apple_File.csv')
write.csv(Google_File,'Google_File.csv')
write.csv(Amazon_File,'Amazon_File.csv')
write.csv(Samsung_File,'Samsung_File.csv')
write.csv(Microsoft_File,'Microsoft_File.csv')
write.csv(All_Compete,'All_Compete.csv')

rm(OCC_Weekly_Report_Output.2017.05.18$In.House)
OCC_Weekly_Report_Output.2017.05.18<-OCC_Weekly_Report_Output.2017.05.18[c(1:29,31:38)]
His_Lat<-rbind(Combined_FY14_15_16_Final_Data_Historical.2017.01.20,OCC_Weekly_Report_Output.2017.05.18)
Msft_Full<-subset(His_Lat, Up_Classification == 'Microsoft')

install.packages('ggplot2')

ggplot(d) + 
  geom_line(aes(y=XBOX_Combined_Report_Output_FY14_15_16_17.2017.07.06$AdSpend, x=XBOX_Combined_Report_Output_FY14_15_16_17.2017.07.06$Year, color=cvr)) +
  stat_smooth(aes(y=XBOX_Combined_Report_Output_FY14_15_16_17.2017.07.06$AdSpend, x=XBOX_Combined_Report_Output_FY14_15_16_17.2017.07.06$Year), method = lm, formula = y ~ poly(x, 10), se = FALSE)



inhouse_entries1<-subset(Final_Data,In.House == 'Y')
inhouse_entries2<-subset(Final_Data,In.House == 'N')
inhouse_Total<-rbind(inhouse_entries1,inhouse_entries2)
write.csv(Youtube_Entries,file = paste('Youtube_Entries',Sys.Date(),"csv",sep = '.'))

Business_Entries<-subset(Combined_Data,BusVsCons == "Business")

msftsub<-subset(Consumer_Entries,Advertiser == 'Microsoft')
colSums(msftsub$AdSpend) 
write.csv(msftsub,'msftsub.csv')

Youtube_Entries<-subset(OCC_Weekly_Report_Output.2017.04.20,MediaOutlet == 'YouTube')
colSums(Youtube_Entries$AdSpend)

?distinct()
Five_Cols<-Combined_FY14_15_16_Final_Data_Historical.2017.01.20[,c(2,6:8,31:35)]
levels(factor(Five_Cols$Classification))
Five_Brands<-subset(Five_Cols, Classification != 'Third Party')

Five_Cols_N<-OCC_Weekly_Report_Output.2017.04.20[,c(2,6:8,32:36)]
Five_Brands_N<-subset(Five_Cols_N, Classification != 'Third Party')

Final_Five_All_entries_WTP<-rbind(Five_Cols,Five_Cols_N)
Remdup1<-unique(Final_Five_All_entries_WTP[,1:9])


Prefinal_Data<-rbind(Own_Spends,CoPartner_Spends, ThirdParty_Spends)

install.packages("tm",dependencies=TRUE)
install.packages("NLP")

library(tm)

Removed_Amazon_Own<-subset(Combined_FY14_15_16_Final_Data_Historical.2017.01.20, Classification != 'Amazon Own')

levels(factor(Removed_Amazon_Own$Classification))
Amazon_New_Own_Combined<-rbind(Amazon_FY14_Media_Spend...Copy, Amazon_FY15_Media_Spend...Copy, Amazon_FY16_Media_Spend...Copy)

levels(Amazon_New_Own_Combined)
colnames(Amazon_New_Own_Combined)

data2013<-subset(FY14_Data, Year == '2013')
data2014<-subset(FY14_Data, Year == '2014')
Remsixmonth<-subset(data2013, Year !='6')
Remsevenmonth<-subset(data2014, Year !='7')

FY14_Data<-rbind(Remsixmonth,Remsevenmonth)


sixdata2016<-subset(data2016, MonthNum == '6')
write.csv(FY14_Data, file = paste("FY14",Sys.Date(),"csv",sep = '.'))

amz17<-subset(OCC_Weekly_Report_Output.2017.06.22, Up_Classification == 'Amazon')

amzhis<-rbind(Amazon_FY14_Media_Spend...Copy,Amazon_FY15_Media_Spend...Copy,Amazon_FY16_Media_Spend...Copy)
levels(factor(Amazon_FY16_Media_Spend...Copy$Category))

Amazon_Historical<-Amazon_Historical[,1:28]
write.csv(Amazon_Historical, file = paste("Amazon_Historical",Sys.Date(),"csv",sep = '.'))

install.packages('gtools')
XBOX_Combined_Report_Output_FY14_15_16_17.2017.07.06$In.House<-'NA'

setwd('D:/CAMPAIGN/XBOX & OCC in R/OCC/HISTORICAL DATA_FY14,15 &16 DATA/OUTPUTS/FY14151617')
write.csv(Business_Entries, 'Business_Entries_Output_07112017.csv')

levels(factor(Business_Entries$Category))
Business_Entries<-subset(FUll_FY14151617, BusVsCons == 'Business')

Combined_Data<-FY17_Data
write.csv(FY17_Data, 'FY17_Data.csv')

sixmonthFY14<-subset(FY14_Data, MonthNum == '6'& Year == '2013')
sevenmonthFY14<-subset(FY14_Data,MonthNum == '7' & Year == '2014')

sixmonthFY15<-subset(FY15_Data, MonthNum == '6'& Year == '2014')
sevenmonthFY15<-subset(FY15_Data,MonthNum == '7' & Year == '2015')

sixmonthFY16<-subset(FY16_Data, MonthNum == '6'& Year == '2015')
sevenmonthFY16<-subset(FY16_Data,MonthNum == '7' & Year == '2016')

sixmonthFY17<-subset(FY17_Data, MonthNum == '6'& Year == '2016')
sevenmonthFY17<-subset(FY17_Data,MonthNum == '7' & Year == '2017')
----------------------------------------------------
data2015<-subset(FY15_Data, Year == '2015')
data2014<-subset(FY15_Data, Year == '2014')
Remsixmonth<-subset(data2014, MonthNum !='6')

FY15_Data<-rbind(data2015,Remsixmonth)
----------------------------------------------------
FY14_Data<-FY14_Data[,2:31]
FY15_Data<-FY15_Data[,2:31]
FY16_Data<-FY16_Data[,2:31]
FY17_Data<-FY17_Data[,2:31]

setwd('D:/CAMPAIGN/XBOX & OCC in R/COMMON RAW DATAFILES/HISTORICAL')
write.csv(FY14151617_RawData_New_Amz, 'FY14151617_RawData_New_Amz.csv')

FY141516<-rbind(FY14_Data,FY15_Data,FY16_Data)
RemoveAmazon<-subset(FY141516,Advertiser != 'Amazon')
Amzonly<-subset(FY141516, Advertiser == 'Amazon')

FY141516_With_New_Amzdata<-rbind(RemoveAmazon, Amazon_FY141516_Reformatted_RawData)
FY14151617_RawData_New_Amz<-rbind(FY141516_With_New_Amzdata, FY17_Data)
mediacheck<-subset(Amazon_FullData_Reformatted_FY141516, Media.Week == '0')

levels(factor(Amazon_FullData_Reformatted_FY141516$Media.Week))
Amzwomwzero<-subset(Amazon_FullData_Reformatted_FY141516, Media.Week != '0')
Amz14<-subset(mediacheck, FY == 'FY14')
Amz15<-subset(mediacheck, FY == 'FY15')
Amz16<-subset(mediacheck, FY == 'FY16')

Amz16$Quarter<-'Q4'
Amz16$MonthNum<-12
Amz16$Year<-2015
Amz16$Media.Week<-52

Amzchangedmwzero<-rbind(Amz14,Amz15,Amz16)
levels(factor(Consumer_Entries$Media.Week))

Amazon_FY141516_Reformatted_RawData<-rbind(Amzwomwzero, Amzchangedmwzero)

Microsoft_FY17<-subset(FY17_Data, Advertiser == 'Microsoft')
levels(factor(Microsoft_FY17$Advertiser))
write.csv(Microsoft_FY17,'Microsoft_FY17.csv')

FY17_File<-subset(FY14151617_Final_Processed_Output.2017.07.27, FY == 'FY17')
FY17_File<-FY17_File[,2:36]
OCC_Weekly_Report_Output.2017.09.08<-OCC_Weekly_Report_Output.2017.09.08[,2:36]
FY17_FY18_Till_Aug_end<-rbind(FY17_File,OCC_Weekly_Report_Output.2017.09.08)
write.csv(FY17_FY18_Till_Aug_end, 'FY17_FY18_Till_Aug_end.csv')

OCC_Weekly_Report_Output.2017.09.08$Date_WeekStarting<-as.Date(paste("1", OCC_Weekly_Report_Output.2017.09.08$Media.Week, OCC_Weekly_Report_Output.2017.09.08$Year, sep = "-"), format = "%w-%W-%Y")
OCC_Weekly_Report_Output.2017.09.08$Date_WeekEnding<-as.Date(paste("1", OCC_Weekly_Report_Output.2017.09.08$Media.Week, OCC_Weekly_Report_Output.2017.09.08$Year, sep = "-"), format = "%w-%W-%Y")+6

AWSData<-subset(BusinessEntriesData, Product.Campaign == 'Amazon Web Services')
AWSData<-AWSData[,2:31]

Busient<-subset(Combined_Data, BusVsCons == 'Business')
AWSData1<-subset(Busient, Product.Campaign == 'Amazon Web Services')
levels(factor(AWSData1))
colnames(AWSData)

FULLAWSDATA<-rbind(AWSData, AWSData1)
setwd('D:/WORK/XBOX & OCC in R/OCC/WEEKLY FY18/OUTPUTS')
write.csv(FULLAWSDATA, file = paste("FULLAWSDATA",Sys.Date(),"csv",sep = '.'))
--------------------------------------------------------------------------------
install.packages('coinmarketcapr')
library(coinmarketcapr)
plot_top_5_currencies()

market_today <- get_marketcap_ticker_all()
head(market_today[,1:8])

install.packages('treemap')
library(treemap)
df1 <- na.omit(market_today[,c('id','market_cap_usd')])
df1$market_cap_usd <- as.numeric(df1$market_cap_usd)
df1$formatted_market_cap <-  paste0(df1$id,'\n','$',format(df1$market_cap_usd,big.mark = ',',scientific = F, trim = T))
treemap(df1, index = 'formatted_market_cap', vSize = 'market_cap_usd', title = 'Cryptocurrency Market Cap', fontsize.labels=c(12, 8), palette='RdYlGn')
--------------------------------------------------------------------------------------------------------------------------------------------------------------------

install.packages("installr")
library(installr)
updateR()

levels(factor(Business_Entries$Advertiser))
Business_Entries<-subset(FY14151617_RawData_New_Amz, BusVsCons == 'Business')
  
setwd('D:/WORK/R RELATED/XBOX & OCC in R/BUSINESS ENTRIES')
write.csv(Business_Entries, file = paste("BUSINESSDATA_HISTORICAL_FY14151617",Sys.Date(),"csv",sep = '.'))


df1$Sentiment<-NA
df1<-as.data.frame(data_27112017) 

df2<-sentiment(data_27112017[,6])
df1<-df1[,1:6]

df3<-cbind(df1,df2)

df4<-df1[,6] %>% extract_sentiment_terms()

data_27112017$element_id<-1:nrow(data_27112017)
df2$element_id<-1:nrow(df2)

df3<-merge(data_27112017,df2, by = 'element_id')

data_27112017<-data_27112017[,1:7]
splitthesentences <- strsplit(chart,'.')

chart<-as.character(data_27112017$Q001_999)
df4<-subset(df3, is.na())


library(qdap)
install.packages('magrittr')
library(magrittr)
df5<-data_27112017 %$% polarity(Q001_999, ResponseID)
df5<-as.data.frame(df5)



---------------------------------------------------------------------------------
Jan_2017<-Australia_Jan_2017[,c(1:28)]
Feb_2017<-Australia_Feb_2017[,c(1:28)]
Mar_2017<-Australia_Mar_2017[,c(1:28)]
Apr_2017<-Australia._April_2017[,c(1:28)]
May_2017<-Australia_May_2017[,c(1:28)]
Jun_2017<-Australia_June_2017[,c(1:28)]
Jul_2017<-Australia_July_2017[,c(1:28)]
Aug_2017<-Australia_August_2017[,c(1:28)]
Sep_2017<-Australia_September_2017[,c(1:28)]

Combined_Data<-rbind(Jan_2017,Feb_2017,Mar_2017,Apr_2017,May_2017,Jun_2017,Jul_2017,Aug_2017,Sep_2017)
Combined_Data$CY<-"2017"
colnames(Feb_2017)

Jancols<-c(colnames(Jan_2017))
Febcols<-c(colnames<-(Feb_2017))
Marcols<-c(colnames<-(Mar_2017))
Aprcols<-colnames<-(Apr_2017)
Maycols<-colnames<-(May_2017)
Juncols<-colnames<-(Jun_2017)
Julcols<-colnames<-(Jul_2017)
Augcols<-colnames<-(Aug_2017)
Sepcols<-colnames<-(Sep_2017)

library(dplyr)
Z <- rbind.fill(Jan_2017,Feb_2017,Mar_2017,Apr_2017,May_2017,Jun_2017,Aug_2017,Sep_2017)

colnames <- c(Jancols,Febcols,Marcols)

Final_Data<-Prefinal_Data




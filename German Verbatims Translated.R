install.packages("translateR")
library("translateR")
library("textcat")

##Convert the factor columns to character which need to be translated##
German.Verbatims$data1<-as.character(German.Verbatims$QCON_02a)
German.Verbatims$data2<-as.character(German.Verbatims$QCON_02b)
German.Verbatims$data3<-as.character(German.Verbatims$AQ_05a)
German.Verbatims$data4<-as.character(German.Verbatims$AQ_05b)

German.Verbatims1<-German.Verbatims[,17:20]

##Check if each of the newly created columns are characters
#str(German.Verbatims$data1)

##Translation Code using the GOogle Translator API by each subset divided as above
##Do not add the $ sign to create a new row else we will get the error of multinested lists
##Just add the variable you want to be created as extra in the dataframe, the function translate
##takes this by default as it has the inbuilt ability

data1_translated <- translate(dataset = German.Verbatims1, content.field = 'data1',
                              google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                              source.lang = 'de',
                              target.lang = 'en')

data2_translated <- translate(dataset = German.Verbatims1, content.field = 'data2',
                              google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                              source.lang = 'de',
                              target.lang = 'en')

Translated_Verbatims<-cbind(German.Verbatims,data1_translated[,5],data2_translated[,5],data3_translated[,5],data4_translated[,5])

write.csv(Translated_Verbatims,'Translated_Verbatims.csv')

##EOC
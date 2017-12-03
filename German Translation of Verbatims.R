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

##Split the dataframe into 200 records each since the Google API is only taking 200 translations records each

d1a<-German.Verbatims1[1:100,]
d1b<-German.Verbatims1[101:200,]
d1c<-German.Verbatims1[201:300,]
d1d<-German.Verbatims1[301:400,]
d1e<-German.Verbatims1[401:500,]
d1f<-German.Verbatims1[501:600,]
d1g<-German.Verbatims1[601:700,]
d1h<-German.Verbatims1[701:800,]
d1i<-German.Verbatims1[801:840,]

##Translation Code using the GOogle Translator API by each subset divided as above

d1a$data1_translated <- translate(dataset = d1a, content.field = 'data1',
                                google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                                source.lang = 'de',
                                target.lang = 'en')

d1b$data1_translated <- translate(dataset = d1b, content.field = 'data1',
                                               google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                                               source.lang = 'de',
                                               target.lang = 'en')

d1c$data1_translated <- translate(dataset = d1c, content.field = 'data1',
                                               google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                                               source.lang = 'de',
                                               target.lang = 'en')

d1d$data1_translated <- translate(dataset = d1d, content.field = 'data1',
                                               google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                                               source.lang = 'de',
                                               target.lang = 'en')

d1e$data1_translated <- translate(dataset = d1e, content.field = 'data1',
                                  google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                                  source.lang = 'de',
                                  target.lang = 'en')


d1f$data1_translated <- translate(dataset = d1f, content.field = 'data1',
                                  google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                                  source.lang = 'de',
                                  target.lang = 'en')

d1g$data1_translated <- translate(dataset = d1g, content.field = 'data1',
                                  google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                                  source.lang = 'de',
                                  target.lang = 'en')

d1h$data1_translated <- translate(dataset = d1h, content.field = 'data1',
                                  google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                                  source.lang = 'de',
                                  target.lang = 'en')

d1i$data1_translated <- translate(dataset = d1i, content.field = 'data1',
                                  google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                                  source.lang = 'de',
                                  target.lang = 'en')

German.Verbatims_translated_data1<-rbind(d1a, d1b, d1c, d1d, d1e, d1f, d1g, d1h, d1i)
write.csv (German.Verbatims_translated_data1, file = 'data1.csv')

German.Verbatims1$data1_translated <- translate(dataset = German.Verbatims1, content.field = 'data1',
                                  google.api.key = 'AIzaSyCcXBtJe5cNWJ4qJ4XPrf-V0nT-lsefxlA',
                                  source.lang = 'de',
                                  target.lang = 'en')
##EOC
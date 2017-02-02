library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidytext)

setwd("../data/")
Info<-read.xlsx("InaugurationInfo.xlsx",1)

folder.path="../data/InauguralSpeeches/"
speeches=list.files(path = folder.path, pattern = "*.txt")
prez.out=substr(speeches, 6, nchar(speeches)-4)

length.speeches=rep(NA, length(speeches))
ff.all<-Corpus(DirSource(folder.path))

length.speeches=rep(NA, length(speeches))
ff.all<-Corpus(DirSource(folder.path))


ff.all<-tm_map(ff.all, stripWhitespace)
ff.all<-tm_map(ff.all, content_transformer(tolower))
ff.all<-tm_map(ff.all, removeWords, stopwords("english"))
ff.all<-tm_map(ff.all, removeWords, character(0))
ff.all<-tm_map(ff.all, removePunctuation)
mylist<-c("can","shall","should","will","may","must")
ff.all<-tm_map(ff.all,removeWords,mylist)


tdm.all<-TermDocumentMatrix(ff.all)
tdm.tidy=tidy(tdm.all)

tdm.tidy$name<-substr(tdm.tidy$document,6,nchar(tdm.tidy$document)-6)
unique_name<-match(tdm.tidy$name,Info$File)
tdm.tidy$party<-Info$Party[unique_name]

ana_party<-tdm.tidy[,c(1,3,5)]
w_Repub<-ana_party[ana_party$party=="Republican",1:2]
w_Demo<-ana_party[ana_party$party=="Democratic",1:2]


w_Repub<-summarise(group_by(w_Repub, term), sum(count))
w_Demo<-summarise(group_by(w_Demo, term), sum(count))

w_ana<-merge(w_Demo,w_Repub,by="term",all = TRUE)
colnames(w_ana)<-c("term","Democount","Repubcount")
w_ana$Democount[is.na(w_ana$Democount)]<-0
w_ana$Repubcount[is.na(w_ana$Repubcount)]<-0



w_ana$odd<-log(w_ana$Demo/w_ana$Repub)

Repub<-w_ana[w_ana$odd==-Inf|w_ana$odd<(-0.8),c(1,3)]
Demo<-w_ana[w_ana$odd==Inf|w_ana$odd>(0.8),c(1,2)]

set.seed(1234)
wordcloud(words = Demo$term, freq = Demo$Democount, min.freq = 1, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = Repub$term, freq = Repub$Repubcount, min.freq = 1, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
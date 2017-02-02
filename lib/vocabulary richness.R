
library(xlsx)
library(qdap)
library(ggplot2)
library(plotly)


setwd("../data/")
Info<-read.xlsx("InaugurationInfo.xlsx",1)
date<-read.csv("date.csv")

setwd("../data/InauguralSpeeches/")


count<-function(name){
  n_sentence<-length(sent_detect(name))
  
  word<-word_split(name)# seperate words
  word<-strip(word[[1]])# remove punctuation
  word<-word[word!=""] 
  word<-unique(word) # delete duplicate words
  n_word<-length(word)
  
  return(c(n_sentence,n_word))
}


filename<-paste("inaug",Info$File,"-",Info$Term,".txt",collapse = NULL,sep="")

matrix_count<-matrix(NA,ncol=3,nrow = 58)

for(i in 1:(length(filename)-1)){
  
  text<-readLines(filename[i],warn = FALSE)
  matrix_count[i,2:3]<-count(text)
  
}



count<-as.data.frame(matrix_count)
colnames(count)<-c("file","sentence","word")
count$file<-paste(Info$File,Info$Term)
count$var<-count$word/count$sentence
count$party<-Info$Party


date$Date<-as.character(date$Date)
year<-substr(date$Date,nchar(date$Date)-3,nchar(date$Date))
year<-year[year!=""]

count$year<-year
count$year<-as.numeric(count$year)



p<-ggplot(data=count)+
  geom_point(mapping = aes(x=year,y=var,col=party))+
  geom_line(mapping = aes(x=year,y=var,col="blue"))+
  geom_text(x=1942,y=8,label="ww2")+
  geom_text(x=1916,y=8,label="ww1")+
  geom_text(x=1862,y=6,label="civil war")+
  geom_text(x=1947,y=5,label="cold war start")+
  geom_text(x=1991,y=6,label="cold war end")

ggplotly(p)

p<-ggplot(data=count)+
  geom_point(mapping = aes(x=year,y=var,col=party))+
  geom_line(mapping = aes(x=year,y=var,col="red"))+
  geom_text(x=1837,y=11,label="Panic 1837")+
  geom_text(x=1893,y=14,label="Panic 1893")+
  geom_text(x=1907,y=8,label="Panic 1907")+
  geom_text(x=1929,y=5.5,label="wall street")+
  geom_text(x=2008,y=6.7,label="global")+
  geom_text(x=1989,y=4,label="saving&loan")

ggplotly(p)



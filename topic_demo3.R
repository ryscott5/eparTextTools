#topic_demo3
#Run this code to read in the new functions
source("textFunctions.R")

source("demo.docs.R")

corpus1<-allDocs("demo.docs.folder")

source("TopicFunctions.R")

BASE_INPUT<-PreTopicFrame(corpus1,25)

form1<-paste("~as.factor(Orig)",paste(colnames(BASE_INPUT$out$meta)[12:ncol(BASE_INPUT$out$meta)],sep="",collapse="+"),sep="+")

#ideally, we would use the spectral modle but it crashes R unfortunately, instead, we fit the model below. notice that k is the number of topics. each document is assumed to have multiple categories (Blei et al 2003). We extract the most likely topic for each document. The topic model is also used on the back end for evaluating causal patterns as it forms the basis of word clustering etc.

#st1<-stm(BASE_INPUT$out$documents,BASE_INPUT$out$vocab,data=BASE_INPUT$out$meta, K=0,prevalence=eval(parse(text=form1)), init.type="Spectral",max.em.its=300)

st1<-stm(BASE_INPUT$out$documents,BASE_INPUT$out$vocab,data=BASE_INPUT$out$meta, K=10,prevalence=eval(parse(text=form1)), init.type="LDA",max.em.its=200)

#about 174 rounds to convergence!

#here, we extract the most likely topics from the topic model. importantly, each document has all of the topics included within it.
tps<-max.col(st1$theta)

#here, we read from a table of verbs to the wd dataframe.
wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)

#adds annotations for subjects, nouns and verbs by topics, then only keeps sentences that use words from the "Up.Words" column of wd.

Pf1<-AnnotateVerbsByTopic(tps,wd$Up.Words,BASE_INPUT$processed,BASE_INPUT$out,BASE_INPUT$Annotations,BASE_INPUT$SentFrame)

library(jsonlite)
library(plyr)

#calls to alchemy api,filling folder named "test2"

#FillFolder(Pf1,"test2")

Frame1<-ParseFolderToFrame("test2",Pf1,wd$Up.Words)

head(Frame1[[1]])
frametable.html(Frame1[[1]], BASE_INPUT,"test2",Pf1)
head(Frame1[[1]])
basic_table<-frametable(Frame1[[1]], BASE_INPUT,"test2",Pf1,st1)
save(basic_table,st1,file="basic_topic3.RData")


clusterWords(basic_table$Object,100,st1)
clusterWords(basic_table$Subject,80,st1)
clusterWords(basic_table$Verb,10,st1)

idSimilar("women",basic_table$Object,100,st1) 


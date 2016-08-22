source("TopicFunctions.R")
BASE_INPUT<-PreTopicFrame(corpus1,25)

form1<-paste("~as.factor(Orig)",paste(colnames(BASE_INPUT$out$meta)[12:ncol(BASE_INPUT$out$meta)],sep="",collapse="+"),sep="+")

st1<-stm(BASE_INPUT$out$documents,BASE_INPUT$out$vocab,data=BASE_INPUT$out$meta, K=10,prevalence=eval(parse(text=form1)), init.type="LDA",max.em.its=20)


tps<-max.col(st1$theta)

wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)

Pf1<-AnnotateVerbsByTopic(tps,wd$Up.Words,BASE_INPUT$processed,BASE_INPUT$out,BASE_INPUT$Annotations,BASE_INPUT$SentFrame)

FillFolder(Pf1,"test1")

Frame1<-ParseFolderToFrame("test1",Pf1)

cormats1<-TopicCoreFrame(tps,Frame1)
#Run this code to read in the new functions
source("textFunctions.R")

source("demo.docs.R")

corpus1<-allDocs("demo.docs.folder")


#

source("TopicFunctions.R")

BASE_INPUT<-PreTopicFrame(corpus1,25)

form1<-paste("~as.factor(Orig)",paste(colnames(BASE_INPUT$out$meta)[12:ncol(BASE_INPUT$out$meta)],sep="",collapse="+"),sep="+")

#st1<-stm(BASE_INPUT$out$documents,BASE_INPUT$out$vocab,data=BASE_INPUT$out$meta, K=0,prevalence=eval(parse(text=form1)), init.type="Spectral",max.em.its=300)

st1<-stm(BASE_INPUT$out$documents,BASE_INPUT$out$vocab,data=BASE_INPUT$out$meta, K=10,prevalence=eval(parse(text=form1)), init.type="LDA",max.em.its=200)

#about 174 rounds to convergence!

tps<-max.col(st1$theta)

wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)

Pf1<-AnnotateVerbsByTopic(tps,wd$Up.Words,BASE_INPUT$processed,BASE_INPUT$out,BASE_INPUT$Annotations,BASE_INPUT$SentFrame)

table(Pf1$L1)

BASE_INPUT$SentFrame[-BASE_INPUT$processed$docs.removed,][-BASE_INPUT$out$docs.removed,]$id[Pf1$value[as.numeric(Frame1$SmallVerb$whichi)]]


Pf1$L2[as.numeric(Frame1$SmallVerb$whichi)]
Pf1<-Pf1[which(nchar(Pf1$Sent)>20),]
Pf1<-unique(Pf1[,c(1,3,5)])
colnames(Pf1)

library(jsonlite)
Frame1<-ParseFolderToFrame("test2",Pf1,wd$Up.Words)

BASE_INPUT$SentFrame[-BASE_INPUT$processed$docs.removed,][-BASE_INPUT$out$docs.removed,]
colnames(Frame1[[1]])


frametable<-function(PARSEFRAME,BASEINPUT,FOLDERNAME,PREPFRAME){
    OV<-PREPFRAME[as.numeric(list.files(file.path("getAlchemy",FOLDERNAME)) %>% gsub(".json","",.) %>% gsub("al","",.) %>% .[as.numeric(PARSEFRAME$whichi)]),]
    OVF<-BASEINPUT$SentFrame[-BASEINPUT$processed$docs.removed,][-BASEINPUT$out$docs.removed,][OV$value,] 
    PARSEFRAME<-cbind(PARSEFRAME,OVF[,c("author","datetimestamp","id","ents")])
    PARSEFRAME<-PARSEFRAME[,c(1,2,3,5,7,8,9,10,11)]
   colnames(PARSEFRAME)<-c('Subject','Object',"Verb","Sentence","Topic","Author","Date","Document","Entities")
     PARSEFRAME$Topic<-sapply(1:10,function(i) paste(labelTopics(st1,n=5)[[1]][i,],collapse=","))[PARSEFRAME$Topic]
     PARSEFRAME$Topic<-as.factor(PARSEFRAME$Topic)
     PARSEFRAME$Verb<-as.factor(PARSEFRAME$Verb)
     PARSEFRAME}
     
ttab<-frametable(fullframe,BASE_INPUT,"test2",Pf1)
head(ttab)

geoLocate<-function(PARSEFRAME,BASEINPUT,FOLDERNAME,PREPFRAME){
  OV<-PREPFRAME[as.numeric(list.files(file.path("getAlchemy",FOLDERNAME)) %>% gsub(".json","",.) %>% gsub("al","",.) %>% .[as.numeric(PARSEFRAME$whichi)]),]
  OVF<-c(1:length(BASEINPUT$Annotations))[-BASEINPUT$processed$docs.removed][-BASEINPUT$out$docs.removed][OV$value] 
  lapply(OVF, function(i){
  esub<-subset(BASEINPUT$Annotations[[i]],type=="entity")
  as.character(as.String(BASEINPUT$SentFrame$S[i])[esub[which(unlist(lapply(esub, function(x) unlist(x$features)=="location"))==TRUE)]])
  })}

geosLocs<-geoLocate(fullframe,BASE_INPUT,"test2",Pf1)
geosLocs


callOSM<-function(GEOS){
psends<-data.frame("value"=unique(GEOS$value))
library(RCurl)
tf<-lapply(1:nrow(psends), function(i){Sys.sleep(2)
fromJSON(getURL(URLencode(paste("http://nominatim.openstreetmap.org/search?q=",gsub("[[:punct:s]|[:punct:]]+", "",as.character(psends$value)[i],fixed=F),"&email=ryscott5@uw.edu&format=json&limit=1",sep=""))))})
names(tf)<-psends$value
tf
}

outLoc<-callOSM(geosLocs)





2*400
geosLocs$value[2]
?gsub
unique(gsub("(\\W|\\W+s, \\s)","",as.character(geosLocs$value),fixed=F))
geoM<-callOSM(geosLocs)
head(geosLocs)
library(nominatim)
?osm_search
osm_search(as.character(geosLocs$value[1]), email="ryscott5@uw.edu",key = getOption("OSM_API_KEY", "NA"))
datatable(data=PARSEFRAME,rownames=FALSE,filter="top")

osm_search
devtools::install_github("hrbrmstr/nominatim")
head(fullframe)
frametable(fullframe,BASE_INPUT,"test2",Pf1)

as.numeric(list.files(file.path("getAlchemy","test2")) %>% gsub(".json","",.) %>% gsub("al","",.) %>% .[as.numeric(fullframe$whichi)])[1]
as.numeric(fullframe$whichi)[1]

head(fullframe)
library(stringr)
sentframeID<-lapply(1:nrow(fullframe), function(i){which(str_detect(BASE_INPUT$SentFrame$SC,fixed(fullframe$Sent[i])))})
sentframeID2<-unlist(sentframeID)
sentframeID[1]
head(Se)
      [-BASE_INPUT$processed$docs.removed,][-BASE_INPUT$out$docs.removed,][OV,] 
       

frametable(fullframe,BASE_INPUT,"test2",Pf1)

frametable<-function(PARSEFRAME){
PARSEFRAME<-PARSEFRAME[,c(1,2,3,4,5,9)]
colnames(PARSEFRAME)<-c('Subject Keyword','Object Keyword',"Verb","Paragraph","Sentence","Topic")
table(Pf1$whichi)
datatable(data=PARSEFRAME,rownames=FALSE,filter="top")}

frametable(Frame1[[1]])
cormats0<-TopicCoreFrame(tps,Frame1[[1]])
colnames(Frame1[[1]])
cormats1<-TopicCoreFrame(tps,Frame1[[1]])

f1<-SimpFrameList(cormats1)
f1[[1]]

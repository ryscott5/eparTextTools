source("textFunctions.R")
source("TopicFunctions.R")

#Specify Working Folder
workingfolder<-"Ag.Nutrition.Grants"
messages<-readMails("../AgandNut","../testnewdocs")

fullcorp<-c(tcorp,messages)

#removed .bmp and .db files manually
fileslist<-lapply(file.path("../AgandNut",list.files("../AgandNut")),getTextR)
fileslist<-fileslist[sapply(fileslist, function(X){length(X[[1]])})>1]

tcorp<-do.call(c,fileslist)


#clean up workspace
rm(fileslist)
rm(messages)
jgc()

#gets data ready for processing
BASE_INPUT<-PreTopicFrame(fullcorp,10)

#saves files so you can reload.
dir.create("workingfiles")
saveRDS(BASE_INPUT, "workingfiles/basefile.rds")
saveRDS(fullcorp, "workingfiles/basecorpus.rds")

#here we add opportunity labels to documents 

nex<-read.csv("../nonExcel.csv",stringsAsFactors=FALSE)

nexjoin<-plyr::join(data.frame("name"=BASE_INPUT$SentFrame$id),data.frame("name"=basename(as.character(nex$path)),"OpID"=as.character(nex$Opportunity.ID)),type="left",match="first")
BASE_INPUT$SentFrame$OpID<-nexjoin$OpID
nexjoin2<-plyr::join(data.frame("name"=BASE_INPUT$out$meta$id),data.frame("name"=basename(as.character(nex$path)),"OpID"=as.character(nex$Opportunity.ID)),type="left",match="first")
BASE_INPUT$out$meta$OpID<-nexjoin2$OpID
rm(list("nexjoin","nexjoin2","nex")

buildcliff()
startcliff()
pred1<-PredictCountryByDoc(BASE_INPUT)
stopcliff()

BASE_INPUT$out$meta<-reflectCountryCol(BASE_INPUT$out$meta,pred1,20,FALSE)

saveRDS(BASE_INPUT,file.path(workingfolder,"basefile.rds"))
write.csv(pred1,file.path(workingfolder,"countrypredictions1.csv"))

form1<-paste("~as.factor(Orig)",paste(select.list(colnames(BASE_INPUT$out$meta),multiple=TRUE),sep="",collapse="+"),sep="+")
saveRDS(form1,file.path(workingfolder,"formula1.csv"))

jgc()

st1<-stm(BASE_INPUT$out$documents,BASE_INPUT$out$vocab,data=BASE_INPUT$out$meta,prevalence=eval(parse(text=form1)),K=0, init.type="Spectral",max.em.its=500)
saveRDS(st1, file.path(workingfolder,"topicmodel.rds"))

BASE_INPUT$top.topics<-max.col(st1$theta)

#here, we read from a table of verbs to the wd dataframe. The function allows you to edit a google docs frame shared from the address, so you can add, subtract words. You also could replace the reading of the csv with a call to a local dataframe.

wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)
allwords<-c(wd$Up.Words,wd$Down.Words)

AnnotatesLarge<-AnnotateVerbsTopicJoin(allwords,BASE_INPUT$processed,BASE_INPUT$out,BASE_INPUT$Annotations,BASE_INPUT$SentFrame)
AnnotatesSmaller<-CombinationFrame(AnnotatesLarge)
rm(AnnotatesLarge)
saveRDS(AnnotatesSmaller,file.path(workingfolder,"AnnotationFrame.rds"))

ProcessedANNS<-ProcessforAPI(AnnotatesSmaller)

FillFolder(ProcessedANNS,workingfolder)

Frame1<-ParseFolderToFrame(workingfolder,ProcessedANNS,allwords)
saveRDS(Frame1,file.path(workingfolder,"ParsedFrame.rds"))

Frame1<-readRDS("Frame1out.rds")



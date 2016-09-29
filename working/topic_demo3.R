#topic_demo3
#Run this code to read in the new functions
install.packages("devtools")
install.packages("tm.plugin.mail")
source("textFunctions.R")

dir.create("../docs2")
library(stringr)
messages<-list.files("../docs",full.names=TRUE)[str_detect(tools::file_ext(list.files("../docs")),"msg")]
plyr::l_ply(messages,function(X) system(paste("msgconvert --mbox ../docs/msgs.mbox",X)))
file.remove(messages)
library(tm.plugin.mail)
mails<-MBoxSource("../docs2/msgs.mbox")
mbases<-list.files("../docs")[tools::file_ext(list.files("../docs"))%in%"mbox"==FALSE]
file.copy("../docs/msgs.mbox","../docs2/msgs.mbox")
file.remove("../docs/msgs.mbox")

#removed .bmp and .db files manually
fileslist<-lapply(file.path("../docs",list.files("../docs")),getTextR)
fileslist2<-fileslist[sapply(fileslist, function(X){length(X[[1]])})>1]
tcorp<-do.call(c,fileslist2)
mailsc<-Corpus(mails)
fullcorp<-c(tcorp,mailsc)
class(fullcorp)
fullcorp[[310]]$meta
rm(fileslist)
rm(fileslist2)
rm(mails)
rm(mailsc)
rm(messages)
rm(tcorp)
#we skip a lot of the descriptives in this demo
source("TopicFunctions.R")
jgc()

#instead going straight tot he topic model. Here, we are going to make a data frame for the topic model.25 is a list of how many entitites to use in the topic model. PreTopicFrame is mostly a cleaning function, it takes the data, breaks it into paragraphs, and identifies the X most relevant entities across the dataset based on frequency of occurance. We can then use these entities to improve the structure of the topic model.

BASE_INPUT<-PreTopicFrame(fullcorp,25)
saveRDS(BASE_INPUT, "basefile.rds")
saveRDS(fullcorp, "basecorpus.rds")

#Next, we specify a formula for the topic model.We are fitting a structural topic model, which means we can use metadata describing documents/paragraphs to improve the models ability to fit a latent topic space to the words included within. There are a few things to consider here methodologically. First, we include fixed effects for each document ("Orig"). This is important because two paragraphs from the same document are not independent. We also include the presence of entities in the document as  fixed effect. This means that we would have many many fixed effects for this model, which is generally fine as it is giving more information to fitting the model, and there are roughly 5000 words with which to "play" with. If there are fewer than 5000 words, you might want to decrease the number of entities selected above.

test<-reflectCountryCol(BASE_INPUT$out$meta,pred1,30,FALSE)

nexjoin2<-plyr::join(data.frame("name"=BASE_INPUT$out$meta$id),data.frame("name"=basename(as.character(nex$path)),"OpID"=as.character(nex$Opportunity.ID)),type="left",match="first")
head(nexjoin2)
BASE_INPUT$out$meta$OpID<-nexjoin2$OpID
BASE_INPUT$out$meta<-reflectCountryCol(BASE_INPUT$out$meta,pred1,20,FALSE)
colnames(BASE_INPUT$out$meta)



options(java.parameters = "-Xmx2048m")

form1<-paste("~as.factor(Orig)",paste(select.list(colnames(BASE_INPUT$out$meta),multiple=TRUE),sep="",collapse="+"),sep="+")
form1
26 29 31 37 38 39 40 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57


#next, we fit our topic model using the stm command. The decision on how many topics doesn't have a great answer. Numbers like 10 always sound nice, but I think a value such as 20 or 30 is actually probably better as it provides more information for clustering words at the end of the analysis. If the model doesn't converge, it can easily be restarted by inputing st1 into a new stm call in the model arguement.

?save
save(list=c("BASE_INPUT","fullcorp","pred1"),file="keepthese921.Rdata")
attach("keepthese921.Rdata")
BASE_INPUT$out$meta$OpID<-as.character(BASE_INPUT$out$meta$OpID)
BASE_INPUT$out$meta$OpID[is.na(BASE_INPUT$out$meta$OpID)]<-"unknown"

st1<-stm(BASE_INPUT$out$documents,BASE_INPUT$out$vocab,data=BASE_INPUT$out$meta,prevalence=eval(parse(text=form1)),K=0, init.type="Spectral",max.em.its=500)

#st1<-stm(BASE_INPUT$out$documents,BASE_INPUT$out$vocab,data=BASE_INPUT$out$meta, K=0,prevalence=eval(parse(text=form1)), init.type="Spectral",max.em.its=300)
stm::labelTopics(st1, n=3)

#ideally, we would use the spectral model (commented out) but it crashes R. Instead we utilize Latent Dirchlet Association. Notice that k is the number of topics. each document is assumed to have multiple categories (Blei et al 2003). We extract the most likely topic for each document. The topic model is also used on the back end for evaluating causal patterns as it forms the basis of word clustering etc.


#about 174 rounds to convergence!

#here, we extract the most likely topics from the topic model. importantly, each document has all of the topics included within it. Thus, "tps" in the code below is the most likely topic, not the only topic for a given document.
BASE_INPUT$top.topics<-max.col(st1$theta)

#here, we read from a table of verbs to the wd dataframe. The function allows you to edit a google docs frame shared from the address, so you can add, subtract words. You also could replace the reading of the csv with a call to a local dataframe.
wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)
allwords<-c(wd$Up.Words,wd$Down.Words)
#Next, we add annotations for subjects, nouns and verbs by topics, then only keeps sentences that use words from the "Up.Words" column of wd. This is a step of the code that aids in limiting the amount of external resources used because it helps to filter the text to only relevant sentences.

Pf1<-AnnotateVerbsByTopic(tps,wd$Up.Words,BASE_INPUT$processed,BASE_INPUT$out,BASE_INPUT$Annotations,BASE_INPUT$SentFrame)

library(jsonlite)
library(plyr)

#Next, we make a call to alchemy api,filling folder named "test3". AlchemyAPI is a service provided by IBM Bluemix. It is excellent for relation extraction from text, though there are services from Microsoft, Google, etc. it could be compared to in the future.

recombine$Sent
?save
save(recombine,FillFolder,file="recombo.RData")
recombine$Sent<-gsub("\n","",recombine$Sent)

FillFolder(recombine[],"comboSents")




recombine[which(recombine$rowid==73),]
URLencode(recombine[73,]$Sent)
rm(toCharANS)
nrow(recombine)
rm(OUT)
rm(ANNOTATELIST)
rm(PROCESSED)
rm(STMOBJ)
rm(KeepA)
FillFolder(Pf1,"test3")

#Once the folder is filled, we load the individual .json files from the folder into a dataframe. We download the json files because it helps in debugging and keeps R from storing them all within its own memory. This slows the function down somewhat, but also makes it easier to run on systems that might eventually run out of memory.



Frame1<-ParseFolderToFrame("AgNut.Relations",recombine,allwords)

#once we have a frame, our work here is mostly done. we have a topic model, with relevant relations extracted, so now it is time to analyze results! calling the frametable command will reformate Frame1 into a nice looking table, while frametable.html will make an html table. 
frametable.html(Frame1[[1]], BASE_INPUT,"test2",Pf1)
head(Frame1[[1]])
basic_table<-frametable(Frame1[[1]], BASE_INPUT,"test2",Pf1,st1)

#By saving the nice table and the topic model to an .RData file, we can go one step further and build an interactive application to analyze the text.
clusterWords(basic_table$Verb,10,st1)

idSimilar("women",basic_table$Object,100,st1) 

save(basic_table,st1,file="basic_topic3.RData")



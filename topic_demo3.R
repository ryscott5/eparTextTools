#topic_demo3
#Run this code to read in the new functions
source("textFunctions.R")

source("demo.docs.R")

corpus1<-allDocs("demo.docs.folder")


#we skip a lot of the descriptives in this demo
source("TopicFunctions.R")


#instead going straight tot he topic model. Here, we are going to make a data frame for the topic model.25 is a list of how many entitites to use in the topic model. PreTopicFrame is mostly a cleaning function, it takes the data, breaks it into paragraphs, and identifies the X most relevant entities across the dataset based on frequency of occurance. We can then use these entities to improve the structure of the topic model.

BASE_INPUT<-PreTopicFrame(corpus1,25)


#Next, we specify a formula for the topic model.We are fitting a structural topic model, which means we can use metadata describing documents/paragraphs to improve the models ability to fit a latent topic space to the words included within. There are a few things to consider here methodologically. First, we include fixed effects for each document ("Orig"). This is important because two paragraphs from the same document are not independent. We also include the presence of entities in the document as  fixed effect. This means that we would have many many fixed effects for this model, which is generally fine as it is giving more information to fitting the model, and there are roughly 5000 words with which to "play" with. If there are fewer than 5000 words, you might want to decrease the number of entities selected above.

form1<-paste("~as.factor(Orig)",paste(colnames(BASE_INPUT$out$meta)[12:ncol(BASE_INPUT$out$meta)],sep="",collapse="+"),sep="+")


#next, we fit our topic model using the stm command. The decision on how many topics doesn't have a great answer. Numbers like 10 always sound nice, but I think a value such as 20 or 30 is actually probably better as it provides more information for clustering words at the end of the analysis. If the model doesn't converge, it can easily be restarted by inputing st1 into a new stm call in the model arguement.


st1<-stm(BASE_INPUT$out$documents,BASE_INPUT$out$vocab,data=BASE_INPUT$out$meta, K=30,prevalence=eval(parse(text=form1)), init.type="LDA",max.em.its=100)

#st1<-stm(BASE_INPUT$out$documents,BASE_INPUT$out$vocab,data=BASE_INPUT$out$meta, K=0,prevalence=eval(parse(text=form1)), init.type="Spectral",max.em.its=300)

#ideally, we would use the spectral model (commented out) but it crashes R. Instead we utilize Latent Dirchlet Association. Notice that k is the number of topics. each document is assumed to have multiple categories (Blei et al 2003). We extract the most likely topic for each document. The topic model is also used on the back end for evaluating causal patterns as it forms the basis of word clustering etc.


#about 174 rounds to convergence!

#here, we extract the most likely topics from the topic model. importantly, each document has all of the topics included within it. Thus, "tps" in the code below is the most likely topic, not the only topic for a given document.

tps<-max.col(st1$theta)

#here, we read from a table of verbs to the wd dataframe. The function allows you to edit a google docs frame shared from the address, so you can add, subtract words. You also could replace the reading of the csv with a call to a local dataframe.
wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)

#Next, we add annotations for subjects, nouns and verbs by topics, then only keeps sentences that use words from the "Up.Words" column of wd. This is a step of the code that aids in limiting the amount of external resources used because it helps to filter the text to only relevant sentences.

Pf1<-AnnotateVerbsByTopic(tps,wd$Up.Words,BASE_INPUT$processed,BASE_INPUT$out,BASE_INPUT$Annotations,BASE_INPUT$SentFrame)

library(jsonlite)
library(plyr)

#Next, we make a call to alchemy api,filling folder named "test3". AlchemyAPI is a service provided by IBM Bluemix. It is excellent for relation extraction from text, though there are services from Microsoft, Google, etc. it could be compared to in the future.

FillFolder(Pf1,"test3")

#Once the folder is filled, we load the individual .json files from the folder into a dataframe. We download the json files because it helps in debugging and keeps R from storing them all within its own memory. This slows the function down somewhat, but also makes it easier to run on systems that might eventually run out of memory.

Frame1<-ParseFolderToFrame("test2",Pf1,wd$Up.Words)

#once we have a frame, our work here is mostly done. we have a topic model, with relevant relations extracted, so now it is time to analyze results! calling the frametable command will reformate Frame1 into a nice looking table, while frametable.html will make an html table. 
frametable.html(Frame1[[1]], BASE_INPUT,"test2",Pf1)
head(Frame1[[1]])
basic_table<-frametable(Frame1[[1]], BASE_INPUT,"test2",Pf1,st1)

#By saving the nice table and the topic model to an .RData file, we can go one step further and build an interactive application to analyze the text.
clusterWords(basic_table$Verb,10,st1)

idSimilar("women",basic_table$Object,100,st1) 

save(basic_table,st1,file="basic_topic3.RData")



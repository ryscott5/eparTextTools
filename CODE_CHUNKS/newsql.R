library(devtools)
library(roxygen2)
library(stringr)
library(data.table)
library(igraph)
library(networkD3)
library(RSQLite)
library(RCurl)
library(dplyr)
matchtable<-readRDS("../Research.Grants/matchtable.rds")
dt2 <- data.table(matchtable)

dt2$object.keywords<-sapply(dt2$object.keywords,function(X) paste(unlist(X),collapse=";"))
dt2$subject.keywords<-sapply(dt2$subject.keywords,function(X) paste(unlist(X),collapse=";"))
dt2$action.lemmatized<-sapply(dt2$action.lemmatized,function(X) paste(unlist(X),collapse=";"))
dt2$action.verb.text<-sapply(dt2$action.verb.text,function(X) paste(unlist(X),collapse=";"))
#dt2<-dt2[,list(action.verb.text = tolower(unlist(strsplit(action.verb.text,";"))),object.keywords=tolower(unlist(strsplit(object.keywords,";"))),subject.keywords=tolower(unlist(strsplit(subject.keywords,";"))),Topic=TopTopics,Orig=Orig),by = sentence]
dt2<-dt2[,list(action.lemmatized = tolower(unlist(strsplit(action.lemmatized,";"))),object.keywords=tolower(unlist(strsplit(object.keywords,";"))),subject.keywords=tolower(unlist(strsplit(subject.keywords,";"))),Topic=TopTopics,Orig=Orig),by = sentence]

dt2$rowid<-1:nrow(dt2)
my_db <- dplyr::src_sqlite("../sqlite-31.db", create = F)
dt2$action.lemmatized<-str_extract(dt2$action.lemmatized,"\\w+$")
dt2<-left_join(dt2,distinct(tbl(my_db, sql("SELECT lemma,class from verbnetroles"))),by=c("action.lemmatized"="lemma"),copy=TRUE)
head(dt2)
dt2$base_Class<-str_extract(dt2$class,"\\w+")
dt2$class_cat<-as.numeric(str_extract(dt2$class,"[0-9]+"))
base_guideline<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1JBkTjHTW7YTRfHuJ-lZqjotK0XkvtdyI5tld-PUaQkw/pub?gid=0&single=true&output=csv")))
dt2<-left_join(dt2,base_guideline,by=c("class_cat"="Class"))

dt2$VerbType.simple<-as.character(dt2$Verb.Type)
dt2$VerbType.simple[which(dt2$VerbType.simple%in%names(sort(table(dt2$Verb.Type),decreasing=TRUE))[1:25]==FALSE)]<-"Other Verb Type"
table(dt2$VerbType.simple)
dt2part<-filter(dt2,rowid%in%which(1:max(dt2$rowid)%in%dt2$rowid[-which(dt2$VerbType.simple=="Verbs with Predicative Complements")]==FALSE))
dt2part<-rbind(dt2part,filter(dt2, VerbType.simple!="Verbs with Predicative Complements"))

dt2.sing<-lapply(1:max(dt2part$rowid),function(X){sample_n(filter(dt2part,rowid==X),1)}) %>% bind_rows()
?simplify
colnames(dt2.sing)
dt2.sing$
net1<-lapply(unique(dt2.sing$Orig),function(X) {
  lapply(unique(dt2.sing$VerbType.simple), function(Y) {
    temp<-filter(dt2.sing, Orig==X,VerbType.simple==Y)
    temp<-graph_from_data_frame(data.frame("In"=temp$subject.keywords,"Out"=temp$object.keywords))
  E(temp)$weight <- 1
  temp<-simplify(temp,remove.multiple=TRUE, edge.attr.comb=list(weight="sum"))
  temp
  })
  })
lso<-lapply(1:length(net1), function(k) {sapply(1:26,function(i) ecount(net1[[1]][[i]])+ecount(net1[[k]][[i]])-2*ecount(net1[[1]][[i]] %s% net1[[k]][[i]]))})
lsom<-do.call(rbind,lso)
plot(lsom)

degree(net1[[1]][[1]])
?graph.intersection
graph.intersection(net1[[1]][[1]],net1[[2]][[1]])
net1[[1]][[1]] %s% net1[[2]][[1]]
edge_attr(net1[[1]][[1]])

plot(net1[[1]][[6]])
net1<-simplify(net1, edge.attr.comb=list(weight="sum"))
net1[[2]]


plot(callwn2)


sort(table(dt2.sing$VerbType.simple),decreasing=TRUE)




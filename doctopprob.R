rm(ProcessedANNS)
colnames(matchtable)
matchtable$SnE<-NULL
colnames(matchtable)
matchtable2<-unique(matchtable)
matchtable$basename%in%
length(baseinput$top.topics)
library(stm)
nrow(st1$theta)
length(first)
st1$theta[first]
install.packages("prob")
length(which(is.na(matchtable$OpID)))
#684 documents
fnames<-list.files("../Fullset/Fullset_Demo")
fulsi<-read.csv("../Fullset/fullset.csv")
fulset<-fulsi[which(is.na(fulsi$path)==FALSE),]
fulsetmatch<-sapply(matchtable$basename,function(X) which(str_detect(fulset$path,fixed(as.character(X)))))
#256 Opportunities
fulsetmatch<-melt(fulsetmatch)
head(fulset)
X=51974
summary(fulsetmatch)
matchtable$Orig[51974]
matchtable$basename[51974]
nops<-sapply(1:nrow(baseinput$SentFrame), function(X) fulset$Opportunity.ID[which(str_detect(fulset$path,as.character(baseinput$SentFrame$id[X])))[1]])
nm<-data.frame("Orig"=baseinput$SentFrame$Orig,"OpID"=nops)
head(nm)
nm<-unique(nm)
head(nm)
mts<-sapply(1:nrow(matchtable), function(X) fulset$Opportunity.ID[fulsetmatch$value[which(fulsetmatch$L1==X)[1]]])
which(is.na(mts))
[which(is.na(matchtable$OpID))]
length(unique(na.omit(fulsi$Opportunity.ID)))

expopids<-as.character(baseinput$SentFrame$OpID)
baseinput$SentFrame[28589,]$id
fulset[which(str_detect(fulset$path,fixed(basename(as.character(baseinput$SentFrame[28589,]$id)))))
)
tail(expopids)
  which(c(baseinput$SentFrame$OpID%in%baseinput$out$meta$OpID)==FALSE)]

library(stm)
stm::mak
doctopprob<-function(docname,topic){
first<-which(baseinput$out$meta$id==docname)
data.frame("id"=docname,"prob"=prod(1-st1$theta[first,topic]))}

table(
bindate<-baseinput$out$meta$id[lubridate::date(baseinput$out$meta$datetimestamp)<"2011-01-01"]


AgProbs<-do.call(rbind, lapply(unique(baseinput$out$meta$id),function(X) try(doctopprob(X,na.omit(c(workpage$Ag,workpage$Nut))))))

agprobs2<-AgProbs[sort(AgProbs$prob,index.return=T,decreasing=TRUE)$ix,]
oldnon<-read.csv("../../bucket1/nonExcel.csv")
agprobs2$Ag.Nut.Port<-as.character(agprobs2$id)%in%basename(as.character(oldnon$path))
agprobs2<-dplyr::filter(agprobs2,id%in%bindate)
agprobs2$sortorder<-1:nrow(agprobs2)
agprobs2
ggplot()+geom_bar(aes(x=agprobs2$prob>0.05,fill=agprobs2$Ag.Nut.Port))+theme_minimal()+xlab("Probability Order")+ylab("")
agprobs2$
+scale_fill_gradient(name="Probability",low="#b7a57a",high="#4b2e83")+theme(text=element_text(size=24))
?scale_fill_gradient2
scale_f

library(ggthemes)
install.packages("pROC")
library(pROC)
?roc
rout<-roc(agprobs2$Ag.Nut.Port,agprobs2$prob)
ggplot()+geom_path(aes(x=1-rout$specificities,y=rout$sensitivities))+geom_abline(aes(intercept=0,slope=1),lty=2)+theme_minimal()+xlab("1-Specificity")+ylab("Sensitivity")+theme(text=element_text(size=24))

head(agprobs2)
agprobs2$prob>seq(0,.99,.01)=

ddply(matchtable$TopTopics

misops<-data.frame("row"=which(is.na(baseinput$SentFrame$OpID)))
misops$OpID<-as.character(sapply(misops$row, function(X)
  as.character(fulset$Opportunity.ID)[which(str_detect(fulset$path,fixed(basename(as.character(baseinput$SentFrame$id)[X]))))[1]]))

baseinput$SentFrame$OpID<-as.character(baseinput$SentFrame$OpID)             
baseinput$SentFrame$OpID[misops$row]<-misops$OpID





unique(baseinput$SentFrame$OpID)

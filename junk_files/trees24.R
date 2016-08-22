class(corpEnt2)
corpEnt[[1]]
library(lubridate)
corpEntN<-corpus1[sapply(corpus1,function(x) length(content(x)))>0]
corpus1b<-corpus1[sapply(corpus1,function(x) length(content(x)))>0]
corpEntN<-lapply(names(corpEntN[[1]]$meta),function(K){meta(corpEntN,K)})
corpEntN[[1]]<-melt(sapply(1:length(corpEntN[[1]]),function(x) paste(corpEntN[[1]][[x]],collapse="")))[,1]
corpEntN[[2]]<-melt(sapply(1:length(corpEntN[[2]]), function(x) ymd_hms(corpEntN[[2]][[x]]),simplify=T))
corpEntN[[2]]<-join(data.frame("L1"=1:length(corpEntN[[1]])),corpEntN[[2]])$value
corpEntN[[3]]<-melt(sapply(1:length(corpEntN[[3]]),function(x) paste(corpEntN[[3]][[x]],collapse="")))[,1]
corpEntN[[4]]<-melt(sapply(1:length(corpEntN[[4]]),function(x) paste(corpEntN[[4]][[x]],collapse="")))[,1]
corpEntN[[5]]<-melt(sapply(1:length(corpEntN[[5]]),function(x) paste(corpEntN[[5]][[x]],collapse="")))[,1]
corpEntN[[6]]<-melt(sapply(1:length(corpEntN[[6]]),function(x) paste(corpEntN[[6]][[x]],collapse="")))[,1]
corpEntN[[7]]<-melt(sapply(1:length(corpEntN[[6]]),function(x) paste(corpEntN[[7]][[x]],collapse="")))[,1]
corpEntN<-data.frame(corpEntN)
colnames(corpEntN)<-names(meta(corpus1[[1]]))

para_token_annotator <-Annotator(function(s, a = Annotation()) {
  spans <- blankline_tokenizer(s)
  n <- length(spans)
  ## Need n consecutive ids, starting with the next "free"
  ## one:
  from <- next_id(a$id)
  Annotation(seq(from = from, length.out = n),
             rep.int("paragraph", n),
             spans$start,
             spans$end)}, list(description ="A paragraph token annotator based on blankline_tokenizer()."))
library(pbapply)
par1<-lapply(corpus1b,function(x) annotate(as.String(content(x)),para_token_annotator))
corpEntN$Orig<-1:nrow(corpEntN)
par2<-lapply(1:length(par1),function(i) {
  dfout<-join(data.frame("S1"=sapply(par1[i],function(x) as.character(as.String(content(corpus1b[[i]]))[x]),USE.NAMES=FALSE),"Orig"=i),corpEntN[i,],by="Orig")
  colnames(dfout)[1]<-"S"
  dfout})
par2<-do.call(rbind,par2)
par2$SC<-as.character(par2$S)
par2<-subset(par2,nchar(par2$SC)>=100)
allans<-pblapply(par2$S,function(X) annotate(X,sent_token_annotator))
par2<-par2[-which(sapply(allans,function(X) length(X)>0)==FALSE),]
allans<-allans[sapply(allans,function(X) length(X)>0)]
allans<-pblapply(1:nrow(par2),function(i){annotate(par2$S[i],word_token_annotator,allans[[i]])})
allans<-pblapply(1:nrow(par2),function(i){annotate(par2$S[i],list(org.annotate,pers.annotate,location.annotate),allans[[i]])})

par2$SnE<-NA
par2$ents<-NA
for(i in 1:nrow(par2)){
  entit1<-c(as.String(par2$S[i])[subset(allans[[i]], type=="entity")])
  par2$ents[i]<-paste(entit1,sep="",collapse=",")
  sn<-as.character(par2$S[i])
  if(length(entit1)>0){
    for(k in 1:length(entit1)){
      sn<-gsub(entit1[k],"",sn,fixed=TRUE)
    }
    par2$SnE[i]<-sn}}
par2$ents
library("pbapply")

allents<-sapply(par2$ents,function(x) c(unlist(strsplit(x,","))),USE.NAMES=FALSE)
allents<-unlist(allents)
top.ents<-names(sort(table(allents),decreasing=TRUE)[1:25])
entp<-lapply(top.ents, function(X) str_detect(par2$SC,fixed(X)))
names(entp)<-top.ents
entp<-do.call(cbind,entp)
processed <-textProcessor(par2$SnE,metadata=cbind(par2[,2:length(par2)],entp),sparselevel=1)
out <- prepDocuments(processed$documents,processed$vocab,processed$meta,lower.thresh=4)
colnames(out$meta)<-gsub("\\W",".",colnames(out$meta))
form1<-paste("~as.factor(Orig)",paste(colnames(out$meta)[12:ncol(out$meta)],sep="",collapse="+"),sep="+")
#st1<-stm(out$documents,out$vocab,K=0, init.type="Spectral")

st1<-stm(out$documents,out$vocab,data=out$meta, K=10,prevalence=eval(parse(text=form1)), init.type="LDA",max.em.its=20)
#st1<-stm(out$documents,out$vocab,data=out$meta, K=10,prevalence=eval(parse(text=form1)), init.type="LDA",max.em.its=100,model=st1)
tps<-max.col(st1$theta)

wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)

AnnotateVerbsByTopic<-function(MAXTOPS,WT,PROCESSED,OUT,ANNOTATELIST,SENTENCEFRAME){
pos_tag_annotator<- Maxent_POS_Tag_Annotator()
tcs<-pblapply(1:length(unique(tps)), function(k){
  toChar<-SENTENCEFRAME$SC[-PROCESSED$docs.removed][-OUT$docs.removed][which(tps==k)]
  toCharANS<-ANNOTATELIST[-PROCESSED$docs.removed] %>% .[-OUT$docs.removed] %>% .[which(tps==k)]
  KeepA<-unique(unlist(sapply(WT, function(W) which(str_detect(toChar,tolower(W))))))
  anns<-lapply(KeepA,function(i){
  ANT<-annotate(as.String(toChar[KeepA]),pos_tag_annotator,toCharANS[i][[1]])
  wans<-subset(ANT, type=="word")
  vans<-wans[which(unlist(wans$features)%in%c("VB","VBN","VBZ","VBG","VBN","VBP"))]
  vanstf<-sapply(1:length(vans), function(i2) TRUE%in%str_detect(as.character(as.String(toChar[i])[vans[i2]]),WT))
  vans2<-vans[vanstf==TRUE]
  vanstf<-do.call(rbind,lapply(1:length(vans2), function(i3){
    vsent<-subset(ANT,start<=vans2[i3]$start) %>% subset(.,end>=vans2[i3]$end) %>% subset(type=="sentence")
    #vsent<-subset(anns[[1]], start>=vsent$start) %>% subset(.,end<=vsent$end)
    #psent<-parse_annotator(as.String(toChar),vsent)
    #list(vsent,psent)
    data.frame("Sent"=ifelse(length(vsent)>0,as.character(as.String(toChar[i])[vsent]),NA),"rowid"=i)
  }))
  vanstf})})
allcs<-na.omit(melt(tcs))
allcs}

FillFolder<-function(PREPFRAME,FOLDERNAME){
library(httr)
if(dir.exists("getAlchemy")==FALSE) {dir.create("getAlchemy")}
if(dir.exists(file.path("getAlchemy",FOLDERNAME))==FALSE) {dir.create(file.path("getAlchemy",FOLDERNAME))}

for(i in 1:nrow(PREPFRAME)){
  X<-PREPFRAME$Sent[i]
  req <- POST("http://access.alchemyapi.com/calls/text/TextGetRelations", 
              body = list(apikey="6837e8ae18678cadd3c42fc55ed938b3818ce470",text=X,keywords=1,outputMode="json",disambiguate=0), encode = "form",write_disk(file.path("getAlchemy",FOLDERNAME,paste("al",i,".json",sep=""))))
  Sys.sleep(.5)
}}

ParseFolderToFrame<-function(FOLDERNAME,PREPFRAME){
rels<-lapply(1:length(list.files("getAlchemy")),function(i) fromJSON(readLines(file.path("getAlchemy",FOLDERNAME,list.files("getAlchemy/FOLDERNAME")[i])))$relations)
rels2<-rels[which(sapply(rels,function(x) length(x)>0))]
joinkey<-lapply(1:length(rels),function(i)  {
  mrow<-max(c(nrow(rels[[i]]$subject$keywords[[1]]),nrow(rels[[i]]$object$keywords[[1]]),length(rels[[i]]$action$verb$text)))
  emp<-data.frame(matrix(rep(NA,mrow*4),ncol=4))
  #lapply(1:4,function(q) emp[,q]<-as.character(emp[,q]))
  colnames(emp)<-c("subkey","obkey","verbkey","whichi")
  emp$subkey<-as.character(emp$subkey)
  emp$obkey<-as.character(emp$obkey)
  emp$verbkey<-as.character(emp$verbkey)
  emp$whichi<-as.character(emp$whichi)
  if(length(rels[[i]])>0){
    if(is.null(rels[[i]]$subject$keywords[[1]])==FALSE){
      emp$subkey[1:nrow(rels[[i]]$subject$keywords[[1]])]<-unlist(rels[[i]]$subject$keywords[[1]])
    }
    if(is.null(rels[[i]]$object$keywords[[1]])==FALSE){
      emp$obkey[1:nrow(rels[[i]]$object$keywords[[1]])]<-unlist(rels[[i]]$object$keywords[[1]])
    }
    if(is.null(rels[[i]]$action$verb$text)==FALSE){
      emp$verbkey[1:length(rels[[i]]$action$lemmatized)]<-rels[[i]]$action$verb$text
    }
    emp$whichi<-i
  }
  emp
})
joinkey<-do.call(rbind.fill,joinkey)
joinkey$subkey<-tolower(joinkey$subkey)
joinkey$obkey<-tolower(joinkey$obkey)
joinkey$verbkey<-tolower(joinkey$verbkey)
joinkey_sub<-subset(joinkey, verbkey%in%WT)
library(dplyr)
subcs<-cbind(joinkey_sub,PREPFRAME[as.numeric(list.files("getAlchemy")[as.numeric(joinkey_sub$whichi)] %>% gsub("al","",.) %>% gsub('.json',"",.,fixed=T)),])
subcs}

TopicCoreFrame<-function(MAXTOPS,PARSEFRAME){
cormatsbysub<-lapply(unique(MAXTOPS),function(X) {
  topiccs<-filter(subcs,L1==X)
  dmh<-hclust(stringdistmatrix(unique(na.omit(topiccs$subkey)),method="lcs"))
  dmh$labels<-unique(na.omit(topiccs$subkey))
  plot(dmh)
  ids1<-rect.hclust(dmh,h=mean(dmh$height))
  dmo<-hclust(stringdistmatrix(unique(na.omit(topiccs$obkey)),method="lcs"))
  dmo$labels<-unique(na.omit(topiccs$obkey))
  plot(dmo)
  ids2<-rect.hclust(dmo,h=mean(dmo$height))
  dmv<-hclust(stringdistmatrix(unique(na.omit(topiccs$verbkey)),method="lcs"))
  dmv$labels<-unique(na.omit(topiccs$verbkey))
  
  bysubject<-lapply(ids1,function(Y) topiccs[which(topiccs$subkey%in%names(Y)),])
  names(bysubject)<-sapply(ids1,function(Y) names(Y[1]))
head(bysubject)
  byobject<-lapply(ids2,function(Y) topiccs[which(topiccs$obkey%in%names(Y)),])
  names(byobject)<-sapply(ids2,function(Y) names(Y[1]))

  byverb<-lapply(unique(na.omit(topiccs$verbkey)),function(Y) joinkey[which(topiccs$verbkey==Y),])
  names(byverb)<-unique(na.omit(topiccs$verbkey))

  combine.ds<-function(bysubject,byobject,byverb){
  byobject2<-unique(melt(byobject)[,-7])
  byverb2<-unique(melt(byverb))
  bysubject2<-unique(melt(bysubject)[,-7])
  head(bysubject2)
  colnames(byverb2)[5]<-"VerbCat"
  colnames(byobject2)[7]<-"ObjCat"
  colnames(bysubject2)[7]<-"SubCat"
  merge(bysubject2[,c(4,7)],byverb2[,c(4,5)], all=T) %>% merge(byobject2[,c(4,7)],all=T)
}
  obcats<-combine.ds(bysubject,byobject,byverb)
  head(obcats)
  tst<-obcats[,c("SubCat","ObjCat")]
  ts.e<-matrix(rep(NA,length(unique(tst$ObjCat))*length(unique(tst$SubCat))),nrow=length(unique(tst$SubCat)))
  rownames(ts.e)<-unique(tst$SubCat)
  colnames(ts.e)<-unique(tst$ObjCat)
  for(i in 1:nrow(ts.e)){
    ts.e[i,]<-sapply(names(ts.e[i,]),function(K){length(which(subset(tst,tst$SubCat==rownames(ts.e)[i])$ObjCat==K))})
  }
  ts.e
  })
cormatsbysub
}








cormatsbysub[[7]]


#subs<-lapply(1:length(list.files("getAlchemy")), function(i){fromJSON(readLines(file.path("getAlchemy",list.files("getAlchemy")[i])))$relations$subject})

#subs2<-subs[which(sapply(subs,function(x) length(x)>0))]
#subkey<-lapply(subs2,function(x) x$keywords)

#obs<-lapply(1:length(list.files("getAlchemy")),function(i) fromJSON(readLines(file.path("getAlchemy",list.files("getAlchemy")[i])))$relations$object)
#obs2<-obs[which(sapply(obs,function(x) length(x)>0))]
#obskey<-lapply(obs2,function(x) x$keywords)
length(tcs[[1]])
toChar<-unique(do.call(c,toChar))
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator<- Maxent_POS_Tag_Annotator()
parse_annotator <- Parse_Annotator()
anns<-annotate(toChar,sent_token_annotator) %>% annotate(toChar,list(word_token_annotator,pos_tag_annotator),.)
wans<-subset(anns, type=="word")
vans<-wans[which(unlist(wans$features)%in%c("VB","VBN","VBZ","VBG","VBN","VBP"))]

vanstf<-sapply(1:length(vans), function(i) TRUE%in%str_detect(as.character(as.String(toChar)[vans[i]]),wd$Up.Words))
vans2<-vans[vanstf==TRUE]
vanstf<-lapply(1:length(vans2), function(i){
  vsent<-subset(anns,start<=vans2[i]$start) %>% subset(.,end>=vans2[i]$end) %>% subset(type=="sentence")
  vsent<-subset(anns, start>=vsent$start) %>% subset(.,end<=vsent$end)
  #psent<-parse_annotator(as.String(toChar),vsent)
  #list(vsent,psent)
  vsent
})
vanstf<-unique(vanstf)


length(vans2)

vanstf<-lapply(1:length(vans2), function(i){
  vsent<-subset(anns,start<=vans2[i]$start) %>% subset(.,end>=vans2[i]$end) %>% subset(type=="sentence")
  vsent<-subset(anns, start>=vsent$start) %>% subset(.,end<=vsent$end)
  #psent<-parse_annotator(as.String(toChar),vsent)
  #list(vsent,psent)
  vsent
})

i=33 stopped
for(i in 1:length(vanstf)){
  X<-as.String(toChar)[vanstf[[i]][1]]
  req <- POST("http://access.alchemyapi.com/calls/text/TextGetRelations", 
              body = list(apikey="6837e8ae18678cadd3c42fc55ed938b3818ce470",text=X,keywords=1,outputMode="json",disambiguate=0), encode = "form",write_disk(file.path("getAlchemy",vanstf[[i]][1]$id)))
  Sys.sleep(.5)
}




library(jsonlite)
subs<-lapply(1:length(list.files("getAlchemy")),function(i) fromJSON(readLines(file.path("getAlchemy",list.files("getAlchemy")[i])))$relations$subject)
subkey<-lapply(subs,function(x) x$keywords)

obs<-lapply(1:length(list.files("getAlchemy")),function(i) fromJSON(readLines(file.path("getAlchemy",list.files("getAlchemy")[i])))$relations$object)
obskey<-lapply(obs,function(x) x$keywords)
rels<-lapply(1:length(list.files("getAlchemy")),function(i) fromJSON(readLines(file.path("getAlchemy",list.files("getAlchemy")[i])))$relations)
joinkey<-lapply(1:length(rels),function(i)  {
  mrow<-max(c(nrow(rels[[i]]$subject$keywords[[1]]),nrow(rels[[i]]$object$keywords[[1]]),length(rels[[i]]$action$verb$text)))
  emp<-data.frame(matrix(rep(NA,mrow*4),ncol=4))
  #lapply(1:4,function(q) emp[,q]<-as.character(emp[,q]))
  colnames(emp)<-c("subkey","obkey","verbkey","whichi")
  emp$subkey<-as.character(emp$subkey)
  emp$obkey<-as.character(emp$obkey)
  emp$verbkey<-as.character(emp$verbkey)
  emp$whichi<-as.character(emp$whichi)
  if(length(rels[[i]])>0){
    if(is.null(rels[[i]]$subject$keywords[[1]])==FALSE){
      emp$subkey[1:nrow(rels[[i]]$subject$keywords[[1]])]<-unlist(rels[[i]]$subject$keywords[[1]])
    }
    if(is.null(rels[[i]]$object$keywords[[1]])==FALSE){
      emp$obkey[1:nrow(rels[[i]]$object$keywords[[1]])]<-unlist(rels[[i]]$object$keywords[[1]])
    }
    if(is.null(rels[[i]]$action$verb$text)==FALSE){
      emp$verbkey[1:length(rels[[i]]$action$lemmatized)]<-rels[[i]]$action$verb$text
    }
    emp$whichi<-i
  }
  emp
})
??rbind.fill
library(plyr)
joinkey<-do.call(rbind.fill,joinkey)
joinkey$subkey<-tolower(joinkey$subkey)
joinkey$obkey<-tolower(joinkey$obkey)
joinkey$verbkey<-tolower(joinkey$verbkey)



library(stringdist)


dmh<-hclust(stringdistmatrix(unique(na.omit(joinkey$subkey)),method="lcs"))
dmh$labels<-unique(na.omit(joinkey$subkey))
plot(dmh)
ids1<-rect.hclust(dmh,mean(dmh$height))

dmo<-hclust(stringdistmatrix(unique(na.omit(joinkey$obkey)),method="lcs"))
dmo$labels<-unique(na.omit(joinkey$obkey))
plot(dmo)
ids2<-rect.hclust(dmo,mean(dmo$height))
dmsub<-hclust(stringdistmatrix(unique(na.omit(joinkey$verbkey)),method="lcs"))
dmsub$labels<-unique(na.omit(joinkey$verbkey))

plot(dmsub)
install.packages("psych")
library(psych)
polychroric()

bysubject<-lapply(ids1,function(X) joinkey[which(joinkey$subkey%in%names(X)),])
names(bysubject)<-sapply(ids1,function(X) names(X[1]))

byobject<-lapply(ids2,function(X) joinkey[which(joinkey$obkey%in%names(X)),])
names(byobject)<-sapply(ids2,function(X) names(X[1]))

byverb<-lapply(unique(na.omit(joinkey$verbkey)),function(X) joinkey[which(joinkey$verbkey==X),])
names(byverb)<-unique(na.omit(joinkey$verbkey))

combine.ds<-function(bysubject,byobject,byverb){
  byobject<-melt(byobject)
  byverb<-melt(byverb)
  bysubject<-melt(bysubject)
  colnames(byverb)[5]<-"VerbCat"
  colnames(byobject)[5]<-"ObjCat"
  colnames(bysubject)[5]<-"SubCat"
  join(bysubject[,c(-4)],byverb[,c(-4)]) %>% join(byobject[,c(-4)])
}
obcats<-combine.ds(bysubject,byobject,byverb)

tst<-obcats[,c("SubCat","ObjCat")]
ts.e<-matrix(rep(NA,length(unique(tst$ObjCat))*length(unique(tst$SubCat))),nrow=length(unique(tst$SubCat)))
rownames(ts.e)<-unique(tst$SubCat)
colnames(ts.e)<-unique(tst$ObjCat)
ts.e  
for(i in 1:nrow(ts.e)){
  ts.e[i,]<-sapply(names(ts.e[i,]),function(K){length(which(subset(tst,tst$SubCat==rownames(ts.e)[i])$ObjCat==K))})
}

ggplot(melt(ts.e))+geom_tile(aes(x=Var1,y=Var2,fill=value,alpha=value))+scale_fill_continuous_tableau(palette='Green')+theme_minimal()+theme(axis.text.x=element_text(angle=90))+ggtitle('Increased Causal Relations')+xlab("Subject")+ylab("Object")+coord_flip()
?scale_fill_continuous
d3heatmap(ts.e, scale="column",colors="Blues",Rowv=FALSE,Colv=FALSE,xaxis_font_size=8)




library(tm)
library("quanteda")
t(tf(quanteda::as.dfm(ts.e),"boolean")) %*% as.dfm(ts.e)

cor(ts.e)
?polychoric
data.frame("word"=na.omit(unique(joinkey$subkey)),"cluster"=melt(ids1) %>% .$L1)
melt(ids1) %>% .$L1
polychor(melt(ids2) %>% .$L1
         head(obcats)
         
         obcats$ObjCat
         obcats$VerbCat
         na.omit(obcats) %>% polychoric(.$SubCat,.$ObjCat)
         ?polychoric
         library(DT)
         tablemake<-function(verb.parselist, relations,class.word="Verb"){
           verb.parselist<-outlist
           class.word="Verb"
           relations=rels
           tabwhich<-if(class.word=="Verb"){verb.parselist$byverb} else {if(class.word=="Subject") {verb.parselist$bysubject} else {verb.parselist$byobject}}
           tabwhich<-lapply(tabwhich,function(X){
             for(k in 1:length(X$whichi)){
               X$whichi[k]<-relations[[as.numeric(X$whichi[k])]]$sentence[1]
             }
             colnames(X)=c("Subject","Object","Verb","Sentence")
             X})
           DT::datatable(do.call(rbind, tabwhich),filter="top")
         }
         
         tabwhich
         tablemake(outlist,rels,class.word="Verb")
         
         DT::datatable(outlist,filter="top")
         ?datatable
         
         rels[[1]]$sentence[1]
         
         
         
         
         
         cons<-lapply(1:length(joinkey$subkey), function(i){
           i=3
           k<-POST("http://access.alchemyapi.com/calls/text/TextGetRankedConcepts", 
                   body = list(apikey="6837e8ae18678cadd3c42fc55ed938b3818ce470",text=I(unique(na.omit(joinkey$subkey))[i]),knowledgeGraph=1,outputMode="json"), encode = "form")
         })
         
         cons
         ?POST
         dm<-matrix(dm, ncol=nrow(joinkey))
         dm[3,]
         ?kmeans
         ?dist
         align_local(joinkey$subkey[1],joinkey$subkey[2])
         ?TextReuseCorpus
         lapply(obs[[6]]$text,function(X) which(st1$vocab==wordStem(X)))
         install.packages("stringdist")
         posterior(st1,obs[[1]]$text, ...)
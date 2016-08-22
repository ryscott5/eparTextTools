#Annotate Paragraph Tokens
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
par1<-lapply(corpEntS,function(x) annotate(x,para_token_annotator))
par1<-do.call(rbind,lapply(1:length(corpEntS),function(i) do.call(rbind, lapply(1:length(par1[[i]]),function(k) merge(data.frame("S"=as.String(corpEntS[i])[par1[[i]]][[k]],"Orig"=i),corpEnt3[i,])))))

par1$S
processed <-textProcessor(par1$S,metadata=par1[,2:length(par1)],sparselevel=1,)

missingINF<-unlist(unique(sapply(colnames(corpEnt3),function(x){which(is.na((corpEnt3[,x])))}),simplify=T))

out <- prepDocuments(processed$documents,processed$vocab,processed$meta,lower.thresh=4)
head(out$meta)

#st1<-stm(out$documents,out$vocab,K=0, init.type="Spectral")
st1<-stm(out$documents,out$vocab,data=out$meta, K=10,prevalence=~as.factor(Orig), init.type="LDA",max.em.its=5)

tps<-max.col(st1$theta)

wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)


for(k in tps){
  toChar<-as.character(par1$S[-processed$docs.removed])[-out$docs.removed][which(tps==k)]
}

toChar<-lapply(wd$Up.Words, function(W) toChar[str_detect(toChar,tolower(W))])
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
length(vans2)

vanstf<-lapply(1:length(vans2), function(i){
  vsent<-subset(anns,start<=vans2[i]$start) %>% subset(.,end>=vans2[i]$end) %>% subset(type=="sentence")
  vsent<-subset(anns, start>=vsent$start) %>% subset(.,end<=vsent$end)
  #psent<-parse_annotator(as.String(toChar),vsent)
  #list(vsent,psent)
  vsent
})

vanstf<-unique(vanstf)
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
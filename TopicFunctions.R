#TopicFunctions
library(lubridate)

PreTopicFrame<-function(CORPUS_A,howmanyentities=25){
  corpEntN<-CORPUS_A[sapply(CORPUS_A,function(x) length(content(x)))>0]
  corpus1b<-CORPUS_A[sapply(CORPUS_A,function(x) length(content(x)))>0]
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
  corpEntN
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
  
  allents<-sapply(par2$ents,function(x) c(unlist(strsplit(x,","))),USE.NAMES=FALSE)
  allents<-unlist(allents)
  top.ents<-names(sort(table(allents),decreasing=TRUE)[1:howmanyentities])
  entp<-lapply(top.ents, function(X) str_detect(par2$SC,fixed(X)))
  names(entp)<-top.ents
  entp<-do.call(cbind,entp)
  processed <-textProcessor(par2$SnE,metadata=cbind(par2[,2:length(par2)],entp),sparselevel=1)
  out <- prepDocuments(processed$documents,processed$vocab,processed$meta,lower.thresh=4)
  colnames(out$meta)<-gsub("\\W",".",colnames(out$meta))
  list("SentFrame"=par2,"Annotations"=allans,"processed"=processed,"out"=out)
}


AnnotateVerbsByTopic<-function(MAXTOPS,WT,PROCESSED,OUT,ANNOTATELIST,SENTENCEFRAME){
  pos_tag_annotator<- Maxent_POS_Tag_Annotator()
  tcs<-pblapply(1:length(unique(MAXTOPS)), function(k){
    toChar<-SENTENCEFRAME$SC[-PROCESSED$docs.removed][-OUT$docs.removed][which(MAXTOPS==k)]
    toCharANS<-ANNOTATELIST[-PROCESSED$docs.removed] %>% .[-OUT$docs.removed] %>% .[which(MAXTOPS==k)]
    KeepA<-unique(unlist(sapply(WT, function(W) which(str_detect(toChar,tolower(W))))))
    anns<-lapply(KeepA,function(i){
      ANT<-annotate(as.String(toChar[i]),pos_tag_annotator,toCharANS[i][[1]])
      wans<-subset(ANT, type=="word")
      vans<-wans[which(unlist(wans$features)%in%c("VB","VBN","VBZ","VBG","VBN","VBP"))]
      vanstf<-sapply(1:length(vans), function(i2) TRUE%in%str_detect(as.character(as.String(toChar[i])[vans[i2]]),WT))
      vans2<-vans[vanstf==TRUE]
      vanstf<-do.call(rbind,lapply(1:length(vans2), function(i3){
        vsent<-subset(ANT,start<=vans2[i3]$start) %>% subset(.,end>=vans2[i3]$end) %>% subset(type=="sentence")
        #vsent<-subset(anns[[1]], start>=vsent$start) %>% subset(.,end<=vsent$end)
        #psent<-parse_annotator(as.String(toChar),vsent)
        #list(vsent,psent)
        data.frame("Sent"=ifelse(length(vsent)>0,as.character(as.String(toChar[i])[vsent]),NA),"rowid"=which(SENTENCEFRAME[-PROCESSED$docs.removed,][-OUT$docs.removed,][which(MAXTOPS==k),][i,]$SC==SENTENCEFRAME[-PROCESSED$docs.removed,][-OUT$docs.removed,]$SC))
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

ParseFolderToFrame<-function(FOLDERNAME,PREPFRAME,WT){
  rels<-lapply(1:length(list.files(file.path("getAlchemy",FOLDERNAME))),function(i) fromJSON(readLines(file.path("getAlchemy",FOLDERNAME,list.files(file.path("getAlchemy",FOLDERNAME))[i])))$relations)
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
  subcs<-cbind(joinkey_sub,PREPFRAME[as.numeric(list.files(file.path("getAlchemy",FOLDERNAME))[as.numeric(joinkey_sub$whichi)] %>% gsub("al","",.) %>% gsub('.json',"",.,fixed=T)),])
  subcs_full<-cbind(joinkey,PREPFRAME[as.numeric(list.files(file.path("getAlchemy",FOLDERNAME))[as.numeric(joinkey$whichi)] %>% gsub("al","",.) %>% gsub('.json',"",.,fixed=T)),])
  list("SmallVerb"=subcs,"FullVerb"=subcs_full)}
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

library(DT)

frametable<-function(PARSEFRAME,BASEINPUT,FOLDERNAME,PREPFRAME,TOPICMOD){
  # PARSEFRAME<-Frame1[[1]]
  # rm(PARSEFRAME)
  # BASEINPUT<-BASE_INPUT
  # rm(BASEINPUT)
  # FOLDERNAME<-"test2"
  # rm(FOLDERNAME)
  # PREPFRAME<-Pf1
  # rm(PREPFRAME)
  # TOPICMOD<-st1
  # rm(TOPICMOD)
  OV<-PREPFRAME[as.numeric(list.files(file.path("getAlchemy",FOLDERNAME)) %>% gsub(".json","",.) %>% gsub("al","",.) %>% .[as.numeric(PARSEFRAME$whichi)]),]
  OVF<-BASEINPUT$SentFrame[-BASEINPUT$processed$docs.removed,][-BASEINPUT$out$docs.removed,][OV$value,] 
  PARSEFRAME<-cbind(PARSEFRAME,OVF[,c("author","datetimestamp","id","ents")])
  colnames(PARSEFRAME)
  PARSEFRAME<-PARSEFRAME[,c("subkey","obkey","verbkey","Sent","L1","author","datetimestamp",'id',"ents")]
  colnames(PARSEFRAME)<-c('Subject','Object',"Verb","Sentence","Topic","Author","Date","Document","Entities")
  PARSEFRAME$Topic<-sapply(1:TOPICMOD$settings$dim$K,function(i) paste(labelTopics(TOPICMOD,n=5)[[1]][i,],collapse=","))[PARSEFRAME$Topic]
  PARSEFRAME$Topic<-as.factor(PARSEFRAME$Topic)
  PARSEFRAME$Verb<-as.factor(PARSEFRAME$Verb)
  PARSEFRAME}

frametable.html<-function(PARSEFRAME,BASEINPUT,FOLDERNAME,PREPFRAME,TOPICMOD){
  OV<-PREPFRAME[as.numeric(list.files(file.path("getAlchemy",FOLDERNAME)) %>% gsub(".json","",.) %>% gsub("al","",.) %>% .[as.numeric(PARSEFRAME$whichi)]),]
  OVF<-BASEINPUT$SentFrame[-BASEINPUT$processed$docs.removed,][-BASEINPUT$out$docs.removed,][OV$value,] 
  PARSEFRAME<-cbind(PARSEFRAME,OVF[,c("author","datetimestamp","id","ents")])
  PARSEFRAME<-PARSEFRAME[,c(1,2,3,5,7,8,9,10,11)]
  colnames(PARSEFRAME)<-c('Subject Keyword','Object Keyword',"Verb","Sentence","Topic","Author","Date and Time","Document","Entities")
  PARSEFRAME$Topic<-sapply(1:TOPICMOD$settings$dim$K,function(i) paste(labelTopics(TOPICMOD,n=5)[[1]][i,],collapse=","))[PARSEFRAME$Topic]
  PARSEFRAME$Topic<-as.factor(PARSEFRAME$Topic)
  PARSEFRAME$Verb<-as.factor(PARSEFRAME$Verb)
  datatable(data=PARSEFRAME,rownames=FALSE,filter="top")}

TopicCoreFrame<-function(MAXTOPS,PARSEFRAME){
  library(stringdist)
  cormatsbysub<-lapply(unique(MAXTOPS),function(X) {
    topiccs<-filter(PARSEFRAME,L1==X)
    if(nrow(topiccs)>0){
    dmh<-hclust(stringdistmatrix(unique(na.omit(topiccs$subkey)),method="lcs"))
    dmh$labels<-unique(na.omit(topiccs$subkey))
    ids1<-rect.hclust(dmh,h=mean(dmh$height))
    dmo<-hclust(stringdistmatrix(unique(na.omit(topiccs$obkey)),method="lcs"))
    dmo$labels<-unique(na.omit(topiccs$obkey))
    ids2<-rect.hclust(dmo,h=min(dmo$height))
    dmv<-hclust(stringdistmatrix(unique(na.omit(topiccs$verbkey)),method="lcs"))
    dmv$labels<-unique(na.omit(topiccs$verbkey))
    
    bysubject<-lapply(ids1,function(Y) topiccs[which(topiccs$subkey%in%names(Y)),])
    names(bysubject)<-sapply(ids1,function(Y) names(Y[1]))
    head(bysubject)
    byobject<-lapply(ids2,function(Y) topiccs[which(topiccs$obkey%in%names(Y)),])
    names(byobject)<-sapply(ids2,function(Y) names(Y[1]))
    
    byverb<-lapply(unique(na.omit(topiccs$verbkey)),function(Y) PARSEFRAME[which(topiccs$verbkey==Y),])
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
    ts.e} else {matrix(NA)}
  })
  cormatsbysub
}



SimpFrameList<-function(TCOREFRAME){
  lapply(TCOREFRAME,function(X){t1<-melt(as.matrix(X)) %>% filter(value>0)
  colnames(t1)<-c("Subject","Object","Count")
  t1
  })}



clusterWords<-function(COLUMN,NUM,TOPICMODEL){
  subprobs<-do.call(rbind.fill,lapply(unique(COLUMN), function(X){
    k<-data.frame(t(exp(TOPICMODEL$beta$logbeta[[1]][,which(TOPICMODEL$vocab==tm::stemDocument(tolower(X)))])))
    if(nrow(k)>0){
      k$word<-X
    }
    k
  }))
  row.names(subprobs)<-subprobs$word
  subprobs<-subprobs[,1:10]
  d1<-kmeans(subprobs,NUM)
  optName<-sapply(1:NUM, function(k) {names(d1$cluster[d1$cluster==k][which.min(sapply(1:length(which(d1$cluster==k)),function(i) sum(abs(subprobs[which(d1$cluster==k),]-fitted(d1)[which(d1$cluster==k),])[i,])))])})
  data.frame("Word"=row.names(subprobs),"optName"=optName[d1$cluster])
}


idSimilar<-function(SEARCH,COLUMN,NUM,TOPICMODEL){
  subprobs<-do.call(rbind.fill,lapply(unique(COLUMN), function(X){
    k<-data.frame(t(exp(TOPICMODEL$beta$logbeta[[1]][,which(TOPICMODEL$vocab==tm::stemDocument(tolower(X)))])))
    if(nrow(k)>0){
      k$word<-X
    }
    k
  }))
  sid<-which(subprobs$word==SEARCH)
  row.names(subprobs)<-subprobs$word
  subprobs<-subprobs[,1:10]
  d1<-kmeans(subprobs,NUM)
  names(d1$cluster[d1$cluster==d1$cluster[sid]])
}

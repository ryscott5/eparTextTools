#TopicFunctions
library(lubridate)
jgc <- function()
{
  rJava::.jcall("java/lang/System", method = "gc")
}    

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
  colnames(corpEntN)<-names(meta(CORPUS_A[[1]]))
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
  par1<-lapply(corpus1b,function(x) annotate(as.String(content(x)),para_token_annotator))
  corpEntN$Orig<-1:nrow(corpEntN)
  par2<-lapply(1:length(par1),function(i) {
    dfout<-join(data.frame("S1"=sapply(par1[i],function(x) as.character(as.String(content(corpus1b[[i]]))[x]),USE.NAMES=FALSE),"Orig"=i),corpEntN[i,],by="Orig")
    colnames(dfout)[1]<-"S"
    dfout})
  par2<-do.call(rbind,par2)
  par2$SC<-as.character(par2$S)
  par2<-par2[nchar(par2$SC)>=100,]
  allans<-pblapply(par2$S,function(X) annotate(X,sent_token_annotator))
  par2<-par2[which(sapply(allans,function(X) length(X)>0)==TRUE),]
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

jgc()

AnnotateVerbsTopicJoin<-function(WT,PROCESSED,OUT,ANNOTATELIST,SENTENCEFRAME,toptopics){
  pos_tag_annotator<- Maxent_POS_Tag_Annotator()
  #OUT<-BASE_INPUT$out
  #ANNOTATELIST<-BASE_INPUT$Annotations
  #PROCESSED<-BASE_INPUT$processed
  #WT<-allwords
    toChar<-SENTENCEFRAME$S[-PROCESSED$docs.removed] %>% .[-OUT$docs.removed]
    toCharANS<-ANNOTATELIST[-PROCESSED$docs.removed] %>% .[-OUT$docs.removed]
    KeepA<-unique(unlist(sapply(WT, function(W) which(str_detect(toChar,tolower(W))))))
      anns<-lapply(KeepA,function(i){
      ANT<-annotate(as.String(toChar[i]),pos_tag_annotator,toCharANS[i][[1]])
      wans<-subset(ANT, type=="word")
      vans<-wans[which(unlist(wans$features)%in%c("VB","VBN","VBZ","VBG","VBN","VBP"))]
      vanstf<-sapply(1:length(vans), function(i2){TRUE%in%str_detect(as.character(as.String(toChar[i])[vans[i2]]),WT)})
      vans2<-vans[vanstf==TRUE]
      vanstf<-do.call(rbind,lapply(1:length(vans2), function(i3){
        vsent<-subset(ANT,start<=vans2[i3]$start) %>% subset(.,end>=vans2[i3]$end) %>% subset(type=="sentence")
        #vsent<-subset(anns[[1]], start>=vsent$start) %>% subset(.,end<=vsent$end)
        #psent<-parse_annotator(as.String(toChar),vsent)
        #list(vsent,psent)
        as.String(toChar[i])[vsent]
        data.frame("Sent"=ifelse(length(vsent)>0,as.character(as.String(toChar[i])[vsent]),NA),"rowid"=i)
      }))
      vanstf})
    anf<-do.call(rbind,anns)  
    anf<-cbind(anf,OUT$meta[anf$rowid,])
    anf$top.topics<-toptopics[anf$rowid]
    anf<-subset(anf, is.na(anf$Sent)==FALSE)
    anf<-unique(anf)
    anf}

CombinationFrame<-function(PREPFRAME){
  ddply(.data=PREPFRAME, .(Orig,rowid),summarize,"Sent"=paste(as.character(Sent),collapse="\n",sep="\n"))
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
      droppedLs<-which(vanstf==FALSE)
      vans2<-vans[vanstf==TRUE]
      vanstf<-do.call(rbind,lapply(1:length(vans2), function(i3){
        vsent<-subset(ANT,start<=vans2[i3]$start) %>% subset(.,end>=vans2[i3]$end) %>% subset(type=="sentence")
        #vsent<-subset(anns[[1]], start>=vsent$start) %>% subset(.,end<=vsent$end)
        #psent<-parse_annotator(as.String(toChar),vsent)
        #list(vsent,psent)
        data.frame("Sent"=ifelse(length(vsent)>0,as.character(as.String(toChar[i])[vsent]),NA),"rowid"=which(SENTENCEFRAME[-PROCESSED$docs.removed,][-OUT$docs.removed,][which(MAXTOPS==k),][i,]$SC==SENTENCEFRAME[-PROCESSED$docs.removed,][-OUT$docs.removed,]$SC))
      }))
      vanstf})})
  names(tcs)<-c(1:MAXTOPS)[-droppedLs]
  droppedLs2<-which(sapply(tcs,length)<1)
  tcs2<-tcs[sapply(tcs, length)>0]
  allcs<-na.omit(melt(tcs2))
  allcs}

ProcessforAPI<-function(PREPFRAME){
PREPFRAME$Sent<-str_trim(PREPFRAME$Sent)
PREPFRAME$Sent<-gsub("\n"," ",PREPFRAME$Sent)
badcs<-paste(gsub("\\w|\\s|\\.|\\,","",PREPFRAME$Sent),collapse="")
badcs<-unique(unlist(strsplit(badcs,split="")))
for(i in 1:length(badcs)){
  PREPFRAME$Sent<-gsub(badcs[i]," ",PREPFRAME$Sent,fixed=T)
}
PREPFRAME$Sent<-gsub("  "," ",PREPFRAME$Sent)
PREPFRAME}


idtopics<-function(STMOBJ,TERM,N){
  #TERM<-"cassava"
  #STMOBJ<-st1
  ptops<-findTopic(STMOBJ,list(TERM),n=N,type="prob")
  if(length(ptops)>1){
  ptopsl<-sapply(1:length(ptops), function(i) paste(sageLabels(STMOBJ,n=N)$marginal$prob[ptops,][i,],collapse=" "))
  mselect<-ptops[which(ptopsl%in%select.list(ptopsl,multiple=T))]
  } else {mselect<-ptops}
  cat(paste("\nmodel",mselect,"selected",sep=" ",collapse=","))
  return(mselect)
}


picktopics<-function(STMOBJ,TERM,N){
  test<-idtopics(STMOBJ,TERM,N)
  termlist<-TERM
  if(menu(c("yes","no"),title="\nCheck For Similar Words?")==1){
    terms <- getIndexTerms("NOUN", 1, getTermFilter("ExactMatchFilter", TERM, TRUE))
    newterms<-getSynonyms(terms[[1]]) %>% select.list(.,multiple=TRUE)
    termlist<-c(termlist,newterms)
    test<-append(test,sapply(newterms,function(X) tryCatch({idtopics(STMOBJ,X,N)},error=function(e){NA})))
    test<-na.omit(test)
  }
  cat("\nYou are using topics\n",test,"\nbased on selection of",termlist,sep=" ",collapse=" ")
  list("searchterms"=termlist,"topics"=test)
  }
  

data_mapper<<-function(CountryPredictions,OPPORTUNITY){
  gchars<-ddply(CountryPredictions,.(OpID),summarise,"charsum"=sum(nchars))
  CountryPredictions<-plyr::join(CountryPredictions,gchars)
  CountryPredictions$weight<-CountryPredictions$nchars/CountryPredictions$charsum
  CountryPredictions<-cbind(CountryPredictions[,1:2],CountryPredictions[,3:c(ncol(CountryPredictions)-3)]*CountryPredictions$weight)
  ftemp<-dplyr::filter(CountryPredictions, OpID%in%OPPORTUNITY)
  tframe<-reshape2::melt(colSums(ftemp[,3:ncol(ftemp)],na.rm=TRUE)/sum(ftemp[,3:ncol(ftemp)],na.rm=TRUE))
  tframe$countryids<-row.names(tframe)
  tframe$ccode2<-plyr::join(data.frame("ISO3166.1.Alpha.2"=tframe$countryids),ccodes)$ISO3166.1.Alpha.3
  tframe$nameC<-plyr::join(data.frame("ISO3166.1.Alpha.2"=tframe$countryids),ccodes)$official_name_en
  tframe$hover<-paste(tframe$nameC,": ",round(tframe$value*100)/100,sep="")
  tframe}


FillFolderMan<-function(PREPFRAME,FOLDERNAME){
  library(httr)
  #if(dir.exists("getAlchemy")==FALSE) {dir.create("getAlchemy")}
  #if(dir.exists(file.path(FOLDERNAME,"ALCHEMY"))==FALSE) {dir.create(file.path(FOLDERNAME,"ALCHEMY"))}
  for(i in 1:nrow(PREPFRAME)){
    X<-PREPFRAME$Sent[i]
    req <- POST("http://access.alchemyapi.com/calls/text/TextGetRelations", 
                body = list(apikey="6837e8ae18678cadd3c42fc55ed938b3818ce470",text=X,keywords=1,outputMode="json",disambiguate=0), encode = "form",write_disk(file.path("getAlchemy",FOLDERNAME,paste("al",i,".json",sep=""))))
    Sys.sleep(.5)
  }}

findBroken<-function(FOLDERNAME){
flist<-list.files(file.path(FOLDERNAME,"ALCHEMY"))
bad<-sapply(flist,function(X){
tfile<-readLines(file.path("getAlchemy",FOLDERNAME,X))
p1<-str_detect(paste(tfile,collapse=" ",sep=" "),"If you are seeing this message, you are likely making an excessive number of concurrent HTTP connections to this service.  Please check the concurrency limits for your assigned service tier.")
a<-tryCatch(jsonlite::fromJSON(tfile), error=function(e){"error"})
p2<-a=="error"
TRUE%in%c(p1,p2)
})        
flist[bad]
}

FillFolder<-function(PREPFRAME,FOLDERNAME){
  library(httr)
  if(dir.exists(file.path(FOLDERNAME,"ALCHEMY"))==FALSE) {dir.create(file.path(FOLDERNAME,"ALCHEMY"))}
  for(i in args[1]:nrow(PREPFRAME)){
    X<-PREPFRAME$Sent[i]
    req <- POST("http://access.alchemyapi.com/calls/text/TextGetRelations", 
                body = list(apikey=ALKEY,text=X,keywords=1,outputMode="json",disambiguate=0), encode = "form",write_disk(file.path("getAlchemy",FOLDERNAME,paste("al",i,".txt",sep=""))))
    Sys.sleep(1)
  }}



FillRetry<-function(PREPFRAME,FOLDERNAME,FILENAMES){
  for(X in FILENAMES){
    Num1<-gsub(".json","",X) %>% gsub("al","",.) %>% as.numeric()
    Sent<-PREPFRAME$Sent[Num1]
    file.remove(file.path("getAlchemy",FOLDERNAME,X))
    req <- POST("http://access.alchemyapi.com/calls/text/TextGetRelations", 
                body = list(apikey="6837e8ae18678cadd3c42fc55ed938b3818ce470",text=Sent,keywords=1,outputMode="json",disambiguate=0), encode = "form",write_disk(file.path(FOLDERNAME,"ALCHEMY",X)))
    Sys.sleep(.5)
  }}
FillRetry<-function(PREPFRAME,FOLDERNAME,FILENAMES){
  library(httr)
  for(X in FILENAMES){
    Num1<-gsub(".json","",X) %>% gsub("al","",.) %>% as.numeric()
    Sent<-PREPFRAME$Sent[Num1]
    file.remove(file.path("getAlchemy",FOLDERNAME,X))
    req <- POST("http://access.alchemyapi.com/calls/text/TextGetRelations", 
                body = list(apikey="6837e8ae18678cadd3c42fc55ed938b3818ce470",text=Sent,keywords=1,outputMode="json",disambiguate=0), encode = "form",write_disk(file.path(FOLDERNAME,"ALCHEMY",X)))
    Sys.sleep(.5)
    
  }}

processFolder<-function(working.filepath, PREPFRAME){
  tempfname<-list.files(file.path(working.filepath,"ALCHEMY"),full.names=T)
  nosize<-sapply(tempfname, function(X) file.info(X)$size==0)
  templist<-pblapply(tempfname[which(nosize==FALSE)], function(X){fromJSON(X,flatten=T)$relations %>% as.data.frame() %>% mutate(.,"filename"=as.character(X))})
  tempframe<-rbind.pages(templist)
  tempframe$comboID=str_extract(tempframe$filename,"[0-9]+")
  PREPFRAME$comboID<-1:nrow(PREPFRAME)
  recombineOut<-plyr::join(tempframe,PREPFRAME, by="comboID",type="left",match="all")
  return(recombineOut)
}

ParseFolderToFrame<-function(FOLDERNAME,PREPFRAME,WT){
  rels<-lapply(1:length(list.files(file.path(FOLDERNAME,"ALCHEMY"))),function(i) fromJSON(readLines(file.path(FOLDERNAME,"ALCHEMY",list.files(file.path(FOLDERNAME,"ALCHEMY"))[i])))$relations)
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
  subcs<-cbind(joinkey_sub,PREPFRAME[as.numeric(list.files(file.path(FOLDERNAME,"ALCHEMY"))[as.numeric(joinkey_sub$whichi)] %>% gsub("al","",.) %>% gsub('.json',"",.,fixed=T)),])
  subcs_full<-cbind(joinkey,PREPFRAME[as.numeric(list.files(file.path(FOLDERNAME,"ALCHEMY"))[as.numeric(joinkey$whichi)] %>% gsub("al","",.) %>% gsub('.json',"",.,fixed=T)),])
  list("SmallVerb"=subcs,"FullVerb"=subcs_full)}

frametable<-function(PARSEFRAME,BASEINPUT,FOLDERNAME,PREPFRAME){
  OV<-PREPFRAME[as.numeric(list.files(file.path(FOLDERNAME,"ALCHEMY")) %>% gsub(".json","",.) %>% gsub("al","",.) %>% .[as.numeric(PARSEFRAME$whichi)]),]
  OVF<-BASEINPUT$SentFrame[-BASEINPUT$processed$docs.removed,][-BASEINPUT$out$docs.removed,][OV$value,] 
  PARSEFRAME<-cbind(PARSEFRAME,OVF[,c("author","datetimestamp","id","ents")])
  PARSEFRAME<-PARSEFRAME[,c(1,2,3,5,7,8,9,10,11)]
  colnames(PARSEFRAME)<-c('Subject','Object',"Verb","Sentence","Topic","Author","Date","Document","Entities")
  PARSEFRAME$Topic<-sapply(1:10,function(i) paste(labelTopics(st1,n=5)[[1]][i,],collapse=","))[PARSEFRAME$Topic]
  PARSEFRAME$Topic<-as.factor(PARSEFRAME$Topic)
  PARSEFRAME$Verb<-as.factor(PARSEFRAME$Verb)
  PARSEFRAME}

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
  OV<-PREPFRAME[as.numeric(list.files(file.path(FOLDERNAME,"ALCHEMY")) %>% gsub(".json","",.) %>% gsub("al","",.) %>% .[as.numeric(PARSEFRAME$whichi)]),]
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
  OV<-PREPFRAME[as.numeric(list.files(file.path(FOLDERNAME,"ALCHEMY")) %>% gsub(".json","",.) %>% gsub("al","",.) %>% .[as.numeric(PARSEFRAME$whichi)]),]
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

nearest_to2<-function(topicm,wordvec,n=10,fixword=FALSE,limitwords=NULL){ 
  mt<-t(exp(topicm$beta$logbeta[[1]]))
  row.names(mt)<-topicm$vocab
  if(fixword==FALSE){
    whichwords<-unique(unlist(sapply(wordvec,function(X){which(stringr::str_detect(row.names(mt),X))})))} else
    {whichwords<-which(row.names(mt)%in%wordvec)}
  if(length(limitwords)>0){
    whichwords<-whichwords[limitwords]
  }
  sims = cosineSimilarity(mt, matrix(as.vector(mt[whichwords,]), ncol = ncol(mt)))
  ords = order(-sims[, 1])
  list("searchwords"=row.names(mt)[whichwords],"simwords"=structure(1 - sims[ords[1:n]], names = rownames(sims)[ords[1:n]]))
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


#nex<-read.csv("../nonExcel.csv",stringsAsFactors=FALSE)
#nexjoin<-plyr::join(data.frame("name"=BASE_INPUT$SentFrame$id),data.frame("name"=basename(as.character(nex$path)),"OpID"=as.character(nex$Opportunity.ID)),type="left",match="first")
#BASE_INPUT$SentFrame$OpID<-nexjoin$OpID
library(plyr)
#for these commands, we need to have the opportunity id labeles as OpID in the SentFrame part of BASE_INPUT
buildcliff<-function() {system('sudo docker run -p "8080:8080" -d --name cliff cliff:2.1.1')}
startcliff<-function() {system('sudo docker start cliff')}
checkcliff<-function(){system('sudo docker ps')}
stopcliff<-function(){system('sudo docker stop cliff')}

PredictCountryByDoc<-function(BASE_INPUT){
  fullc<-ddply(BASE_INPUT$SentFrame, .(Orig), summarise, "fullc"=paste(SC, collapse=" ",sep=" "))
  countries<-vector("list",nrow(fullc))
  for(i in 1:nrow(fullc)){
    try({
      TEXTI<-fullc$fullc[i]
      ncons<-ceiling(nchar(TEXTI)/ceiling(nchar(TEXTI)/4000))
      noutstop<-c(1,c(1:ceiling(nchar(TEXTI)/4000))*ncons)
      stsp<-lapply(1:c(length(noutstop)-1),function(X) c(noutstop[X],noutstop[X+1]))
      subers<-lapply(stsp,function(X) substr(TEXTI,X[1],X[2]))
      mres<-lapply(subers,function(X) GET(url="http://localhost:8080/CLIFF-2.1.1/parse/text",query = list(replaceAllDemonyms="true",q=X)))
      mres<-mres[sapply(mres,function(X) X$status_code==200)]
      res<-do.call(rbind.fill, lapply(mres,function(X) jsonlite::fromJSON(X[[1]][[1]])$results$places$focus$countries))[,c("countryCode","score")]
      res<-unlist(sapply(1:nrow(res),function(i) rep(res$countryCode[i],res$score[i])))
      countries[[i]]<-data.frame(t(as.matrix(table(res)/length(res))))
      cat(i)})}
  for(i in which(sapply(countries,length)==0)){
    countries[[i]]=data.frame("none"=0)}
  countries<-do.call(rbind.fill,countries)
  countries$Orig<-fullc$Orig
  countries$nchars<-nchar(fullc$fullc)
  f1<-join(BASE_INPUT$SentFrame[,c("OpID","Orig")],countries)
  f1<-unique(f1)
  f1
}
 

reflectCountryCol<-function(MATCHFRAME,pred2,howmany,binomial=FALSE){
  pred2[,3:c(ncol(pred2)-1)][is.na(pred2[,3:c(ncol(pred2)-1)])]<-0
  if(binomial==TRUE){
    pred2[,3:c(ncol(pred2)-1)][pred2[,3:c(ncol(pred2)-1)]>0]<-1
  }
  maxCs<-colnames(pred2[,3:c(ncol(pred2)-1)])[sort(colSums(pred2[,3:c(ncol(pred2)-1)]),index=TRUE,decreasing=TRUE)$ix][1:howmany]
  cntjoin<-join(MATCHFRAME,pred2[,c("OpID","Orig",maxCs)], by=c("OpID","Orig"))
  return(cntjoin)}


library('wordVectors')

nearest_to2<-function(topicm,wordvec,n=10,fixword=FALSE,limitwords=NULL){ 
  mt<-t(exp(topicm$beta$logbeta[[1]]))
  row.names(mt)<-topicm$vocab
  if(fixword==FALSE){
    whichwords<-unique(unlist(sapply(wordvec,function(X){which(stringr::str_detect(row.names(mt),X))})))} else
    {whichwords<-which(row.names(mt)%in%wordvec)}
  if(length(limitwords)>0){
    whichwords<-whichwords[limitwords]
  }
  sims = cosineSimilarity(mt, matrix(as.vector(mt[whichwords,]), ncol = ncol(mt)))
  ords = order(-sims[, 1])
  list("searchwords"=row.names(mt)[whichwords],"simwords"=structure(1 - sims[ords[1:n]], names = rownames(sims)[ords[1:n]]))
}

runMap<-function(FILENAMEQUOTED,path.file=FALSE,titleC){shiny::shinyApp(
  ui = fluidPage(sidebarLayout(
    sidebarPanel(selectizeInput("OpporID", label = "Opportunity Number", choices = unique(pred2$OpID),multiple=TRUE, selected=pred2$OpID[1]),
                 selectInput("projection",label="Map Projection",choices=c('equirectangular','mercator', 'orthographic','natural earth','kavrayskiy7','miller', 'robinson','eckert4','azimuthal equal area','azimuthal equidistant','conic equal area','conic conformal','conic equidistant','gnomonic','stereographic','mollweide','hammer','transverse mercator'),selected='mollweide')
    ),
    mainPanel(
      plotlyOutput("plotly"),
      tableOutput("table")
    ))),
  server = function(input, output) {
    ccodes<<-read.csv(textConnection(getURL("http://data.okfn.org/data/core/country-codes/r/country-codes.csv")),stringsAsFactors=FALSE)
    output$plotly<-renderPlotly({
      g <- list(showframe = FALSE,showcoastlines = TRUE,projection = list(type = input$projection))
      l <- list(color = toRGB("grey"), width = 0.5)
      d1<-data_mapper(pred2,input$OpporID)
      plot_ly(d1,z=value,hoverinfo="text", locations = ccode2, type = 'choropleth', marker = list(line = l),color = value, colors = 'Blues',zmax=max(value),zmin=min(value),text=hover,colorbar = list(title = 'Country Relevance'),source="select") %>% layout(title =paste(titleC,"<br>Source:<a href='https://github.com/c4fcm/CLIFF'>CLIFF</a>"), geo = g)
    })
    output$table<-renderTable({
      eventdata<-event_data("plotly_click", source = "select")$pointNumber+1
      d1<-data_mapper(pred2,input$OpporID)
      rframe<-data.frame("col1"=pred2$OpID[sort.int(pred2[,d1$countryids[eventdata]],decreasing=TRUE,index.return=TRUE)$ix[1:5]])
      colnames(rframe)[1]<-paste("Top 5 Opportunities for", d1$nameC[eventdata])
      rframe})
  },
  onStart=function(){
    suppressWarnings(library(plyr,warn.conflicts=FALSE,quietly=TRUE))
    suppressWarnings(library(dplyr,warn.conflicts=FALSE,quietly=TRUE))
    suppressWarnings(library(plotly,warn.conflicts=FALSE,quietly=TRUE))
    suppressWarnings(library(RCurl,warn.conflicts=FALSE,quietly=TRUE))
    suppressWarnings(library(DT,warn.conflicts=FALSE,quietly=TRUE))
    pred2<<-if(path.file==TRUE){read.csv(FILENAMEQUOTED,stringsAsFactors=FALSE) %>% .[,2:ncol(.)]} else {FILENAMEQUOTED}
    data_mapper<<-function(CountryPredictions,OPPORTUNITY){
      gchars<-ddply(CountryPredictions,.(OpID),summarise,"charsum"=sum(nchars))
      CountryPredictions<-plyr::join(CountryPredictions,gchars)
      CountryPredictions$weight<-CountryPredictions$nchars/CountryPredictions$charsum
      CountryPredictions<-cbind(CountryPredictions[,1:2],CountryPredictions[,3:c(ncol(CountryPredictions)-3)]*CountryPredictions$weight)
      ftemp<-dplyr::filter(CountryPredictions, OpID%in%OPPORTUNITY)
      tframe<-reshape2::melt(colSums(ftemp[,3:ncol(ftemp)],na.rm=TRUE)/sum(ftemp[,3:ncol(ftemp)],na.rm=TRUE))
      tframe$countryids<-row.names(tframe)
      tframe$ccode2<-plyr::join(data.frame("ISO3166.1.Alpha.2"=tframe$countryids),ccodes)$ISO3166.1.Alpha.3
      tframe$nameC<-plyr::join(data.frame("ISO3166.1.Alpha.2"=tframe$countryids),ccodes)$official_name_en
      tframe$hover<-paste(tframe$nameC,": ",round(tframe$value*100)/100,sep="")
      tframe}
  })}


tableapp<-function(basic_table,TOPICMODEL){
  wordpossibles<-colnames(basic_table)[unlist(sapply(c("subject","action","object"),function(X) which(stringr::str_detect(colnames(basic_table),X))))]
  basic_table<-basic_table %>% select(-one_of("Sent","Orig"))
  for(i in which(sapply(1:ncol(basic_table), function(X) class(basic_table[,X]))=="list")){
    basic_table[,i]<-sapply(basic_table[,i],function(X){paste(unlist(X),collapse=";")})}
  basic_table$sentence<-stringr::str_trim(basic_table$sentence)
  shinyApp(ui = fluidPage(sidebarLayout(
    sidebarPanel(selectInput("NOV", label = "Select Word Type",choices = wordpossibles, selected = wordpossibles[1]),
                 textInput("word",label= "Word", value=""),
                 checkboxInput("sent",label="Show Sentence",value=FALSE),
                 sliderInput("Kclusters", label = "Number of Words",min = 1, max = 25, value = 5, step = 1),
                 checkboxInput('Shorten',label='Shorten Strings?', value=TRUE)),
    mainPanel(dataTableOutput("table1")))), 
    server = function(input, output) {
      output$table1<-renderDataTable({
        if(input$sent==TRUE){t1<-basic_table} else {t1<-basic_table %>% select(-one_of("sentence"))}
        if(nchar(input$word)>1){
          matchwords<-names(nearest_to2(TOPICMODEL,input$word,n=input$Kclusters,fixword=FALSE,limitwords=NULL)[[2]])
          t1<-t1[unlist(sapply(matchwords,function(X) {which(stringr::str_detect(t1[,input$NOV],X))})),]}
        else {t1<-t1} 
        if(input$Shorten==TRUE){
          datatable(t1,rownames=FALSE,filter ='bottom',extensions = 'Buttons')} else {datatable(t1,rownames=FALSE,filter ='bottom',extensions = 'Buttons', options = list(dom = 'Bfrtip',scrollX=TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')), callback = JS('table.page(3).draw(false);'))}
      })})}


igraphob_object_force<-function(WORD,mtable,W,inputWord=TRUE,sankey=FALSE,verbfilter=c(),strictlimit=FALSE){
  # WORD='production'
  #mtable<-matchtable
  #W<-10
  if(inputWord==TRUE){
    ntr<-dplyr::filter(mtable,str_detect(tolower(subject.keywords),tolower(WORD)))
  }
  else {
    ntr<-dplyr::filter(mtable,str_detect(tolower(object.keywords),tolower(WORD)))
  }
  dt <- data.table(ntr)
  dt$object.keywords<-sapply(dt$object.keywords,function(X) paste(unlist(X),collapse=";"))
  dt$subject.keywords<-sapply(dt$subject.keywords,function(X) paste(unlist(X),collapse=";"))
  dt$action.lemmatized<-sapply(dt$action.lemmatized,function(X) paste(unlist(X),collapse=";"))
  dt$action.verb.text<-sapply(dt$action.verb.text,function(X) paste(unlist(X),collapse=";"))
  dt<-dt[,list(action.verb.text = unlist(strsplit(action.verb.text,";")),object.keywords=unlist(strsplit(object.keywords,";")),subject.keywords=unlist(strsplit(subject.keywords,";"))),by = sentence]
  dt<-na.omit(dt)
  if(length(verbfilter)>0){
    dt<-dplyr::filter(dt,action.verb.text%in%verbfilter)
  }
  if(strictlimit==TRUE){
    if(inputWord==TRUE){
      dt<-dplyr::filter(dt,str_detect(tolower(subject.keywords),tolower(WORD)))} else {dt<-dplyr::filter(dt,str_detect(tolower(object.keywords),tolower(WORD)))}
  }
  net1<-graph_from_data_frame(rbind(data.frame("In"=dt$subject.keywords,"Out"=dt$action.verb.text),data.frame("In"=dt$action.verb.text,"Out"=dt$object.keywords)))
  E(net1)$weight <- 1
  netsimp<-simplify(net1,edge.attr.comb=list(weight="sum","ignore"))
  E(netsimp)$width <- E(netsimp)$weight
  netsimp <- delete_edges(netsimp, E(netsimp)[weight<=W])
  netsimp<-simplify(netsimp,edge.attr.comb=list(weight="sum","ignore"))
  bad.vs<-V(netsimp)[degree(netsimp) == 0]
  netsimp <-delete.vertices(netsimp, bad.vs)
  d3ob<-netsimp %>% igraph_to_networkD3() 
  d3ob$nodes$group=sapply(d3ob$nodes$name,function(X) {if(X%in%dt$action.verb.text) {"verb"} else {if(X%in%dt$subject.keyword){"subject"} else {if(X%in%dt$object.keywords){"object"}}}})
  if(sankey==FALSE){
    forceNetwork(d3ob$links,d3ob$nodes,"source","target","value",NodeID="name",Group="group",fontSize=24,fontFamily="Arial", opacity = 0.9, bounded = TRUE, opacityNoHover = TRUE,colourScale=JS("d3.scale.ordinal().domain(['verb','subject','object']).range(['#d9d9d9','#b7a57a','#4b2e83'])"))} else {
      sankeyNetwork(d3ob$links,d3ob$nodes,"source","target","value",NodeID="name",NodeGroup="group",colourScale=JS("d3.scale.ordinal().domain(['verb','subject','object']).range(['#d9d9d9','#b7a57a','#4b2e83'])"),fontSize=24,fontFamily="Arial")}}


frametable<-function(PARSEFRAME,BASEINPUT,origent){
  basejoin<-BASEINPUT$out$meta
  basejoin$TopTopics<-BASEINPUT$top.topics
  joined<-cbind(PARSEFRAME,basejoin[PARSEFRAME$rowid,])
  colnames(joined)
  joined<-joined[,c(c(1:ncol(PARSEFRAME)),18,19,22,27,c(28+origent),ncol(joined))]
  joined<-joined[,c(1:ncol(joined))[-which(colnames(joined)%in%c("filename","comboID","rowid"))]]
  joined$ents<-gsub(",",";",joined$ents)
  joined
}



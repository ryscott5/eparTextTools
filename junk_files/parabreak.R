
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


processed <-textProcessor(par1$S,metadata=par1[,2:length(par1)],sparselevel=1,)
missingINF<-unlist(unique(sapply(colnames(corpEnt3),function(x){which(is.na((corpEnt3[,x])))}),simplify=T))
processed$vocab[2000]
out <- prepDocuments(processed$documents,processed$vocab,processed$meta,lower.thresh=4)
head(out$meta)
#st1<-stm(out$documents,out$vocab,K=0, init.type="Spectral")
st1<-stm(out$documents,out$vocab,data=out$meta, K=10,prevalence=~as.factor(Orig), init.type="LDA",max.em.its=5)


length(as.character(par1$S[-out$docs.removed,]))
findThoughts(st1,texts=as.character(par1$S[-processed$docs.removed])[-out$docs.removed],topics=2,n=10)
ggplot(melt(st1$theta))+geom_bar(aes(x=Var1,y=value,fill=as.factor(Var2)),stat="identity",position="stack")+scale_fill_tableau()+theme_minimal()

kef<-estimateEffect(c(1:10)~as.factor(Orig),st1,metadata=out$meta,documents=out$documents,nsims=1)

names(st1)
klist<-lapply(c(1:10), function(i){kef[[1]][[i]][[1]][[1]]})
klist<-do.call(rbind,lapply(1:10,function(i) cbind(melt(klist[[1]]),data.frame("name"=str_extract(names(klist[[1]]),"[0-9]+"),"top"=i))))
klist<-klist[,c(1:3,5)]
ddply(klist,.(name),summarize, "w"=top[which(top==max(top))][1])
tps<-max.col(st1$theta)

wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)
head(wd)
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
for(i in 14:length(vanstf)){
  X<-as.String(toChar)[vanstf[[i]][1]]
  req <- POST("http://access.alchemyapi.com/calls/text/TextGetRelations", 
    body = list(apikey="6837e8ae18678cadd3c42fc55ed938b3818ce470",text=X,outputMode="json",disambiguate=0), encode = "form",write_disk(file.path("getAlchemy",vanstf[[i]][1]$id)))
  Sys.sleep(.5)
}
library(jsonlite)
subs<-lapply(1:length(list.files("getAlchemy")),function(i) fromJSON(readLines(file.path("getAlchemy",list.files("getAlchemy")[i])))$relations$subject)
obs<-lapply(1:length(list.files("getAlchemy")),function(i) fromJSON(readLines(file.path("get6lchemy",list.files("getAlchemy")[i])))$relations$object)


#Name Outcomes of interest
For example, production...
sapply(obs, function(x) {TRUE%in%str_detect(x$text,"production")})


lapply(subs,function(x) x$text)
lapply(subs,function(x) x$text)
k1$relations$subject
k1$relations$action
k1$relations$object


k1$relations


str_detect(as.String(toChar)[vanstf[[i]][1]],wd$Up.Words)

str_detect(as.character(as.String(toChar)[vanstf[[i]][1]]),wd$Up.Words)
length(vanstf)








as.String(toChar)[vanstf[[2]][[1]]]
vanstf[[2]][[2]]

Tree_parse(vanstf[[2]][[2]]$features[[1]]$parse)$children
Tree_leaf_gatherer <-function()
  {
    v <- list()
    list(update =
           function(e) if(!inherits(e, "Tree")) v <<- c(v, list(e)),
         value = function() v,
         reset = function() { v <<- list() })
}
Tree_parse(vanstf[[2]][[2]]$features[[1]]$parse)
c("VBZ","VBN","TO")
install.packages("parsent")

#if (!require("pacman")) install.packages("pacman")
#pacman::p_load_gh(c("trinker/textshape", "trinker/parsent"))
#ADDPARSEANNOTATOR
library(parsent)
get_phrase_type


lapply(vanstf[[2]][[2]]$features[[1]],get_phrases)

get_phrase_type(vanstf[[1]][[2]]$features[[1]]$parse, "NP") %>% take() %>%  get_leaves()


get_phrase_type_regex(vanstf[[1]][[2]]$features[[1]]$parse, "VP") %>% take() %>% get_phrase_type_regex("VP")
get_phrase_type_regex("VP"))

%>% take() %>% get_phrase_type("VP") %>% take()

get_phrase_type_regex(vanstf[[2]][[2]]$features[[1]]$parse, "VP") %>% get_phrase_type_regex("VP")
rapply
str_split(get_phrase_type_regex(take(vanstf[[2]][[2]]$features[[1]]$parse) ,"VP"),"\\(VP")

?str_split
, function(x) get_phrase_type_regex(x,"VP"))
?rapply

get_phrase_type_regex(vanstf[[2]][[2]]$features[[1]]$parse, "VP")  %>% take() %>% get_phrase_type_regex("NP") %>% get_phrase_type_regex("VP")

get_phrase_type_regex(vanstf[[2]][[2]]$features[[1]]$parse, "VP")  %>% take() %>% get_phrase_type_regex("NP") %>% take() %>% get_phrase_type_regex("VP") 

?get_phrase_type_regex

?get_phrase_type
li
as.String(toChar)[anns[[1]],]

b1<-subset(anns,anns$start>=anns[[1]]$start) %>% subset(.,anns$end<=anns[[1]]$end) 
?Parse_Annotator
b2<-parse_annotator(as.String(toChar), b1)
b2$features
%>% subset(.,type=="word") 
subset(b1,unlist(features)%in%c("VB","VBN","VBZ","VBG","VBN","VBP")) 
c("NN","NNS","NNP","NNPS","VB")

wd$Up.Words

ahead(klist)
kf<-data.frame("high"=c(kef[[1]][[1]][[1]][[1]]+c(1.96*sqrt(diag(kef[[1]][[1]][[1]][[2]])))),"low"=c(kef[[1]][[1]][[1]][[1]]-c(1.96*sqrt(diag(kef[[1]][[1]][[1]][[2]])))),'id'=past("d",str_extract(names(kef[[1]][[1]][[1]][[1]]),"[0-9]+")))
length(kef)
head(kf)
toChar
ggplot(kf[order(kef[[1]][[1]][[1]][[1]]),])+geom_errorbar(aes(x=id,ymin=low,ymax=high))
length(kef[[1]])
?estimateEffect
library(httr)
anns[[1]]
for(i in 1:length(vanstf)){vanstf[[i]][[3]]<-paste("up",i,sep="")}
length(vanstf)

for(i in 1:length(vanstf)){
X<-as.String(toChar)[vanstf[[i]][[1]][1]]
req <- POST(
  "http://access.alchemyapi.com/calls/text/TextGetRelations", 
  body = list(
    apikey="6837e8ae18678cadd3c42fc55ed938b3818ce470",
    text=X,
    outputMode="json",
    disambiguate=0), 
  encode = "form",
  write_disk(file.path("getAlchemy","up1.json")))
}




getAlchemy/up1")
library(jsonlite)
fromJSON(readLines("getAlchemy/up1"))
http_status(req)
tout<-httr::content(req)
x<-tout$relations[[3]]
x$sentence
lapply(tout$relations, function(x){ 
  if(TRUE%in%str_detect(x$sentence,wd$Up.Words)){ 
    if(x$action$lemmatized%in%wd$Up.Words){
  data.frame("SUB"=x$subject$text,"AC"=x$action$lemmatized,"ACTense"=x$action$verb$tense,"OB"=x$object$text)}
    }})}


rapply(tout$relations, function(x) {.$subject})
temp<-do.call(rbind, lapply(tout$entities,function(X) {data.frame(X)[,c("type","relevance","count","text")]}))
temp$compnum<-complains$compnum[i]
temp$operator<-complains$operator[i]
temp$Lat<-complains$Lat[i]
temp$Lon<-complains$Lon[i]
if(i==1){copy_to(my_db,temp,"entities",temporary=FALSE)} else {
  db_insert_into(con = my_db$con, table = "entities", values = temp)}})
cat(paste(" ",i,sep=""))
Sys.sleep(.25)})}

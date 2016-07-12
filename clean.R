path = "../../Volumes/files/Project/EPAR/EPAR Templates and Guidelines/Text Analysis/Sample docs/"
allfiles<-list.files(path, recursive=TRUE)
fnames<-paste(path,allfiles,sep="")
alldocs<-lapply(fnames,getTextR)
corpus<-do.call(c,alldocs)
stopWords <- function(x) removeWords(x, stopwords("en"))
funs <- list(stripWhitespace,
             stopWords,
             removePunctuation,
             stemDocument,
             content_transformer(tolower))
corpus2<-tm_map(corpus, FUN = tm_reduce, tmFuns = funs, mc.cores=1)
library(DT)
tdm<-TermDocumentMatrix(corpus2)
tdm<-removeSparseTerms(tdm,.2)
wordlist<-c("cheese","gender")
t1<-findAssocs(tdm,wordlist,.5)
termDocumentMatrix=tdm
corrVal=.5
names(assoctable)[1]
assoctable<-findAssocs(termDocumentMatrix,wordlist,corrVal)
assocPTable<-function(assoctable,corpus){
  #assoctable<-assoctable[sapply(assoctable,length)>0]
  dft<-do.call(rbind,lapply(1:length(assoctable),function(i){tryCatch({data.frame("Word"=names(assoctable)[i],"Match"=names(assoctable[[i]]),"Association"=c(assoctable[[i]]))},error=function(e){data.frame("Word"=names(assoctable)[i],"Match"="too few words","Association"=c(0))})}))
  dft$Word<-as.character(dft$Word)
  dft$Match<-as.character(dft$Match)
  #dft$Word[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])]<-stemCompletion(dft$Word[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])],dictionary=corpus,type="prevalent")
  dft$Match[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])]<-stemCompletion(dft$Match[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])],dictionary=corpus,type="prevalent")
  datatable(data=dft,rownames=FALSE,filter="top",)}
assocPrettyOneStep<-function(wordlist,termDocumentMatrix,corpus,corrVal=.8){
  assocPTable(findAssocs(termDocumentMatrix,wordlist,corrVal),corpus)
}
assocPrettyOneStep(wordlist=c("cheese","gender","equality"),tdm,corpus,corrVal=.7)

#Word Bigram and Collocations
BigramTokenizer <-function(x){unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
TrigramTokenizer <-function(x){unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}
tdm_bi <- TermDocumentMatrix(corpus2, control = list(tokenize = TrigramTokenizer))
sort(inspect(removeSparseTerms(tdm_bi, 0.5)))[1:10]
library(NLP)
install.packages("openNLP")
library(openNLP)
install.packages('openNLPmodels.en',repos="http://datacube.wu.ac.at/",type="source")
library(openNLPmodels.en)
install.packages('StanfordCoreNLP',repos="http://datacube.wu.ac.at/",type="source")
nchar(as.String(corpus[2]))
options(java.parameters = "-Xmx4g")
gc()

#if Document follows Grand Challenges Layout:
scaplist<-lapply(corpus[1:22],function(X){
  splitted<-unlist(str_split(as.String(X),"Scope and Approach",n=2))
list("PS"=str_split(splitted[[1]],"Problem Statement")[[1]][2],"GS"=str_split(splitted[2],"Measurement and Evaluation")[[1]][1],"ME"=str_split(str_split(splitted[2],"Measurement and Evaluation")[[1]][2],"Risk Mitigation")[[1]][1])
})
names(scaplist[[1]])
problemStatements<-sapply(scaplist,function(x) x$PS)
scopeStatements<-sapply(scaplist,function(x) x$GS)
measureStatements<-sapply(scaplist,function(x) x$ME)

names(corpus[1:22])
install.packages("stm")

library(stm)
?textProcessor
#metadata can be added to the below command
processed <- textProcessor(c(problemStatements,scopeStatements,measureStatements),metadata=data.frame("element"=c(rep("problem",length(problemStatements)),rep("scope",length(scopeStatements)),rep("measure",length(measureStatements))),"doc"=rep(1:length(problemStatements),3)))
out <- prepDocuments(processed$documents,processed$vocab,processed$meta)
?prepDocuments
#can add covariates to selectModel
ksearch<-searchK(out$documents, out$vocab, K = c(7, 10))
?selectModel
mselect<-selectModel(out$documents, out$vocab, K = 20, prevalence=~element,data=out$meta,max.em.its = 75, runs = 20, seed = 8458159)
plotModels(mselect,labels=1:length(mselect$runout))
?plotModels
basemod<-mselect$runout[[3]]
labelTopics(basemod, c(6))
topicQuality(basemod,out$documents)
out
efest <- estimateEffect(1:20~element, basemod, metadata=out$meta, documents=out$documents)

efesmeans<-do.call(rbind, lapply(efest$parameters,function(X){colMeans(do.call(rbind,lapply(X,function(x){x$est})))}))
efesmeans<-data.frame(efesmeans)
head(efesmeans)
efesmeans$topic<-1:nrow(efesmeans)
library(ggplot2)
library(reshape2)
efesmeans<-melt(efesmeans,id="topic")
head(efesmeans)
ggplot(efesmeans)+geom_point(aes(x=value, y=variable))+facet_wrap(~topic)
plot.estimateEffect(efest,"element",model=basemod,method="pointestimate")
?thetaPosterior
th.est<-thetaPosterior(basemod,nsims=1,type="Local",documents=out$documents)
lapply(which(out$meta$doc==1), function(x) th.est[[x]])

theta3s<-lapply(1:22,function(X) do.call(rbind, lapply(which(out$meta$doc==X), function(x){
  temp<-melt(th.est[[x]])
  temp<-temp[order(temp$value,decreasing=TRUE),][1:3,]
  temp$Var1<-out$meta$doc[x]
  temp$element<-out$meta$element[x]
  temp$rank<-1:3
  temp
  })))
theta3s[[1]]
casframe<-dcast(do.call(rbind,theta3s),Var1~element+rank,value.var="Var2")
library(MASS)
for(i in 1:ncol(casframe)){casframe[,i]<-as.factor(casframe[,i])}
mca1<-MCA(casframe[,2:ncol(casframe)])
install.packages("ClustOfVar")
library("ClustOfVar")
?hclustvar
hclust<-hclustvar(X.quali=casframe[,2:ncol(casframe)])
plot(hclust)
mca1$eig
plot(mca1)

theta3s[[1]]
library(tidyr)

library(reshape2)
?recast
library("FactoMineR")

library(FactoMineR)
head(theta3s)
theta3s$Var1<-as.factor(theta3s$Var1)
theta3s$Var2<-as.factor(theta3s$Var2)
head(theta3s)
sumframe<-dcast(theta3s, .~Var1)
#dcast(theta3s, value~Var1+element)

MCA()
?MCA
xtabs(~theta3s$Var2+theta3s$element)

?which.max
?sort

Tagged.Text<-function (x) {
  x<-as.String(x)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator<- Maxent_POS_Tag_Annotator()
  org.annotate<-Maxent_Entity_Annotator(language = "en", kind="organization", probs = FALSE,model = NULL)
  pers.annotate<-Maxent_Entity_Annotator(language = "en", kind="person", probs = FALSE,model = NULL)
  location.annotate<-Maxent_Entity_Annotator(language = "en", kind="location", probs = FALSE,model = NULL)
  #misc.annotate<-Maxent_Entity_Annotator(language = "en", kind="misc", probs = FALSE,model = NULL)
  money.annotate<-Maxent_Entity_Annotator(language = "en", kind="money", probs = FALSE,model = NULL)
  parse_annotator <- Parse_Annotator()
  y1<-annotate(x, sent_token_annotator)
  gc()
  y1<-annotate(x,word_token_annotator,y1)
  gc()
  y2<-annotate(x,pos_tag_annotator,y1)
  gc()
  y2<-annotate(x,org.annotate,y2)
  gc()
  y2<-annotate(x,pers.annotate,y2)
  gc()
  y2<-annotate(x,money.annotate,y2)
  y3<-parse_annotator(x,y2)
  gc()
  list(y2,y3)}

Corpus.tagged<-list(Tagged.Text(problemStatements[1]))
Corpus.tagged[[1]][1]

Corpus.tagged<-list(list(y2,y3))
table(Corpus.tagged[[1]][[1]]$type)
Corpus.tagged[[1]][[2]][[1]]$features[[1]][[1]]
parsedtexts <- sapply(Corpus.tagged[[1]][[2]][[1]]$features, '[[', "parse")
parsetrees <- lapply(parsedtexts, Tree_parse)
gc()
parsetrees
corpart<-subset(Corpus.tagged[[1]][[1]],type=="word")
corpart<-unlist(corpart$features)
as.String(corpus[1])[subset(Corpus.tagged[[1]][[1]],type=="word")[which(corpart%in%c("VB","VBN","VBG","VBP","VBZ"))]]

as.String(corpus[1])[subset(Corpus.tagged[[1]], type=="entity")] 
^en*
*fy^,
causewords<-c("allow", "block", "cause", "enable", "force", "get", "help", "hinder", "hold", "impede", "keep", "leave", "let", "make", "permit", "prevent", "protect", "restrain", "save", "set", "start", "stimulate", "stop","aid","bar", "bribe", "compel", "constrain", "convince", "deter", "discourage", "dissuade", "drive", "have", "hamper", "impel", "incite", "induce", "influence", "inspire", "lead", "move", "persuade", "prompt", "push", "restrict", "rouse", "send", "spur")

verbs<-subset(Corpus.tagged[[1]][[1]],type=="word")[which(corpart%in%c("VB","VBN","VBG","VBP","VBZ"))]
head(verbs)
causeverbs<-verbs[which(stemDocument(as.String(corpus[1])[verbs],language="english")%in%stemDocument(causewords, language="english"))]
tripleist<-lapply(causeverbs$id,function(x){subset(Corpus.tagged[[1]],id%in%c(x-2,x-1,x,x+1,x+2))})

tripleist<-lapply(causeverbs$start, function(x){
smaller<-subset(Corpus.tagged[[1]][[1]], type=="sentence")
smaller<-subset(smaller, start<=x) 
smaller<-subset(smaller, end>=x)
smaller2<-subset(Corpus.tagged[[1]][[1]],start>=smaller$start[1])
smaller2<-subset(smaller2,end<=smaller$end[1])
smaller3<-subset(Corpus.tagged[[1]][[2]], start<=x)
smaller3<-subset(smaller3,end>=x)
list(smaller2,smaller3)})
lapply(sapply(tripleist[[7]][[2]]$features, '[[', "parse"), Tree_parse)
as.String(corpus[1])[tripleist[[1]][1]]
as.String(corpus[1])[subset(tripleist[[8]],type=='entity')]

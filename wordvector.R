numwords<-426572
word.vector.file<-"../LargeFiles/conceptnet-numberbatch-201609_en_main.txt"
library(data.table)
word.vector.df<-fread(word.vector.file,nrows=10,header=FALSE,select=1)
word.vector.df[1,]

readLines(word.vector.file,n=1)
word.vector.df
row.names(word.vector.df)
normalize.vector(colSums(word.vector.df[grep("^man$",word.list),-1]))
word.vector.df[1,]
XML::parseURI(word.vector.df[2])
install.packages("ppls")
devtools::install_github("bmschmidt/wordVectors")
library(ppls)
rm(anf)
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
nearest_to2(st1,"nutrit",1,limitwords=7)
?cosineSimilarity
  nearest_to
?nearest_to
library(stm)
?stm

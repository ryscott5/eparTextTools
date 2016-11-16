
st1$vocab[[1]]

library(stm)
library(tm)
content(corp$`083110 OPP49590_RDI_2010_RSA.pdf`)


dnf<-textProcessor(corp,meta=data.frame("id"=names(corp)))
tm::DocumentTermMatrix(tolower(corp[[1]]))
paste(st1$vocab,collapse="|")

doclimits<-lapply(corp,function(X) paste(stringr::str_extract(tolower(content(X)),paste(st1$vocab,collapse="|")),sep=" ",collapse=" ") %>% gsub("NA","",.))

doclimproc<-textProcessor(doclimits,metadata=data.frame("id"=names(corp)),lowercase=TRUE,removestopwords=FALSE,removenumbers=FALSE,sparselevel=1,language="en")

length(doclimproc$vocab)
textProcessorNEW<-function (documents, metadata = NULL, language = "en",TOPMOD) {
  documents <- as.character(documents)
  documents <- str_replace_all(documents, "[^[:graph:]]", " ")
  txt <- tm::VCorpus(tm::VectorSource(documents), readerControl = list(language = language))
  txt <- tm::tm_map(txt, tm::stripWhitespace)
  if (!is.null(metadata)) {
    for (i in 1:ncol(metadata)) {
      NLP::meta(txt, colnames(metadata)[i]) <- metadata[,i]
    }
  }
  dtm <- tm::DocumentTermMatrix(txt, control = list(dictionary=TOPMOD$vocab))
  if (!is.null(metadata)) {
    docindex <- unique(dtm$i)
    metadata <- NLP::meta(txt)[docindex, ]
  }
  read.slam <- function(corpus) {
    #convert a simple triplet matrix to list format.
    if(!inherits(corpus, "simple_triplet_matrix")) stop("corpus is not a simple triplet matrix")
    if ("TermDocumentMatrix" %in% class(corpus)) {
      non_empty_docs <- which(slam::col_sums(corpus) != 0)
      documents <- ijv.to.doc(corpus[,non_empty_docs]$j, corpus[,non_empty_docs]$i, corpus[,non_empty_docs]$v) 
      names(documents) <- corpus[,non_empty_docs]$dimnames$Docs
    } else {
      non_empty_docs <- which(slam::row_sums(corpus) != 0)
      ijv.to.doc <- function(i,j,v) {
        index <- split(j,i)
        index <- lapply(index,as.integer)
        count <- split(v,i)
        count <- lapply(count,as.integer)
        mapply(rbind,index,count)
      }
      documents <- ijv.to.doc(corpus[non_empty_docs,]$i, corpus[non_empty_docs,]$j, corpus[non_empty_docs,]$v) 
      names(documents) <- corpus[non_empty_docs,]$dimnames$Docs
    }
    vocab <- corpus$dimnames$Terms
    return(list(documents=documents,vocab=vocab))
  }
  out <- read.slam(dtm)
  kept <- (1:length(documents) %in% unique(dtm$i))
  vocab <- as.character(TOPMOD$vocab)
  out <- list(documents = out$documents, vocab = vocab, meta = metadata, docs.removed = which(!kept))
  class(out) <- "textProcesser"
  return(out)
}

npt<-textProcessorNEW(doclimits,metadata=data.frame("id"=names(corp)),language="en",st1)

K <- st1$settings$dim$K
temp<-
eta <- rep(0, K-1) #intialize eta uninformatively
design <- st1$settings$covariates$X[rep(1,length(npt$documents)),]
mu <- design%*%st1$mu$gamma
str(mu) #now we have a mu vector for each document
siginv <- st1$invsigma
beta <- exp(st1$beta$logbeta[[1]]) # always the first one if no content covariate
newdocs <- npt$documents #again just going to use the last few here as a cheat
sigmaentropy <- (0.5 * determinant(st1$sigma, logarithm = TRUE)$modulus[1])

#now we just loop over the documents
fit <- vector(mode="list", length=length(newdocs))
for(i in 1:length(fit)) {
  beta.i <- beta[, newdocs[[i]][1,], drop = FALSE]
  fit[[i]] <- stm:::logisticnormalcpp(eta, mu[i,], siginv, beta.i, newdocs[[i]], sigmaentropy)
}

#MAP estimate for theta
thetaMAP <- do.call(rbind,lapply(fit, function(x) stm:::softmax(c(x$eta$lambda,0))))
#full posterior info inside fit
nrow(thetaMAP)
mops<-sapply(basename(names(corp)),function(X)
  fulset$Opportunity.ID[which(str_detect(fulset$path,fixed(X)))[1]])
na.omit(c(workpage$Ag,workpage$Nut))
as.character(mops)
probM<-sapply(1:length(mops),function(X) prod(1-thetaMAP[X,na.omit(c(workpage$Ag,workpage$Nut))]))
head(probM)
nex<-read.csv("../../bucket1/nonExcel.csv")
newds<-data.frame("Prob"=probM,"OpID"=mops)
newds$mancode<-newds$OpID%in%as.character(nex$Opportunity.ID)
ggplot(newds)+geom_histogram(aes(x=Prob,fill=mancode))
rout<-roc(newds$mancode,newds$Prob)
ggplot()+geom_path(aes(x=1-rout$specificities,y=rout$sensitivities))+geom_abline(aes(intercept=0,slope=1),lty=2)+theme_minimal()+xlab("1-Specificity")+ylab("Sensitivity")+theme(text=element_text(size=24))
temp<-ddply(.data=newds,.(OpID),summarise,"TotProd"=prod(1-Prob))
temp$mancode<-temp$OpID%in%as.character(nex$Opportunity.ID)
ggplot(temp[sort(temp$TotProd,decreasing=FALSE,index.return=TRUE)$ix,])+geom_bar(aes(x=mancode))+theme_bw()+ggtitle("For 30 Highest Probabilites, How Many Were Also Manually Coded?")+xlab("Manually Coded as Agriculture or Nutrition")


table(newds$OpID)
ggplot(newds[sort(newds$Prob,decreasing=FALSE,index.return=TRUE)$ix[1:30],])+geom_bar(aes(x=mancode))+theme_bw()+ggtitle("For 30 Highest Probabilites, How Many Were Also Manually Coded?")+xlab("Manually Coded as Agriculture or Nutrition")

qplot(temp$TotProd)
workpage

summary(glm(mancode~Prob, data=newds,family=binomial(link="logit")))    
newds$
rout2<-roc(newds$mancode,newds$Prob)
rout<-roc(temp$mancode,temp$TotProd)
ggplot()+geom_path(aes(x=1-rout$specificities,y=rout$sensitivities))+geom_abline(aes(intercept=0,slope=1),lty=2)+theme_minimal()+xlab("1-Specificity")+ylab("Sensitivity")+geom_path(aes(x=1-rout2$specificities,y=rout2$sensitivities))+geom_abline(aes(intercept=0,slope=1),lty=2)+theme_minimal()+xlab("1-Specificity")+ylab("Sensitivity")+theme(text=element_text(size=24))

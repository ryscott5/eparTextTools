#start with first noun
#build tree to all verbs, all nouns

library(stringr)
library(data.table)
library(igraph)
library(networkD3)

matchtable<-readRDS("../Research.Grants/matchtable.rds")

igraphob_object<-function(WORD,mtable,W,inputWord=TRUE,sankey=FALSE,verbfilter=c(),strictlimit=FALSE){
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
  netsimp}

igraphob_object2<-function(WORD,mtable,W,inputWord=TRUE,sankey=FALSE,verbfilter=c(),strictlimit=FALSE){
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
  dt<-dt[,list(action.verb.text = tolower(unlist(strsplit(action.verb.text,";"))),object.keywords=tolower(unlist(strsplit(object.keywords,";"))),subject.keywords=tolower(unlist(strsplit(subject.keywords,";")))),by = sentence]
  dt<-na.omit(dt)
  if(length(verbfilter)>0){
    dt<-dplyr::filter(dt,action.verb.text%in%verbfilter)
  }
  if(strictlimit==TRUE){
    if(inputWord==TRUE){
      dt<-dplyr::filter(dt,str_detect(tolower(subject.keywords),tolower(WORD)))} else {dt<-dplyr::filter(dt,str_detect(tolower(object.keywords),tolower(WORD)))}
  }
  net1<-graph_from_data_frame(data.frame("In"=dt$subject.keywords,"Out"=paste(dt$action.verb.text,dt$object.keywords)))
  E(net1)$weight <- 1
  netsimp<-simplify(net1,edge.attr.comb=list(weight="sum","ignore"))
  E(netsimp)$width <- E(netsimp)$weight
  netsimp <- delete_edges(netsimp, E(netsimp)[weight<=W])
  netsimp<-simplify(netsimp,edge.attr.comb=list(weight="sum","ignore"))
  bad.vs<-V(netsimp)[degree(netsimp) == 0]
  netsimp <-delete.vertices(netsimp, bad.vs)
  netsimp}
temp<-igraphob_object2("nutri",matchtable,W=0,strictlimit=T)
temp<-as_adjacency_matrix(temp)


igraphob_object3<-function(WORD,mtable,W,inputWord=TRUE,sankey=FALSE,verbfilter=c(),strictlimit=FALSE){
  #WORD='potato'
  #mtable<-matchtable
  # W<-0
  do.call(union,lapply(unique(mtable$TopTopics),function(TP){
    #TP<-unique(mtable$TopTopics)[2]
    do.call(union,lapply(unique(dplyr::filter(mtable, TopTopics==TP)$Orig),function(OP){
      #OP<-unique(dplyr::filter(mtable, TopTopics==TP)$Orig)[1]
      tryCatch({
        mtable<-dplyr::filter(mtable, TopTopics==TP,Orig==OP)
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
        dt<-dt[,list(action.verb.text = tolower(unlist(strsplit(action.verb.text,";"))),object.keywords=tolower(unlist(strsplit(object.keywords,";"))),subject.keywords=tolower(unlist(strsplit(subject.keywords,";")))),by = sentence]
        dt<-na.omit(dt)
        if(length(verbfilter)>0){
          dt<-dplyr::filter(dt,action.verb.text%in%verbfilter)
        }
        if(strictlimit==TRUE){
          if(inputWord==TRUE){
            dt<-dplyr::filter(dt,str_detect(tolower(subject.keywords),tolower(WORD)))} else {dt<-dplyr::filter(dt,str_detect(tolower(object.keywords),tolower(WORD)))}
        }
        net1<-graph_from_data_frame(data.frame("In"=dt$subject.keywords,"Out"=paste(dt$action.verb.text,dt$object.keywords),vertices=data.frame("Name"=dt$subject.keywords,"Topic"=rep(TP,nrow(dt)),"Orig"=rep(OP,nrow(dt)))))
        E(net1)$weight <- 1
        net1},error=function(e){graph.empty()})}))}))}

igraphob_object4<-function(WORD,mtable,W,inputWord=TRUE,sankey=FALSE,verbfilter=c(),strictlimit=FALSE){
  #WORD='potato'
  #mtable<-matchtable
  #W<-0
  lapply(unique(mtable$TopTopics),function(TP){
    #TP<-unique(mtable$TopTopics)[3]
    lapply(unique(dplyr::filter(mtable, TopTopics==TP)$Orig),function(OP){
      #OP<-unique(dplyr::filter(mtable, TopTopics==TP)$Orig)[1]
      list("Topic"=TP,"Orig"=OP,"Net"=tryCatch({
        mtable<-dplyr::filter(mtable, TopTopics==TP,Orig==OP)
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
        dt<-dt[,list(action.verb.text = tolower(unlist(strsplit(action.verb.text,";"))),object.keywords=tolower(unlist(strsplit(object.keywords,";"))),subject.keywords=tolower(unlist(strsplit(subject.keywords,";"))),Topic=TopTopics,Orig=Orig),by = sentence]
        dt<-na.omit(dt)
        if(length(verbfilter)>0){
          dt<-dplyr::filter(dt,action.verb.text%in%verbfilter)
        }
        if(strictlimit==TRUE){
          if(inputWord==TRUE){
            dt<-dplyr::filter(dt,str_detect(tolower(subject.keywords),tolower(WORD)))} else {dt<-dplyr::filter(dt,str_detect(tolower(object.keywords),tolower(WORD)))}
        }
        net1<-graph_from_data_frame(data.frame("In"=dt$subject.keywords,"Out"=paste(dt$action.verb.text,dt$object.keywords)))
        E(net1)$weight <- 1
        net1},error=function(e){graph.empty()}))})
  })}

igraphob_object5<-function(WORD,mtable,W,inputWord=TRUE,sankey=FALSE,verbfilter=c(),strictlimit=FALSE){
  #WORD='potato'
  #mtable<-matchtable
  #W<-0
  lapply(unique(mtable$TopTopics),function(TP){
    #TP<-unique(mtable$TopTopics)[3]
    lapply(unique(dplyr::filter(mtable, TopTopics==TP)$Orig),function(OP){
      #OP<-unique(dplyr::filter(mtable, TopTopics==TP)$Orig)[1]
      list("Topic"=TP,"Orig"=OP,"Net"=tryCatch({
        mtable<-dplyr::filter(mtable, TopTopics==TP,Orig==OP)
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
        dt<-dt[,list(action.verb.text = tolower(unlist(strsplit(action.verb.text,";"))),object.keywords=tolower(unlist(strsplit(object.keywords,";"))),subject.keywords=tolower(unlist(strsplit(subject.keywords,";"))),Topic=TopTopics,Orig=Orig),by = sentence]
        dt<-na.omit(dt)
        if(length(verbfilter)>0){
          dt<-dplyr::filter(dt,action.verb.text%in%verbfilter)
        }
        if(strictlimit==TRUE){
          if(inputWord==TRUE){
            dt<-dplyr::filter(dt,str_detect(tolower(subject.keywords),tolower(WORD)))} else {dt<-dplyr::filter(dt,str_detect(tolower(object.keywords),tolower(WORD)))}
        }
        net1<-graph_from_data_frame(data.frame("In"=dt$subject.keywords,"Out"=paste(dt$action.verb.text,dt$object.keywords)))
        E(net1)$weight <- 1
        igraph::as_edgelist(net1)
        net1},error=function(e){as_edgelist(graph.empty())}))})
  })}


igraphob_object6<-function(WORD,mtable,W,inputWord=TRUE,sankey=FALSE,verbfilter=c(),strictlimit=FALSE){
  #WORD='potato'
  #mtable<-matchtable
  #W<-0
  lapply(unique(mtable$TopTopics),function(TP){
    #TP<-unique(mtable$TopTopics)[3]
    lapply(unique(dplyr::filter(mtable, TopTopics==TP)$Orig),function(OP){
      #OP<-unique(dplyr::filter(mtable, TopTopics==TP)$Orig)[1]
      list("Topic"=TP,"Orig"=OP,"Net"=tryCatch({
        mtable<-dplyr::filter(mtable, TopTopics==TP,Orig==OP)
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
        dt<-dt[,list(action.verb.text = tolower(unlist(strsplit(action.verb.text,";"))),object.keywords=tolower(unlist(strsplit(object.keywords,";"))),subject.keywords=tolower(unlist(strsplit(subject.keywords,";"))),Topic=TopTopics,Orig=Orig),by = sentence]
        dt<-na.omit(dt)
        if(length(verbfilter)>0){
          dt<-dplyr::filter(dt,action.verb.text%in%verbfilter)
        }
        if(strictlimit==TRUE){
          if(inputWord==TRUE){
            dt<-dplyr::filter(dt,str_detect(tolower(subject.keywords),tolower(WORD)))} else {dt<-dplyr::filter(dt,str_detect(tolower(object.keywords),tolower(WORD)))}
        }
        net1<-graph_from_data_frame(data.frame("In"=dt$subject.keywords,"Out"=dt$object.keyword,'edgename'=dt$action.verb.text))
        net1},error=function(e){graph.empty()}))})
  })}


dt2 <- data.table(matchtable)
dt2$object.keywords<-sapply(dt2$object.keywords,function(X) paste(unlist(X),collapse=";"))
dt2$subject.keywords<-sapply(dt2$subject.keywords,function(X) paste(unlist(X),collapse=";"))
dt2$action.lemmatized<-sapply(dt2$action.lemmatized,function(X) paste(unlist(X),collapse=";"))
dt2$action.verb.text<-sapply(dt2$action.verb.text,function(X) paste(unlist(X),collapse=";"))
dt2<-dt2[,list(action.verb.text = tolower(unlist(strsplit(action.verb.text,";"))),object.keywords=tolower(unlist(strsplit(object.keywords,";"))),subject.keywords=tolower(unlist(strsplit(subject.keywords,";"))),Topic=TopTopics,Orig=Orig),by = sentence]
dt2<-na.omit(dt2)
dt3<-unique(dt2)
dt2<-dplyr::select(dt2, -sentence)
library("wordnet")
library(pbapply)

callwnetverbs<-pblapply(unique(dt2$action.verb.text),function(X){
  #X<-unique(dt2$action.verb.text)[3]
  X2<-getTermFilter("WildcardFilter", X, TRUE) %>% getIndexTerms("VERB", 1, .)
  if(length(X2)>=1){
    getSynonyms(X2[[1]])} else {list()}
})
library(dplyr)
library(dplyr)
install.packages("RSQLite")
my_db <- dplyr::src_sqlite("../sqlite-31.db", create = F)
wordtab<-tbl(my_db, sql("SELECT * from words"))
wordtab<-filter(wordtab, lemma%in%dt2$action.verb.text)
head(matchtable)
vtab<-select(tbl(my_db, sql("SELECT * from verbnetroles")) %>% filter(., wordid%in%collect(select(wordtab,wordid))$wordid),wordid,class)
vtab<-collect(left_join(wordtab,vtab))
vtab<-unique(vtab)
cldf<-filter(collect(tbl(my_db,sql("SELECT * from vnclasses"))),str_detect(class,"\\b([0-9][0-9]$|[0-9][0-9]\\.1)\\b$"))
cldf<-filter(cldf,nchar(str_extract(class,"[\\d\\.]+"))<=4)
cldf$classid<-str_extract(cldf$class,"[0-9]+")
cldf$word<-str_extract(cldf$class,"[a-z]+")
vtab$classid<-str_extract(vtab$class,"[0-9]+")
vtab<-dplyr::left_join(vtab,select(cldf,c(classid,word)))
vtab<-unique(vtab)
vtab<-na.omit(vtab)
dt2$lemma<-dt2$action.verb.text
dt2<-plyr::join(dt2,select(vtab,c(lemma,word)),match="first")
dt2$word[which(dt2$lemma=="be")]<-"exist"

callwn2L<-lapply(names(table(dt2$word)),function(X){
  callwn2<-graph_from_data_frame(data.frame(select(filter(dt2,word==X),c(subject.keywords,object.keywords))),directed=TRUE)
E(callwn2)$weight <- 1
callwn2<-simplify(callwn2, edge.attr.comb=list(weight="sum"))
callwn2
})
names(callwn2L)<-names(table(dt2$word))
plot(callwn2L$dedicate)


callwn2<-lapply(callwnetverbs,function(X) c(X))
names(callwn2)<-unique(dt2$action.verb.text)
callwn2<-callwn2[sapply(callwn2,length)>0]
callwn2<-lapply(names(callwn2),function(X) data.frame("word"=X,"match"=c(callwn2[[X]])))
callwn2<-do.call(rbind,callwn2)
callwn2<-graph_from_data_frame(callwn2,directed=FALSE)
E(callwn2)$weight <- 1
callwn2<-simplify(callwn2, edge.attr.comb=list(weight="sum")
                  edge_attr(callwn2)              
                  get.edge.attribute(callwn2,"weight")                  
                  ig<-as.matrix(get.adjacency(callwn2, type="both",attr="weight",edges=TRUE,names=TRUE))
                  stp = shortest.paths(callwn2)
                  stp[which(is.infinite(stp))]<-100
                  kverbs<-kmeans(ig,500)
                  kverbs$cluster
                  library(matrixStats)
                  
                  clustrename<-data.frame("clust"=1:500,"newword"=unlist(sapply(1:500,function(k) names(kverbs$cluster[kverbs$cluster==k])[which.max(kverbs$centers[k,c(which(kverbs$cluster==k))])])))
                  clnams<-data.frame("oldword"=names(kverbs$cluster),"newword"=clustrename$newword[as.numeric(kverbs$cluster)])
                  clnams[4,]
                  getSynonyms
                  hypeval<-system2("wn",args=c(dt2$action.verb.text[1],"-hypev"))
                  
                  kverbs$cluster[kverbs$cluster==6]
                  ?getWord
                  function (indexterm) 
                  {
                    hypers <- .jcall(indexterm, "[Lcom/nexagis/jawbone/Synset;", 
                                     "getHypernym")
                    sort(unique(unlist(lapply(hypers, getWord))))
                  }
                  
                  src_sqlite("my_db.sqlite3", create = T)
                  tenverbs<-cutree(hclust(as.dist(stp^2)),k=500)
                  ?cutree
                  tenverbs$cluster[tenverbs$cluster==5]
                  
                  clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
                           labels=2, lines=0)
                  
                  bc1<-max(ig)-min(ig)
                  ig<-ig-min(ig)
                  ig<-ig/bc1
                  diag(ig)<-1
                  ig<-round(ig*100)
                  
                  tmcl<-mcl(as.matrix(ig),addLoops=F,allow1=F,max.iter=10)
                  ?kmeans
                  fit <- kmeans(mydata, 5)
                  
                  
                  callwn2<-intergraph::asNetwork(callwn2)
                  library("latentnet")
                  callwn2<-network::as.matrix.network(callwn2,"adjacency")
                  install.packages("MCL")
                  library(MCL)
                  
                  test1<-ergmm(callwn2~bilinear(d = 2), tofit = c("mle"))
                  
                  
                  igraph::get.edge.attribute(callwn2,"weight")
                  
                  
                  
                  
                  
                  related <- getRelatedSynsets(synsets[[1]], "!")
                  net1<-graph_from_data_frame(data.frame("In"=dt2$subject.keywords,"Out"=dt2$object.keyword,'edgename'=dt2$action.verb.text,"topic"=dt2$Topic,"Orig"=dt2$Topic))
                  E(net1)$weight <- 1
                  
                  simplify(net1, edge.attr.comb=list(weight="sum"))
                  install.packages("intergraph")
                  net1net<-intergraph::asNetwork(net1)
                  
                  summary(net1net)[[1]]
                  drs<-net1net %v% "Decision.Rank.Score" 
                  list.edge.attributes(net1net)
                  ideg <- degree(net1net, cmode="indegree") # Indegree for MIDs
                  odeg <- degree(net1net, cmode="outdegree")
                  clo <- closeness(net1net) 
                  list.edge.attributes(net1net)
                  
                  get.edge.attribute(net1net,"Orig")
                  ?summary
                  summary(net1net~edgecov(net1net,"topic"))
                  
                  degrees<-igraph::degree(net1)
                  summary(net1)
                  
                  sort(degrees,decreasing=T)[1:10]
                  
                  test<-igraphob_object6("potato",matchtable,0)
                  tempsmall<-lapply(test,function(K) K[sapply(K, function(X) "name"%in%vertex_attr_names(X[[3]]))])
                  tempsmall<-tempsmall[sapply(tempsmall,length)>0]
                  tempsmall<-unlist(tempsmall, recursive=F)
                  
                  plot(tempsmall[[1]]$Net %u% tempsmall[[2]]$Net  %u% tempsmall[[3]]$Net %u% tempsmall[[4]]$Net)
                  
                  
                  plot(tempsmall[[4]]$Net)
                  
                  mtl1full<-tolower(unlist(matchtable$subject.keywords))
                  mtl1<-unique(tolower(unlist(matchtable$subject.keywords)))
                  topnames<-names(table(mtl1full)[table(mtl1full)>50])
                  library(pbapply)
                  eachlist<-pblapply(topnames, function(WORD){
                    temp<-igraphob_object4(WORD,matchtable,W=0,strictlimit=T)
                    #temp<-igraphob_object5("potato",matchtable,W=0,strictlimit=T)
                    tempsmall<-lapply(temp,function(K) K[sapply(K, function(X) "name"%in%vertex_attr_names(X[[3]]))])
                    tempsmall<-tempsmall[sapply(tempsmall,length)>0]
                    tempsmall<-unlist(tempsmall, recursive=F)
                    tempnets<-lapply(tempsmall,function(X) {
                      Net1<-X$Net %>% set_edge_attr("topic",value=X$Topic) %>% set_edge_attr("Orig",value=X$Orig)
                      #network(as_adjacency_matrix(Net1),matrix.type="adjacency",directed=TRUE,edges=T)
                      Net1
                    })
                    tempnets})
                  eachlist<-unlist(eachlist,recursive=FALSE)
                  
                  templist<-pblapply(eachlist,function(X) list("net"=as_edgelist(X),"covs"=as.data.frame(igraph::get.edge.attribute(X)[c('topic','Orig')])))
                  outlist<-list("elist"=do.call(rbind, lapply(templist, function(X) X$net)),
                                "covslist"=do.call(rbind, lapply(templist, function(X) X$covs)))
                  
                  join_net_by<-function(covar="topic",outlist){
                    alllist<-unique(outlist$covslist[covar][[1]])
                    tlist<-lapply(alllist, function(X) {network::network(outlist$elist[which(outlist$covslist[covar]==X),],matrix.type="edgelist")})
                    names(tlist)<-alllist
                    tlist
                  }
                  
                  fullnet<-join_net_by("topic",outlist)
                  degree(fullnet$`47`)
                  bonpow(fullnet$`39`)
                  ?bonpow
                  network::set.edge.attribute(newnet,"topic",value=outlist$covslist$topic)
                  network::set.edge.attribute(newnet,"orig",value=outlist$covslist$Orig)
                  newnet}
sna::degree(fullnet$`39`)


?degree

newnet
tempnets2<-do.call(union,c(tempnets,byname=TRUE))
igraph::set_edge_attr(tempnets2,"TOPIC",value=edatfun('topic',tempnets2))
igraph::get.edge.attribute(tempnets2)
tempnet3<-network(as_adjacency_matrix(tempnets2),matrix.type="adjacency",directed=TRUE)
?network
tempnet3
get.edge.attribute(tempnet3)
get.edge.id(tempnet3)
edatfun('topic',tempnets2)

rowsum
edatfun<-function(string_entry,netdat){
  newcol<-as.data.frame(edge_attr(tempnets2)[str_detect(edge_attr_names(netdat),string_entry)]) 
  newcol[is.na(newcol)]<-""
  as.character(interaction(newcol,sep="_")) %>% str_extract_all(.,"[0-9]+") %>% sapply(.,function(X) paste(X,collapse="_"))
}
edatfun('topic',tempnets2)
set.edge.attribute(
  newcol
  ?str_extract_all
  
  ?interaction
  ?rowsum
  ?tidyr::unite
  ?network
  network
  ?as_adjacency_matrix  
  library(sna)
  
  tempsmall<-lapply(tempsmall,function(X){
    X$Adj<-network(as_adjacency_matrix(X$Net),matrix.type="adjacency",directed=TRUE,vertex.attr=list('Topic'=X$Topic,'Orig'=X$Orig))
    X
  })
  
  
  library(network)
  tempsmall[[1]]$Net
  
  
  network::matr
  tempsmall[[2]]$Adj
  igraph::edge.attributes(temp)
  temp<-as_adjacency_matrix(temp,edges=T,attr=)
  ?as_adjacency_matrix
  
  
  library("statnet")
  =t1<-lapply(unique(unlist(matchtable$subject.keywords))[1:100], function(X) igraphob_object(X,matchtable,0))
  t12<-union(t1[[1]],t1[[2]])
  for(i in 3:length(t1)){
    t12<-union(t12,t1[[i]])
  }
  t12d3<-t12 %>% igraph_to_networkD3() 
  sankeyNetwork(t12d3$links,t12d3$nodes,"source","target",NodeID="name")
  t12d3$nodes$name
  
  
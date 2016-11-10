install.packages("data.tree")
library("data.tree")
library("plyr")
library("dplyr")
library("data.table")
makenet.radial.json<-function(WORD,mtable){
  WORD
  mtable<-matchtable
  ntr<-dplyr::filter(mtable,str_detect(tolower(subject.keywords),tolower(WORD)))
  baseT <- Node$new(WORD)
  l_ply(ntr$action.lemmatized,function(V){baseT<-baseT$AddChild(V)})
  l_ply(names(baseT)[names(baseT)%in%c(NODE_RESERVED_NAMES_CONST,".__enclos_env__")==FALSE],function(K){
    l_ply(unique(as.character(unlist(filter(ntr,action.lemmatized==K)$object.keywords))),function(O) {if(O==as.character(unlist(filter(ntr,action.lemmatized==K)$object.keywords))[1]) {baseT[[K]]<-baseT[[K]]$AddChild(O)} else {
      baseT[[K]]<-baseT[[K]]$AddSibling(O)}})})
  baseT
  library(igraph)
  baseT
  makeNamesUnique <- function(l) {
    l.names <- names(l$Children)
    # multiple children types
    tab <- table(l.names)
    t.names <- names(tab)
    
    # iterate over types
    for(this.type in seq_along(t.names)) {
      # iterate over duplicate names
      # get an index to this type
      idx <- which(l.names == t.names[this.type])
      for(this.element in seq_along(idx)) {
        # make a copy of this chunk of the tree
        l.sub <- l$Children[[idx[this.element]]]
        # if this is a terminal leaf then re-name and continue
        if(is.null(l.sub$Children)) {
          # print('leaf')
          names(l$Children)[idx[this.element]] <- paste0(t.names[this.type], '_', this.element)
        }
        # otherwise re-name and then step into this element and apply this function recursively
        else {
          # print('branch')
          names(l$Children)[idx[this.element]] <- paste0(t.names[this.type], '_', this.element)
          # fix this branch and splice back into tree
          l$Children[[idx[this.element]]] <- makeNamesUnique(l.sub)
        }
      }
    }
    
    return(l)
  }
  baseT<-makeNamesUnique(baseT)
  plot(as.igraph(baseT))
  
  
  igraphob<-function(WORD,mtable,W){
    ntr<-dplyr::filter(mtable,str_detect(tolower(subject.keywords),tolower(WORD)))
    dt <- data.table(ntr)
    dt$object.keywords<-sapply(dt$object.keywords,function(X) paste(unlist(X),collapse=";"))
    dt$subject.keywords<-sapply(dt$subject.keywords,function(X) paste(unlist(X),collapse=";"))
    dt$action.lemmatized<-sapply(dt$action.lemmatized,function(X) paste(unlist(X),collapse=";"))
    dt$action.verb.text<-sapply(dt$action.verb.text,function(X) paste(unlist(X),collapse=";"))
    dt<-dt[,list(action.verb.text = unlist(strsplit(action.verb.text,";")),object.keywords=unlist(strsplit(object.keywords,";")),subject.keywords=unlist(strsplit(subject.keywords,";"))),by = sentence]
    dt<-na.omit(dt)
    net1<-graph_from_data_frame(rbind(data.frame("In"=dt$subject.keywords,"Out"=dt$action.verb.text),data.frame("In"=dt$action.verb.text,"Out"=dt$object.keywords)))
    E(net1)$weight <- 1
    netsimp<-simplify(net1,edge.attr.comb=list(weight="sum","ignore"))
    E(netsimp)$width <- E(netsimp)$weight
    netsimp <- delete_edges(netsimp, E(netsimp)[weight<=W])
    netsimp<-simplify(netsimp,edge.attr.comb=list(weight="sum","ignore"))
    bad.vs<-V(netsimp)[degree(netsimp) == 0]
    netsimp <-delete.vertices(netsimp, bad.vs)
    netsimp}
  library(stringr)
  netry<-igraphob("varieties",filter(matchtable,TopTopics%in%c(21,42,19)),1)
  
  plot(netry,edge.arrow.size=.4,layout=layout_with_fr(netry))
  
  netry<-igraphob(slabs$marginal$prob[21,1],filter(matchtable,TopTopics%in%c(21,42,19)),1)
  library(networkD3)
  ig1<-igraph_to_networkD3(netry)
  head(ig1)
  forceNetwork(Links = ig1$links, Nodes = ig1$nodes,Source = 'source', Target = 'target', NodeID = 'name',Value='value',Group=1)
  ?forceNetwork
  
  library(stm)
  stm::
  par(mfrow=c(1,1))
  igraphob(slabs$marginal$prob[21,1],filter(matchtable,TopTopics%in%c(21,42,19)),5) %>% plot(.,edge.arrow.size=.4,layout=layout_with_fr(.),vertex.color="#b7a57a",vertex.label.color="#4b2e83",label.cex=2)
  igraph.
  
  igraphob("varieties",filter(matchtable,TopTopics%in%c(21,19,42,34)),1) %>% plot(.,edge.arrow.size=.4,layout=layout_with_fr(.),vertex.color="#b7a57a",vertex.label.color="#4b2e83",main="varieties")
  igraphob("research",filter(matchtable,TopTopics%in%c(21,19,42,34)),3) %>% plot(.,edge.arrow.size=.4,layout=layout_with_fr(.),vertex.color="#b7a57a",vertex.label.color="#4b2e83",main="research")
  
  igraph::union(igraphob("seed",filter(matchtable,TopTopics%in%c(21,19,42,34)),2),igraphob("varieties",filter(matchtable,TopTopics%in%c(21,19,42,34)),2)) %>% plot(.,edge.arrow.size=.4,layout=layout_with_fr(.),vertex.color="#b7a57a",vertex.label.color="#4b2e83",main="varieties+seed")
  
  
  
  
fullgraph<-lapply(c(slabs$marginal$prob[c(21,42,19,34),]),function(X) igraphob(X,filter(matchtable,TopTopics%in%c(21,42,19,34)),2))
par(mfrow=c(2,2))
fullgraph<-lapply(c(slabs$marginal$prob[c(21,42,19,34),1]),function(X) igraphob(X,filter(matchtable,TopTopics%in%c(21,42,19,34)),3))


l_ply(1:4, function(i) {plot(fullgraph[[i]],main=c(slabs$marginal$prob[c(21,42,19,34),1])[i])})



  union(byname = "auto")
  
  ?union
  plot(netry
       ?simplify
       
       plot(d1))
  
  
  dt<-dt[,list(action.verb.text = unlist(strsplit(action.verb.text,";")),object.keywords=unlist(strsplit(object.keywords,";")),subject.keywords=unlist(strsplit(subject.keywords,";"))),by = sentence]
  dt<-na.omit(dt)
  d1<-graph_from_data_frame(rbind(data.frame("In"=dt$subject.keywords,"Out"=dt$action.verb.text),data.frame("In"=dt$action.verb.text,"Out"=dt$object.keywords)))
  class(d1)
  
  
  l1<-lapply(1:nrow(ntr),function(i) {
    tryCatch({part1<-data.frame("enter"=as.character(unlist(ntr$subject.keywords[i])),"out"=unlist(as.character(ntr$action.lemmatized[i])))
    part1<-part1[str_detect(part1[,1],fixed(tolower(WORD))),]
    part2<-data.frame("enter"=unlist(as.character(ntr$action.lemmatized[i])),"out"=unlist(ntr$object.keywords[i]))
    part2<-dplyr::filter(part2,str_detect(enter,as.character(part1$out)))
    rbind(part1,part2)
    },error=function(e){NA})
  })
  l1<-na.omit(l1)
  l1<-do.call(rbind,l1)
  chartmaker<-function(E) {list(name=tolower(E),children=lapply(unique(dplyr::filter(l1,str_detect(tolower(enter),tolower(E)))$out),function(X){list(name=as.character(X))}))}
  lch<-chartmaker(tolower(WORD))
  for(i in 1:length(lch[[2]])) {lch[[2]][[i]]$children<-chartmaker(lch[[2]][[i]]$name)$children}
  lch}
acme <- Node$new(
  
  
  library(igraph)
  library(stringr)
  library(data.table)
  library(networkD3)
  ?forceNetwork
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
  
  
    
    fontSize=24, opacity = 0.9, bounded = TRUE, opacityNoHover = TRUE
    

  ?sankeyNetwork
    }
  igraphob_object_force("varieties",matchtable,2,inputWord=T,sankey=T,verbfilter=c(),strictlimit=TRUE)
  
  igraphob_object_force("varieties",matchtable,0,inputWord=T,sankey=T,verbfilter=c("provide","help","promote"),strictlimit=TRUE)
  igraphob_object_force("varieties",matchtable,0,inputWord=T,sankey=T,verbfilter=c("provide","help","promote"),strictlimit=FALSE)
  
  
  igraphob_object_force("yield gap",matchtable,0,inputWord=F,sankey=T,verbfilter=c(),strictlimit=TRUE)
  igraphob_object_force("yield gap",matchtable,0,inputWord=T,sankey=T,verbfilter=c(),strictlimit=FALSE)
  
  
  
  ?sankeyNetwork
  
  igraphob_object_force("nutrient",matchtable,0,inputWord=F,sankey=T,verbfilter=c("be"),strictlimit=TRUE)
  
    
  ?forceNetwork
  %>% plot(.,main="production as object",edge.arrow.size=.4,layout=layout_with_fr(.),vertex.color="#b7a57a",vertex.label.color="#4b2e83") 
  ?plot.igraph
  
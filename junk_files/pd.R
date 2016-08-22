#probably delete

unlist((as.character(print(corpEnt3[[2]][1]))))[[1]]
lubridate::ymd_hms(
  ?ymd_hms
  as.Date(corpEnt3[[2]])
  sapply(lapply(corpEnt3[[2]],function(x){unlist(lubridate::ymd_hms(as.character(x)),use.names=FALSE)}),function(x){x[[1]]})
  unlist(,use.names=FALSE)
  
  corpEntMet<-lapply(corpEnt3,function(x){
    for(i in 1:length(x[[1]]$meta)){if(length(x[[1]]$meta[i])<1){x[[1]]$meta[i][[1]]<-NA}}
    data.frame("author"=x[[1]]$meta$author,"datetimestamp"=x[[1]]$meta$datetimestamp,"heading"=x[[1]]$meta$heading,"id"=x[[1]]$meta$id,"language"=x[[1]]$meta$language,"origin"=x[[1]]$meta$origin,check.rows=FALSE)})
  
  ?data.frame
  
  corpEntMet<-do.call(rbind,corpEntMet)
  head(corpEntMet)
  corpEntMet<-lapply(corpEnt2,function(x){t(matrix(x[[1]]$meta,dimnames=list(names(x[[1]]$meta))))})
  
  corpEntMet<-lapply(corpEntMet,function(x){
    tryCatch({x[[2]]<-lubridate::ymd_hms(x[[2]])},error=function(e){x$datetimestamp<-NA})
    x[[1]]<-paste(unlist(x[[1]]),collapse=";")
    lapply(3:length(x),function(i){x[[i]]<-paste(unlist(x[[i]],use.names=FALSE,recursive=TRUE),collapse=";",sep=";")})
    x})
  corpEntMet2<-data.frame(id=1:length(corpEntS))
  lapply(1:length(corpEntMet),function(i){
    corpEntMet2[,i]<-NA
    corpEntMet2[,i]<-unlist(corpEntMet[[i]],use.names=FALSE)})
  corpEntMet
  corpEntMet<-as.data.frame(do.call(rbind,corpEntMet))
  
  ,function(i){corpEntMet[names(corpEntMet)[i]]<-unlist(corpEntMet[names(corpEntMet)[i]],use.names=FALSE)})
unlist(corpEntMet$author)
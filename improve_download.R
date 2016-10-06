library(tidyjson)  
install.packages('tidyjson')
# this library
library(dplyr)
library(tidyjson)
??fread
library(jsonlite)
tempfname<-tempfname[1:100]

%>% rbind.pages()
?fromJSON
idallfile
processFolder<-function(working.filepath, PREPFRAME){
tempfname<-list.files(file.path(working.filepath,"ALCHEMY"),full.names=T)
nosize<-sapply(tempfname, function(X) file.info(X)$size==0)
templist<-pblapply(tempfname[which(nosize==FALSE)], function(X){fromJSON(X,flatten=T)$relations %>% as.data.frame() %>% mutate(.,"filename"=as.character(X))})
tempframe<-rbind.pages(templist)
tempframe$comboID=str_extract(tempframe$filename,"[0-9]+")
recombine$comboID<-1:nrow(recombine)
recombineOut<-plyr::join(tempframe,recombine, by="comboID",type="left",match="all")
return(recombineOut)
}


pfolder<-processFolder(file.path("..","..","tempfiles"),recombine)

keepwords<-sapply(allwords[1:65],function(X) which(str_detect(pfolder$action.lemmatized,X)))
keepwords<-unique(unlist(keepwords))
pfolderSmall<-pfolder[keepwords,]
head(pfolderSmall)
attach("savedObjects/.RData",name="tempf")
saveRDS(get("BASE_INPUT","tempf"),"savedObjects/baseInput2.rds")
detach("tempf")
BASE_INPUT<-
pfolder$subject.keywords
st1<-readRDS("savedObjects/st1.rds")
BASE_INPUT<-readRDS("savedObjects/baseInput2.rds")
sort(unique(as.numeric(str_extract(tempframe$filename,"[0-9]+"))))[1:10]
recombine<-readRDS("savedObjects/recombine.rds")



head(recombine)


"../../tempfiles/ALCHEMY"

?rbind.pages

temp<-jsonlite::fromJSON("testjson.txt",simplifyDataFrame = FALSE)
?fromJSON

temp$relations %>% tbl_json()
%>% gather_array()
,flatten=FALSE,simplifyVector=FALSE)
tbl_json(temp)
tidyjson::tbl_json(temp$relations,temp$relations)
?tbl_json
?gather_array


temp %>% gather_array()
paste(temp,collapse="") %>% gather_array()
class(temp)
temp
temp %>% gather_array
?stream_in
temp<-jsonlite::fromJSON(tempfname)
temp %>% gather_array 

?list.files
tidyjson::
?tidyjson
purch_json

purch_items <- purch_json %>% gather_array %>% spread_values(person = jstring("name")) %>% enter_object("purchases") %>% gather_array %>% spread_values(purchase.date = jstring("date")) %>% enter_object("items") %>% gather_array %>%  spread_values(item.name = jstring("name"),item.price = jnumber("price")) %>% select(person, purchase.date, item.name, item.price)
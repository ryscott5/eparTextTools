#
source("textFunctions.R")
source("TopicFunctions.R")

#Specify Working Folder
getwd()
workingfolder<-"../Ag.Nutrition.Grants"

messages<-readMails("../Fullset/Fullset_Demo","../mails")

table(reader::get.ext(list.files("../Fullset/Fullset_Demo")))
#removed .bmp and .db files manually
filelist<-list.files("../Fullset/Fullset_Demo")[unique(unlist(sapply(c(".pdf",".doc",".docx",".DOC"),function(X) which(str_detect(list.files("../Fullset/Fullset_Demo"),fixed(X))))))]
fileslist<-lapply(file.path("../Fullset/Fullset_Demo",filelist),getTextR)

baseinput1<-readRDS("../Ag.Nutrition.Grants/base_input1.rds")
head(baseinput1$SentFrame)

inspect(head(baseinput1$SentFrame))

fileslist<-fileslist[sapply(fileslist, function(X){length(X[[1]])})>1]
jgc()

which(sapply(tcorp,function(X) length(content(X)))<5)
tcorp<-do.call(c,fileslist)
saveRDS(tcorp,file.path("..",workingfolder,"corpus.rds"))

#clean up workspace
saveRDS(PreTopicFrame(tcorp[1:256],1),file.path("..",workingfolder,"base_input1.rds"))

jgc()
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
org.annotate<-Maxent_Entity_Annotator(language = "en", kind="organization", probs = FALSE,model = NULL)
pers.annotate<-Maxent_Entity_Annotator(language = "en", kind="person", probs = FALSE,model = NULL)
location.annotate<-Maxent_Entity_Annotator(language = "en", kind="location", probs = FALSE,model = NULL)

saveRDS(PreTopicFrame(tcorp[257:length(tcorp)],1),file.path("..",workingfolder,"base_input2.rds"))
                
tcorp[257:length(tcorp)]

baseinput1<-readRDS("../Ag.Nutrition.Grants/base_input1.rds")
baseinput2<-readRDS("../Ag.Nutrition.Grants/base_input2.rds")

mergeins<-function(x1,x2) {list("SentFrame"=rbind(x1[[1]],x2[[1]]),"Annotations"=c(x1[[2]],x2[[2]]),"processed"=c(x1[[3]],x2[[3]]),"out"=c(x1[[4]],x2[[4]]))}
baseinput<-mergeins(baseinput1,baseinput2)
rm(baseinput1,baseinput2)
#here we add opportunity labels to documents 

nex<-read.csv("../Fullset/fullset.csv",stringsAsFactors=FALSE)
head(nex)
nexjoin<-plyr::join(data.frame("name"=baseinput$SentFrame$id),data.frame("name"=basename(as.character(nex$path)),"OpID"=as.character(nex$Opportunity.ID)),type="left",match="first")
baseinput$SentFrame$OpID<-nexjoin$OpID
nexjoin2<-plyr::join(data.frame("name"=baseinput$out$meta$id),data.frame("name"=basename(as.character(nex$path)),"OpID"=as.character(nex$Opportunity.ID)),type="left",match="first")
baseinput$out$meta$OpID<-nexjoin2$OpID
rm("nexjoin","nexjoin2","nex")

buildcliff()
startcliff()
library(httr)
pred1<-PredictCountryByDoc(baseinput)
rm(f1)
stopcliff()
head(baseinput$out)
baseinput$out$meta$Orig
baseinput1<-readRDS(file.path(workingfolder,"base_input1.rds"))
baseinput2<-readRDS(file.path(workingfolder,"base_input2.rds"))
baseinput$out$meta<-rbind(baseinput1$out$meta[,1:11],baseinput2$out$meta[,1:11])
names(baseinput2$out$documents)<-as.character(as.numeric(names(baseinput2$out$documents))+as.numeric(names(baseinput1$out$documents))[length(names(baseinput1$out$documents))])

l_ply(names(baseinput2$out$documents),function(X) baseinput2$out$documents$X[1,]<-baseinput2$out$documents$X[1,]+length(baseinput1$out$vocab))
baseinput$out$documents<-c(baseinput1$out$documents,baseinput2$out$documents)
rm(baseinput2)
baseinput$processed <-textProcessor(baseinput$SentFrame$SnE,metadata=baseinput$SentFrame,sparselevel=1)
baseinput$out <- prepDocuments(baseinput$processed$documents,baseinput$processed$vocab,baseinput$processed$meta,lower.thresh=10)
pred1<-read.csv(file.path(workingfolder, "countrypredictions1.csv"),stringsAsFactors=FALSE)
head(pred1)
baseinput$out$meta
baseinput$out$meta<-reflectCountryCol(baseinput$out$meta,pred1,50,FALSE)

workingfolder<-"../Ag.Nutrition.Grants"
saveRDS(baseinput,file.path(workingfolder,"basefile.rds"))
baseinput$out[[1]]
write.csv(pred1,file.path(workingfolder,"countrypredictions1.csv"))

form1<-paste("~as.factor(Orig)",paste(select.list(colnames(baseinput$out$meta),multiple=TRUE),sep="",collapse="+"),sep="+")
writeLines(form1,file.path(workingfolder,"formula1.txt"))

max(unlist(baseinput$out$documents))
length(baseinput$out$vocab)
length(baseinput$out$documents)
length(baseinput$out$documents)
nrow(baseinput$out$meta)
baseinput$out$meta$OpID<-as.character(baseinput$out$meta$OpID)
baseinput$out$meta$OpID[is.na(baseinput$out$meta$OpID)]<-'missing'

system("R CMD BATCH --no-restore run_topic_in_background.R", wait=FALSE)
st1<-readRDS(file.path(workingfolder,"topicmodel.rds"))
baseinput$top.topics<-max.col(st1$theta))
?saveWidget
dir.create("../results")
toplabels<-stm::labelTopics(st1)
head(toplabels)
library(wordcloud)
stmpout<-plot.STM(st1, type="perspectives", topics=c(1,2))
?toLDAvis
toLDAvis(st1, baseinput$out$documents,out.dir="../results/topicmodel",open.browser = FALSE)
baseinput$top.topics<-max.col(st1$theta)
ggplot()+geom_bar(aes(x=baseinput$out$meta$OpID[which(baseinput$top.topics==28)]))

topOps<-function(TOPICNUMBER,howMany,topicmodel,out){
topicWR<-function(TOPICNUMBER){
temp<-data.frame("OpID"=out$meta$OpID,"score"=topicmodel$theta[,TOPICNUMBER],"words"=sapply(out$documents,ncol))
ddply(temp,.(OpID),summarise,"sumwordweight"=sum(score*c(words/sum(words)))/length(score))}
topictry1<-topicWR(28)
data.frame("OpID"=as.character(topictry1$OpID[sort(topictry1$sumwordweight,index.return=TRUE,decreasing=TRUE)$ix[1:howMany]]),"Score"=topictry1$sumwordweight[sort(topictry1$sumwordweight,index.return=TRUE,decreasing=TRUE)$ix[1:howMany]])
}

topID<-function(TOPICNUMBER,howMany,topicmodel,out){
  topicWR<-function(TOPICNUMBER){
    temp<-data.frame("id"=out$meta$id,"score"=topicmodel$theta[,TOPICNUMBER],"words"=sapply(out$documents,ncol))
    ddply(temp,.(id),summarise,"sumwordweight"=sum(score*c(words/sum(words)))/length(score))}
  topictry1<-topicWR(28)
  data.frame("id"=as.character(topictry1$id[sort(topictry1$sumwordweight,index.return=TRUE,decreasing=TRUE)$ix[1:howMany]]),"Score"=topictry1$sumwordweight[sort(topictry1$sumwordweight,index.return=TRUE,decreasing=TRUE)$ix[1:howMany]],"Topic"=TOPICNUMBER)
}
?labelTopics
stm::l
sum(topOps(28,50,st1,baseinput$out)$Score)
library(shiny)
dir.create("../../bucket1/Wellcome")
getwd()
toLDAvis(st1, baseinput$out$documents,open.browser = FALSE,,out.dir="../../bucket1/Wellcome/Topic")
?toLDAvis

shinyApp(ui=fluidPage(
  visOutput("vis1"),
  DT::dataTableOutput("tab1")),
  server=function(input,output){
  output$vis1<-renderVis({toLDAvis(st1, baseinput$out$documents,open.browser = FALSE)})
  output$tab1<-renderDataTable({DT::datatable(topID(28,10,st1,baseinput$out))})
  })
library("gistr")
DT::re

?toLDAvis
?shinyApp

?datatable
#here, we read from a table of verbs to the wd dataframe. The function allows you to edit a google docs frame shared from the address, so you can add, subtract words. You also could replace the reading of the csv with a call to a local dataframe.

wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)
allwords<-c(wd$Up.Words,wd$Down.Words)

AnnotatesLarge<-AnnotateVerbsTopicJoin(allwords,baseinput$processed,baseinput$out,baseinput$Annotations,baseinput$SentFrame,baseinput$top.topics)
AnnotatesSmaller<-CombinationFrame(AnnotatesLarge)
rm(AnnotatesLarge)

saveRDS(AnnotatesSmaller,file.path(workingfolder,"AnnotationFrame.rds"))


ProcessedANNS<-ProcessforAPI(AnnotatesSmaller)
saveRDS(ProcessedANNS,file.path(workingfolder,"ProcessedFrame.rds"))

ProcessedANNS<-readRDS(file.path(workingfolder,"ProcessedFrame.rds"))

nrow(ProcessedANNS)

#edit runAlchemy and source

FillFolder(ProcessedANNS,workingfolder)
library(jsonlite)
Frame1<-processFolder(workingfolder,ProcessedANNS)

rm(BASE_INPUT)
rm(BASEINPUT)
saveRDS(Frame1,file.path(workingfolder,"ParsedFrame.rds"))

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

 
matchtable<-frametable(Frame1,baseinput,0)

dir.create("../AG_NUTRITION_RESULTS")
tableapp(matchtable,st1)

save("matchtable","st1",file="../AG_NUTRITION_RESULTS/agnutshinydata.R")

workpage<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1jRTIHINPLvE8w-_xIBxvxId4n_fFT7Qsg2tjCNrUm3A/pub?output=csv")))
workpage$Ag

?sageLabels
toplabs<-stm::labelTopics(st1,workpage$Ag)$prob
.heldout()
tcorp<-readRDS(file.path(workingfolder,"corpus.rds"))
fulldocs1<-textProcessor(unlist(sapply(tcorp[1:5], `[`, "content")))

install.packages("d3Network")
library(d3Network)
?d3Sankey
d3Sankey(
tempframe<-data.frame(source=c(0,1,2,3,4),target=c(1,2,3,4,4))
tempframe$value<-c(100,100,100,100,100)
nodeframe<-data.frame("name"=c("bacon","sandwich","pie","cake","lemon"))
library(networkD3)

sankeyNetwork(Links=tempframe,Nodes=nodeframe,Source="source",Target="target",Value="value",NodeID="name",fontSize=24,nodeWidth=30)

matchtable$object.keywords

ntr<-filter(matchtable,str_detect(subject.keywords,"micronutrients"))

l1<-lapply(1:nrow(ntr),function(i) {
tryCatch({part1<-data.frame("enter"=as.character(unlist(ntr$subject.keywords[i])),"out"=unlist(as.character(ntr$action.lemmatized[i])))
  part1<-part1[str_detect(part1[,1],fixed("micro")),]
  part2<-data.frame("enter"=unlist(as.character(ntr$action.lemmatized[i])),"out"=unlist(ntr$object.keywords[i]))
  part2<-filter(part2,str_detect(enter,as.character(part1$out)))
  rbind(part1,part2)},error=function(e){NA})})
which(is.na(l1))
l1<-na.omit(l1)
l1<-do.call(rbind,l1)

l1<-na.omit(l1)

networkD3::simpleNetwork(l1,Source="enter",Target="out",fontSize=20)

l12<-data.frame("name"=as.factor(unique(c(as.character(l1$enter),as.character(l1$out)))),"id"=0:c(length(unique(c(as.character(l1$enter),as.character(l1$out))))-1))


l1$enter<-as.numeric(as.character(mapvalues(l1$enter, from = l12$name, to = l12$id)))
l1$out<-as.numeric(as.character(mapvalues(l1$out, from = l12$name, to = l12$id)))
l1$value<-10
l1
childmaker<-function(K) {lapply(K,function(X) list(name=X))}


chartmaker("micronutrients")
?diagonalNetwork
dplyr::f
makenet<-function(WORD){
ntr<-dplyr::filter(matchtable,str_detect(tolower(subject.keywords),tolower(WORD)))
l1<-lapply(1:nrow(ntr),function(i) {
  tryCatch({part1<-data.frame("enter"=as.character(unlist(ntr$subject.keywords[i])),"out"=unlist(as.character(ntr$action.lemmatized[i])))
  part1<-part1[str_detect(part1[,1],fixed(tolower(WORD))),]
  part2<-data.frame("enter"=unlist(as.character(ntr$action.lemmatized[i])),"out"=unlist(ntr$object.keywords[i]))
  part2<-filter(part2,str_detect(enter,as.character(part1$out)))
  rbind(part1,part2)
  },error=function(e){NA})
  })
l1<-na.omit(l1)
l1<-do.call(rbind,l1)
chartmaker<-function(E) {list(name=E,children=lapply(unique(dplyr::filter(l1,enter==E)$out),function(X){list(name=as.character(X))}))}
lch<-chartmaker(tolower(WORD))
for(i in 1:length(lch[[2]])) {lch[[2]][[i]]$children<-chartmaker(lch[[2]][[i]]$name)$children}
lch %>% diagonalNetwork(fontSize=30,linkColour = "#000")
}
makenet.radial<-function(WORD,mtable){
  ntr<-dplyr::filter(mtable,str_detect(tolower(subject.keywords),tolower(WORD)))
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
  lch %>% radialNetwork(fontSize=30,linkColour = "#000")
}
library(plyr)
library(dplyr)
library(networkD3)
makenet("micronutrients")
makenet("anemia")
matchtable$TopTopics%in%
  library(stringr)
makenet.radial("protein",dplyr::filter(matchtable,TopTopics%in%workpage$Ag))
makenet.radial("protein",dplyr::filter(matchtable,TopTopics%in%workpage$Nut))

breedtopics<-dplyr::filter(matchtable,TopTopics%in%c(21,19,42))
table(unique(unlist(breedtopics$subject.keywords)))
                           
makenet.radial("vari",dplyr::filter(matchtable,TopTopics%in%c(21,19,42)))
makenet.radial("drought",dplyr::filter(matchtable,TopTopics%in%c(21,19,42)))
makenet.radial("food",dplyr::filter(matchtable,TopTopics%in%c(21,19,42)))

library(stm)
?findThoughts
slabs<-stm::sageLabels(st1,n=10)
slabs$marginal$prob[21,]
findThoughts(st1,21)

makenet.radial.json<-function(WORD,mtable){
  ntr<-dplyr::filter(mtable,str_detect(tolower(subject.keywords),tolower(WORD)))
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

makenet.radial.json(slabs$marginal$prob[21,][1],dplyr::filter(matchtable,TopTopics%in%c(21,19,42)))
lapply()


l_ply(makenet.radial.json(slabs$marginal$prob[21,][1],dplyr::filter(matchtable,TopTopics%in%c(21,19,42)))[[2]]
      
doublechild<-function(V){
V<-makenet.radial.json(slabs$marginal$prob[21,][1],dplyr::filter(matchtable,TopTopics%in%c(21,19,42)))[[2]][[1]]
l_ply(V[[2]],function(X){
  X<-V[[2]][[1]]
  X$children=makenet.radial.json(X$name,filter(matchtable, TopTopics%in%c(21,19,42)))$children
  V}




makenet.radial("seed",dplyr::filter(matchtable,TopTopics%in%c(21,19,42)))
library(plyr)
library(networkD3)
library(dplyr)
library(stringr)

makenet.radial(slabs$marginal$prob[21,][1],filter(matchtable,TopTopics%in%c(21,19,42)))
matchtable$To


makenet.radial("survey",dplyr::filter(matchtable,TopTopics%in%c(22,53)))
makenet.radial("iron",dplyr::filter(matchtable,TopTopics%in%workpage$Nut))

makenet.radial("potato",dplyr::filter(matchtable,TopTopics%in%workpage$Ag))

saveRDS(matchtable,file="../Ag.Nutrition.Grants/matchtab.rds")



shinyApp(ui=fluidPage(sidebarPanel(textInput("Word","Word","potato"),selectInput("fvar","Filter Variable",names(matchtable),"TopTopics"),textInput("f2","Filter Text","1")),mainPanel(radialNetworkOutput("radout"))),server=function(input,output){
output$radout<-networkD3::renderRadialNetwork({makenet.radial("potato",matchtable[as.character(input$fvar)%in%as.character(input$f2),])})})
})
Nts<-filter(matchtable,TopTopics%in%workpage$Nut)
table(unlist(Nts$subject.keywords))




chartmaker(X$name)
lch[[2]] %>% .[[1]]
l1$children[[1]]$children<-
chartmaker("offer")
charmaker2("micronutrients")

chartmaker("offer")
lapply(unique(l1$enter), chartmaker)[[1]] %>% diagonalNetwork(fontSize=30,linkColour = "#000")
/

E="micronutrients"
chartmaker<-function(E) {list(name=E,children=lapply(table(filter(l1,enter==E)$out),function(X){list(name=X)}))}

lapply(1:nrow(l1), function(i) list(name=l1$enter[i],children=l1$out[i]))

rsplit <- function(x) {
  x <- x[!is.na(x[,1]),,drop=FALSE]
  if(nrow(x)==0) return(NULL)
  if(ncol(x)==1) return(lapply(x[,1], function(v) list(name=v)))
  s <- split(x[,-1, drop=FALSE], x[,1])
  unname(mapply(function(v,n) {if(!is.null(v)) list(name=n, children=v) else list(name=n)}, lapply(s, rsplit), names(s), SIMPLIFY=FALSE))
}

temp
temp)
l3<-filter(l1,enter==enter[[1]])

?dlply
diagonalNetwork(l1,Source="enter",Target="out",fontSize=20)
?simpleNetwork
networkD3::simpleNetwork(l1,Source="enter",Target="out",fontSize=20)

sankeyNetwork(Links = l1, Nodes = l12, Source = 'enter', Target = 'out',Value='value',NodeID = "name", fontSize = 24, nodeWidth = 30)

sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source', Target = 'target', Value = 'value', NodeID = 'name',units = 'TWh', fontSize = 12, nodeWidth = 30)


  ntr$subject.keywords
length(unlist(ntr$action.lemmatized))
                       unlist(),"act"=unlist(ntr$action.lemmatized)
                     
                     
                     "ob"=unlist(ntr$object.keywords))


netframe$key[1]


URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/','master/JSONdata/energy.json')
energy <- jsonlite::fromJSON(URL)

# Plot
sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source', Target = 'target', Value = 'value', NodeID = 'name',units = 'TWh', fontSize = 12, nodeWidth = 30)

desc<-read.csv("../Fullset/fullset.csv")
head(desc)
matchtable<-readRDS(file.path(workingfolder,"matchtab.rds"))
reop<-join(data.frame("OpID"=desc$Opportunity.ID,"basename"=basename(as.character(desc$path))),data.frame("basename"=baseinput$SentFrame$id,"Orig"=baseinput$SentFrame$Orig),type="left",match="first")
matchtable<-join(matchtable,reop,match="first")
head(matchtable)
matchtable$basename
saveRDS(matchtable,file.path(workingfolder,"matchtable2.rds"))
matchtable<-unique(matchtable)
getwd()
data_mapper(read.csv("../Ag.Nutrition.Grants/countrypredictions1.csv"),unique(baseinput$SentFrame$OpID))

library(plyr)
library(dplyr)
library(RCurl)
library(jsonlite)
library(plotly)
library(RCurl)
library(httr)

nex<-read.csv("../nonExcel.csv",stringsAsFactors=FALSE)
nexjoin<-plyr::join(data.frame("name"=BASE_INPUT$SentFrame$id),data.frame("name"=basename(as.character(nex$path)),"OpID"=as.character(nex$Opportunity.ID)),type="left",match="first")
BASE_INPUT$SentFrame$OpID<-nexjoin$OpID
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
  f1<-join(BASE_INPUT$SentFrame[,c("OpID","Orig")],countries)
  f1<-unique(f1)
  f1$nchars<-nchar(fullc$fullc)
  f1
}
runMap<-function(FILENAMEQUOTED,path.file=FALSE){shiny::shinyApp(
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
      plot_ly(d1,z=value,hoverinfo="text", locations = ccode2, type = 'choropleth', marker = list(line = l),color = value, colors = 'Blues',text=hover,colorbar = list(title = 'Country Relevance'),source="select") %>% layout(title ="<br>Source:<a href='https://github.com/c4fcm/CLIFF'>CLIFF</a>", geo = g)
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
reflectCountryCol<-function(MATCHFRAME,pred2,howmany,binomial=FALSE){
  pred2[,3:c(ncol(pred2)-1)][is.na(pred2[,3:c(ncol(pred2)-1)])]<-0
  if(binomial==TRUE){
  pred2[,3:c(ncol(pred2)-1)][pred2[,3:c(ncol(pred2)-1)]>0]<-1
  }
  maxCs<-colnames(pred2[,3:c(ncol(pred2)-1)])[sort(colSums(pred2[,3:c(ncol(pred2)-1)]),index=TRUE,decreasing=TRUE)$ix][1:howmany]
  cntjoin<-join(MATCHFRAME,pred2[,c("OpID","Orig",maxCs)], by=c("OpID","Orig"))
  return(cntjoin)}

pred2<-pred1

head(pred2)
head(BASE_INPUT$SentFrame)

head(reflectCountryCol(BASE_INPUT,pred1,30,FALSE))
crc<-reflectCountryCol(BASE_INPUT$out$meta,pred1,30,FALSE)

BASE_INPUT$SentFrame$ents[7]
pnt1<-textProcessor(documents=BASE_INPUT$SentFrame$ents,metadata=crc[,c(2,c(ncol(crc)-10):ncol(crc))])
pnt1$meta$Orig<-as.factor(pnt1$meta$Orig)
pnt2<-prepDocuments(pnt1$documents,pnt1$vocab,pnt1$meta)
pnt2$meta$OpID<-as.character(pnt2$meta$OpID)
pnt2$meta$OpID[is.na(pnt2$meta$OpID)]<-"unknown"
tr1<-stm(pnt2$documents,pnt2$vocab,K=20,prevalence=eval(parse(text=paste("~",paste(colnames(pnt2$meta),collapse="+",sep=""),sep=""))),data=pnt2$meta)
nrow(pnt2$meta)


colnames(BASE_INPUT$SentFrame)
stm(,)
library(stm)
?prepDocuments
maxCs
head(cntjoin)
startcliff()
checkcliff()
pred1<-PredictCountryByDoc(BASE_INPUT)
stopcliff()
runMap(pred1)






ccodes<-read.csv(textConnection(getURL("http://data.okfn.org/data/core/country-codes/r/country-codes.csv")))
mapper<-function(CountryPredictions,OPPORTUNITY,label_title=OPPORTUNITY){
  #replace f1 with CountryPredictions
  gchars<-ddply(CountryPredictions,.(OpID),summarise,"charsum"=sum(nchars))
  CountryPredictions<-join(CountryPredictions,gchars)
  CountryPredictions$weight<-CountryPredictions$nchars/CountryPredictions$charsum
  CountryPredictions<-cbind(CountryPredictions[,1:2],CountryPredictions[,3:c(ncol(CountryPredictions)-3)]*CountryPredictions$weight)
  ftemp<-filter(CountryPredictions, OpID%in%OPPORTUNITY)
  tframe<-reshape2::melt(colSums(ftemp[,3:ncol(ftemp)],na.rm=TRUE)/sum(ftemp[,3:ncol(ftemp)],na.rm=TRUE))
  tframe$countryids<-row.names(tframe)
  tframe$ccode2<-join(data.frame("ISO3166.1.Alpha.2"=tframe$countryids),ccodes)$ISO3166.1.Alpha.3
  tframe$nameC<-join(data.frame("ISO3166.1.Alpha.2"=tframe$countryids),ccodes)$official_name_en
  tframe$hover<-paste(tframe$nameC,": ",round(tframe$value*100)/100,sep="")
  g <- list(showframe = FALSE,showcoastlines = TRUE,projection = list(type = 'mollweide'))
  l <- list(color = toRGB("grey"), width = 0.5)
  plot_ly(tframe,z=value,hoverinfo="text", locations = ccode2, type = 'choropleth', marker = list(line = l),color = value, colors = 'Blues',text=hover,colorbar = list(title = 'Country Relevance')) %>% layout(title = paste("'Opportunity Number:",label_title,"<br>Source:<a href='https://github.com/c4fcm/CLIFF'>CLIFF</a>'", sep=""), geo = g)}
length(unique(BASE_INPUT$SentFrame$OpID))
write.csv(pred1,"pred1.csv")
stopcliff()

mapper(pred1,pred1$OpID[1:310],"All Opportunities")

mapper(f1$OpID[310])
makemap<-function(FILENAME){
  temp<-readLines("Map_Application.txt")
  temp<-gsub("SPECIFY_CSV_FILE",FILENAME,temp)
  writeLines(temp,"mapoutfile.Rmd")
  rmarkdown::run(file="mapoutfile.Rmd",dir=".",output_format="html_document")
}
makemap("'pred1.csv'")

f1$OpID[15]
colnames(BASE_INPUT$SentFrame)
filter(BASE_INPUT$SentFrame, OpID=="OPP48776")$SC[30]
colSums(f1[,3:ncol(f1)],na.rm=T)/sum(f1[,3:ncol(f1)],na.rm=T)
?sort
paste(f1[sort(f1[,c("IN")],index.return=TRUE)$ix,]$OpID[1:3])

head(countries)
t(data.frame(table(res$name)))
table(BASE_INPUT$SentFrame$OpID)


system('sudo docker run -p "8080:8080" -d --name cliff cliff:2.1.1')

system('sudo docker stop cliff')

system('sudo docker ps')
baseC<-httr::POST(url="http://localhost:8080/CLIFF-2.1.1/parse/text",query=list(replaceAllDemonyms="true",q="Zambia"))
content(baseC)
baseC<-httr::POST(url="http://localhost:8080/results.places.about.countries",query=list(replaceAllDemonyms="true",q="Zambia"))

content(baseC)
basic<-GET("http://localhost:8080")
print(basic)
test2<-GET("http://localhost:8080/results.places.about.countries")
content(test2)
baseC$headers
viewer <- getOption("viewer")
substr(fullc$fullc[1],1,1000)


content(baseC,"parsed")
test<-httr::GET(url="http://localhost:8080/CLIFF-2.1.1/parse/text",query = list(replaceAllDemonyms="true",q=substr(fullc$fullc[1],1,2000)))

fullc
nchar(fullc$fullc[1])/4000

test<-httr::POST(url="http://localhost:8080/CLIFF-2.1.1/parse/text",query = list(replaceAllDemonyms="true",q=substr(fullc$fullc[1],1,4000)))




),encode="json")
httr::content(test, "text")
library(httr)
?POST
?getURL
curl -I http://localhost:8787/CLIFF-2.1.1/parse/text?q=hello%20seattle
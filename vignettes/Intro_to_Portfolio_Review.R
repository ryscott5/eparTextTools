## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("ryscott5/epartexttools")

## ------------------------------------------------------------------------
options(java.parameters = "-Xmx4g")
library(epartexttools)

## ---- echo=TRUE, results='hide'------------------------------------------
example_documents()
corpus1<-allDocs("demo.docs.folder")
jgc()

## ---- echo=TRUE, results='hide',eval=FALSE-------------------------------
#  lapply(corpus1,function(X){X$meta})[[1]]

## ---- echo=TRUE, results='asis'------------------------------------------
corpus2<-doc_clean_process(corpus1)
gc()
jgc()
tdm<-TermDocumentMatrix(corpus2) %>% removeSparseTerms(.,.6)

## ----echo=TRUE, results='asis'-------------------------------------------
word_heatmap(tdm,6)

word_heatmap(tdm,pickwords=c("women","gender","access","land","right","work","labor","yield","security"))

## ----echo=TRUE, results='asis'-------------------------------------------
tdm[,as.vector(tdm["gender",])>1] %>% .[,as.vector(.["women",])>1] %>% word_heatmap(.,20)

## ------------------------------------------------------------------------
wfplots(tdm[,c(2,10,12)],typePlot=2, 5,shortendoc=TRUE)

## ------------------------------------------------------------------------
tdm[,as.vector(tdm["gender",])>20] %>% .[,as.vector(.["women",])>10] %>% wfplots(.,typePlot=2,10,shortendoc=TRUE)

## ------------------------------------------------------------------------
tdm[,as.vector(tdm["gender",])>20] %>% .[,as.vector(.["women",])>10] %>% wfplots(.,typePlot=1,10,shortendoc=TRUE)+ggtitle("Change X versus Y")

## ------------------------------------------------------------------------
interest_plot_bydoc(c("women","farmer","school"),tdm[,1:5])+coord_flip() 

## ------------------------------------------------------------------------
interest_plot_bydoc(c("women","farmer","school"),tdm[,1:5]) %>% plotly::ggplotly() 

## ------------------------------------------------------------------------
TermDocumentMatrix(corpus2[1:10],control=list(weighting=function(x) weightSMART(x))) %>% interest_plot_bydoc(c("women","farmer","school"),.) %>% plotly::ggplotly() 

## ------------------------------------------------------------------------
jgc()
assocPrettyOneStep("gender",tdm, corpus2,.5)

## ------------------------------------------------------------------------
tornadoCompare(tdm,c("gender","equal","femal"),3,10)

## ----eval=F--------------------------------------------------------------
#  jgc()
#  rm(corpus2)
#  rm(tdm)
#  workingfolder<-file.path("Research.Grants")
#  dir.create(file.path(workingfolder))
#  saveRDS(corpus1,file.path(workingfolder,"corpus.rds"))
#  rm(corpus1)

## ----eval=FALSE----------------------------------------------------------
#  corpus1<-readRDS(file.path(workingfolder,"corpus.rds"))
#  jgc()
#  gc()
#  BASE_INPUT<-PreTopicFrame(corpus1,15)
#  BASE_INPUT$out$meta$OpID<-BASE_INPUT$out$meta$Orig
#  #saves files so you can reload
#  jgc()
#  saveRDS(BASE_INPUT,file.path(workingfolder,"base_input1.rds"))

## ---- eval=FALSE---------------------------------------------------------
#  buildcliff()
#  startcliff()
#  library(RCurl)
#  library(httr)
#  BASE_INPUT$SentFrame$OpID<-BASE_INPUT$SentFrame$Orig
#  pred1<-PredictCountryByDoc(BASE_INPUT)
#  stopcliff()
#  BASE_INPUT$out$meta<-reflectCountryCol(BASE_INPUT$out$meta,pred1,10,FALSE)
#  getwd()
#  saveRDS(BASE_INPUT,file.path(workingfolder,"base_input1.rds"))
#  write.csv(pred1,file.path(workingfolder,"countrypredictions1.csv"))
#  jgc()

## ----eval=FALSE----------------------------------------------------------
#  library(plotly)
#  runMap(file.path(workingfolder,"countrypredictions1.csv"),path.file=T,"countries")

## ----eval=FALSE----------------------------------------------------------
#  writeFormulaforSTM(BASE_INPUT,workingfolder)
#  jgc()

## ----eval=FALSE----------------------------------------------------------
#  runSTM(workingfolder)


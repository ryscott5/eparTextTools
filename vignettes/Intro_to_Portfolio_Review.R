## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("ryscott5/epartexttools")
#  library(epartexttools)

## ---- echo=TRUE, results='hide'------------------------------------------
example_documents()
corpus1<-allDocs("demo.docs.folder")

## ---- echo=TRUE, results='hide',eval=FALSE-------------------------------
#  lapply(corpus1,function(X){X$meta})[[1]]

## ---- echo=TRUE, results='asis'------------------------------------------
corpus2<-doc_clean_process(corpus1)
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
assocPrettyOneStep("gender",tdm, corpus2,.5)

## ------------------------------------------------------------------------

tornadoCompare(tdm,c("gender","equal","femal"),20,10)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))


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


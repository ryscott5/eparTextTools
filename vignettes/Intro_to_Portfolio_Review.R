## ---- echo=FALSE, results='asis',eval=FALSE------------------------------
#  example_documents()
#  corpus1<-allDocs("demo.docs.folder")

## ---- echo=FALSE, results='asis',eval=FALSE------------------------------
#  lapply(corpus1,function(X){X$meta})[[1]]

## ---- echo=FALSE, results='asis',eval=FALSE------------------------------
#  corpus2<-doc_clean_process(corpus1)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))


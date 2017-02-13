---
title: "Scraping Structured Documents"
author: "Ryan P Scott"
date: "2017-01-25"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Scraping Structured Documents}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---

Structured docments provide the opportunity to move beyond adopting all text strings for text analysis. Because structured documents such as forms possess entry areas that specify what is supposedly included in the following lines, we can utilize that structure to help define features of the text or characteristics of the documents. 

In terms of structured documents, there are multple kinds a user may run across. First of all, a structured PDF may have OCR labelled text that is in a similar location across a wide body of documents. These are among the more complicated documents to code as one has to rely on characters either being recognized the same across a range of responses or the text information must remained stored in a similar manner across the corpus. Word documents can have structured form fields--this is much less of a problem for extracting data, and we can rely on commmon tools such as Microsoft Access to batch process results. However, this vignette demonstrates how to utilize R to extract necessary information from form fields rapidly by visually assessing the form document and deciding what information is needed for a database.

#Demonstration

```r
tfiles<-list.files("../epar/PracticeDocs2/files",full.names=T)
tfiles<-tfiles[str_detect(tolower(tfiles),"docx")]
```
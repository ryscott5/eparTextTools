# eparTextTools

This tookit provides a set of resources for analyzing textual documents using the R programming language for the conduct of portfolio analysis and review. The tools rely on text mining, natural language processing, and machine learning programs developed by other R users and as such heavily relies on code developed by other packages. Thus, it may be thought of as a set of tools enabling portfolio analysis rather than a new package for conduct of text analysis.

##Extracting Text

To begin this process, first load the textFunctions. For demonstration purposes we also create a demonstration document dataset using the demo.docs.R code.

```r
source("textFunctions.R")
source("demo.docs.R")
```

Main functions enabling workflow include reading documents into R via the "getTextR" command. The getTextR command takes a file directory as an argument, and returns a textual corpus. It is capable of reading word documents (both doc and docx), pdf documents, and txt documents. Further document types can be added by replacing the "FILETYPE NA" line with additional document if loops. The allDocs() command is a conveniance wrapper which loops through a directory parsing documents and creating a corpus with metadata.

```r
corpus1<-allDocs("demo.docs.folder")
```

Documents are read into R as a textual corpus--the method used by the TM package. This enables preservation of document metatdata alongside document text. Metadata such as ids and timestamps is stored in the .$meta location of each corpus.

```r
lapply(corpus1,function(X){X$meta$datetimestamp})
lapply(corpus1,function(X){X$meta$id})

```

Once documents are in R, any textual processing functions in R can be used on the documents. We focus on three general approaches--Description, Cateogrization, and Exploration.


##Text Analysis Methods


###Description

####Word Frequencies and Associations

###Categorization

####Topic Modeling and Document Clustering 

###Exploration and Model Identification

####Natural Language Processing

####Supervised/Unsupervised Learning

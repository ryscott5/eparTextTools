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
lapply(corpus1,function(X){X$meta})
```

###Cleaning and Parsing Text
Texts are stored in the corpus as character strings facilitating use of different analysis packages. Some basic cleaning tools from the TM package may be helpful for performing various funcitons.

```r
corpus2<-doc_clean_process(corpus1)
```

The object Corpus2 is now a cleaned version of the original corpus, which is useful for many description tasks and some analysis tasks.

###Description: Word Frequencies and Associations

For basic description, it is often useful to generate a term document matrix. This is done via the code below, with an additional step to remove sparse terms.

```r
tdm<-TermDocumentMatrix(corpus2) %>% removeSparseTerms(.,.2)
```
Once a term document matrix is available, one can easily begin to create tables and charts to explore the data.

```r
freqterms<-lapply(1:length(corpus2), function(X){findFreqTerms(tdm[,X],lowfreq=15)})
lapply(freqterms,wordcloud)
```





One command, assocPrettyOneStep(), takes a wordlist as an argument and returns a list of associated words above a correlaiton threshold. 

assocPrettyOneStep(c(""),tdm, corpus2,.5)


###Categorization

####Topic Modeling and Document Clustering 

###Exploration and Model Identification

####Natural Language Processing

####Supervised/Unsupervised Learning

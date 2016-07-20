---
output: html_document
---
# eparTextTools

This tookit provides a set of resources for analyzing textual documents using the R programming language for the conduct of portfolio analysis and review. The tools rely on text mining, natural language processing, and machine learning programs developed by other R users and as such heavily relies on code developed by other packages. Thus, it may be thought of as a set of tools enabling portfolio analysis rather than a new package for conduct of text analysis.

To run this document, clone the repository to a local directory. To do this, first set up an ssh key with rstudio by going to tools...global options...git/svn...create ssh key and registering the key with your github account. Then, go to file...new projects...version control and enter the ssh site for the github page (the clone link).  Alternatively, you download a zip file of the repository and open the .Rproject file. This document is stored as readme.md with coding in chunks. 

The program depends on a few software installations, notably "antiword" or "tika" and "Apache openNLP". To install these on a mac, one can use homebrew and run the command "brew install antiword". Installing openNLP requires updating the Java JRE file on your make. This can also be done via homebrew on a mac. 

The document reading code is designed to be able to use the tika library, though this is disabled by default. 

##Useful Links
[Guide to R and Github](http://r-pkgs.had.co.nz/git.html)

[tm packages](https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf)

[ggplot2 guide](http://ggplot2.org/)

##Extracting Text

To begin this process, first load the textFunctions. For demonstration purposes we also create a demonstration document dataset using the demo.docs.R code.

```{r}
source("textFunctions.R")
source("demo.docs.R")
```

Main functions enabling workflow include reading documents into R via the "getTextR" command. The getTextR command takes a file directory as an argument, and returns a textual corpus. It is capable of reading word documents (both doc and docx), pdf documents, and txt documents. Further document types can be added by replacing the "FILETYPE NA" line with additional document if loops. The allDocs() command is a conveniance wrapper which loops through a directory parsing documents and creating a corpus with metadata.

```{r}
corpus1<-allDocs("demo.docs.folder")
```

Documents are read into R as a textual corpus--the method used by the TM package. This enables preservation of document metatdata alongside document text. Metadata such as ids and timestamps is stored in the .$meta location of each corpus.

```{r}
lapply(corpus1,function(X){X$meta})
```

###Cleaning and Parsing Text
Texts are stored in the corpus as character strings facilitating use of different analysis packages. Some basic cleaning tools from the TM package may be helpful for performing various funcitons.

```{r}
corpus2<-doc_clean_process(corpus1)
```

The object Corpus2 is now a cleaned version of the original corpus, which is useful for many description tasks and some analysis tasks.

###Description: Word Frequencies and Associations

For basic description, it is often useful to generate a term document matrix. This is done via the code below, with an additional step to remove sparse terms.

```{r}
tdm<-TermDocumentMatrix(corpus2) %>% removeSparseTerms(.,.2)
```
Once a term document matrix is available, one can easily begin to create tables and charts to explore the data.

The word_heatmap function provides an easy wrapper for structuring term document matrices into an interactive matrix.

```{r}
word_heatmap(tdm,6)
```

Using wfplots(), one can create a basic ggplot() object describing the most frequent terms across documents, or the documents in which the most frequent terms are likely to occur. The objects created by the command can be edited by adding on additional functions.

```{r}
wfplots(tdm,typePlot=1,10,shortendoc=TRUE)
wfplots(tdm,typePlot=0,10,shortendoc=TRUE)
wfplots(tdm,typePlot=0,10,shortendoc=TRUE)+theme_fivethirtyeight()+facet_wrap(~word)
```

The same is true for the interest_plot command, which allows the user to specify words they are interested in viewing across documents rather than relying on specific frequencies. 

```{r}
tdm$dimnames$Docs<-substring(tdm$dimnames$Docs,nchar(tdm$dimnames$Docs)-20)
interest_plot(c("women","farmer","school"),tdm, by.var=c("Identity","Program","Identity"), "Classification")
interest_plot_bydoc(c("women","farmer","school"),tdm)
interest_plot_bydoc(c("women","farmer","school"),tdm)
```


By editing the term document matrix to include weighting, each of these commands can be used while taking the length of documents into account.

```{r}
tdm2<-TermDocumentMatrix(corpus2,control=list(weighting=function(x) weightSMART(x))) 
tdm2$dimnames$Docs<-substring(tdm2$dimnames$Docs,nchar(tdm2$dimnames$Docs)-20)

wfplots(tdm2,typePlot=1,10)
wfplots(tdm2,typePlot=0,10)
interest_plot_bydoc(c("school"),tdm2)
word_heatmap(tdm2,6)
tdm3<-tdm
tdm3$v<-(tdm3$v/colSums(as.matrix(tdm3))[tdm3$j])*100
wfplots(tdm3,typePlot=1,2)
word_heatmap(tdm3,6)
```

One command, assocPrettyOneStep(), takes a wordlist as an argument and returns a list of associated words above a correlation threshold. This thus informs what words are most likely to cooccur accross a corpus of documents.

```{r}
assocPrettyOneStep(c("women","farmer","effect"),tdm, corpus2,.5)
```

Bigrams or trigrams (rather than word-based tokenization) can also be used to process text by creating a custom ngram tokenizer and inserting it into the TermDocumentMatrix() command. All of the previous commands, with the exception of the word association table, can be used to explore word frequencies and correlations.

```{r}
BigramTokenizer <-function(x){unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
tdm_bi <- TermDocumentMatrix(corpus2, control = list(tokenize = BigramTokenizer))
freqterms2<-lapply(1:length(corpus2), function(X){findFreqTerms(tdm_bi[,X],lowfreq=10)})
tdm_bi$dimnames$Docs<-substring(tdm_bi$dimnames$Docs,nchar(tdm_bi$dimnames$Docs)-20)
word_heatmap(tdm_bi,6)
wfplots(tdm_bi,typePlot=1,20)
```

###Topic Modeling and Document Clustering 

kmeans(tdm,5,n=10)


##Natural Language Processing
While description of word frequencies is useful as a baseline for exploring textual documents, such methods rely on a bag-of-words approach, meaning any natural meanings to words or meaning derived from ordering of text is lost. Natural Language Processing provides a methodology for incorporating structure and natural language meanings of words into analysis via detection of common patterns in text.




####Supervised/Unsupervised Learning

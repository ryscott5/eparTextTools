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

#Theory of Text Tools
The goal of the text tools is to build and develop a methodology for automated portfolio review that can leverage tools of machine learning, supervised machine learning, and text mining to support human coding and portfolio review analysis tasks. The basic framework under development is shown below. 

![Model of Program](https://www.lucidchart.com/publicSegments/view/ecbf4945-8913-479b-ab7d-0f44e5553d30/image.png)

The tools work towards two broad goals. First of all, the tools provide a flexible framework for describing and classifying the content of textual documents. This includes analysis of word frequencies, description of common words, testing for correlations between words, and categorization of strings of text into modeled or human coded categories of topics. The text tools, as designed, support query-based description, such as "how often does EPAR research involve the words policy analysis versus program evaluation?" However, they also allow a user to explore documents by allowing the documents to suggest word correlations, commonalities, and topics.

In addition to the basic description tasks, the tools are designed to support automated extraction of program theory from a body of documents by relying on a combination of machine learning, natural language processing, and supervised classification techniques. 

This readme file first guides a user through the basic description and classification tasks, and provides a basic example of how the tools can be leveraged to extract program theory. At present, the code analyzes the public body of EPAR research stored on the Evans School of Public Policy website, but it is designed to be readily portable to any document set.

#Description and Classification

##Extracting Text

To begin this process, first load the textFunctions. For demonstration purposes we also create a demonstration document dataset using the demo.docs.R code.

```{r}
source("textFunctions.R")

source("demo.docs.R")

```

Main functions enabling workflow include reading documents into R via the "getTextR" command. The getTextR command takes a file directory as an argument, and returns a textual corpus. It is capable of reading word documents (both doc and docx), pdf documents, and txt documents. Further document types can be added by replacing the "FILETYPE NA" line with additional document if loops. The allDocs() command is a conveniance wrapper which loops through a directory parsing documents and creating a corpus with metadata.

```{r}

dir.create("figures")
corpus1<-allDocs("demo.docs.folder")
```

Documents are read into R as a textual corpus--the method used by the TM package. This enables preservation of document metatdata alongside document text. Metadata such as ids and timestamps is stored in the .$meta location of each corpus.

```{r}
lapply(corpus1,function(X){X$meta})[[1]]

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
tdm<-TermDocumentMatrix(corpus2) %>% removeSparseTerms(.,.6)


```
Once a term document matrix is available, one can easily begin to create tables and charts to explore the data.

The word_heatmap function provides an easy wrapper for structuring term document matrices into an interactive matrix.

```{r}
word_heatmap(tdm,6)
saveWidget(word_heatmap(tdm,6),"figures1.html")
```

[Example 1: CLICK HERE](http://students.washington.edu/ryscott5/epar/figures/figures1.html)

```{r}
word_heatmap(tdm,20)
saveWidget(word_heatmap(tdm,20),"figures2.html")
```

[Example 2: CLICK HERE](http://students.washington.edu/ryscott5/epar/figures/figures1.html)


Using wfplots(), one can create a basic ggplot() object describing the most frequent terms across documents, or the documents in which the most frequent terms are likely to occur. The objects created by the command can be edited by adding on additional functions.

```{r}
wfplots(tdm[,1:10],typePlot=1,10,shortendoc=TRUE)
ggsave("figures/figures3.png")
```

![http://students.washington.edu/ryscott5/epar/figures/figures3.png](figures/figures3.png)

```{r}
wfplots(tdm[,1:30],typePlot=0,10,shortendoc=TRUE)
ggsave("figures/figures4.png")
```
![http://students.washington.edu/ryscott5/epar/figures/figures4.png](figures/figures4.png)

```{r}
wfplots(tdm[,1:30],typePlot=0,10,shortendoc=TRUE)+theme_fivethirtyeight()+facet_wrap(~word)
ggsave("figures/figures5.png")
```

![http://students.washington.edu/ryscott5/epar/figures/figures5.png](figures/figures5.png)

The same is true for the interest_plot command, which allows the user to specify words they are interested in viewing across documents rather than relying on specific frequencies. 

```{r}
tdm$dimnames$Docs<-substring(tdm$dimnames$Docs,nchar(tdm$dimnames$Docs)-20)
interest_plot(c("women","farmer","school"),tdm, by.var=c("Identity","Program","Identity"), "Classification")
ggsave("figures/figures6.png")
```
![http://students.washington.edu/ryscott5/epar/figures/figures6.png](figures/figures6.png)

```{r}
interest_plot_bydoc(c("women","farmer","school"),tdm)
ggsave("figures/figures7.png")
```

![http://students.washington.edu/ryscott5/epar/figures/figures7.png](figures/figures7.png)

By editing the term document matrix to include weighting, each of these commands can be used while taking the length of documents into account.

```{r}
tdm2<-TermDocumentMatrix(corpus2,control=list(weighting=function(x) weightSMART(x))) 
tdm2$dimnames$Docs<-substring(tdm2$dimnames$Docs,nchar(tdm2$dimnames$Docs)-20)
wfplots(tdm2[,1:10],typePlot=1,10)
wfplots(tdm2[,1:30],typePlot=0,5)
ggsave("figures/figures8.png")
```
![http://students.washington.edu/ryscott5/epar/figures/figures8.png](figures/figures8.png)

```{r}
interest_plot_bydoc(c("school"),tdm2)
ggsave("figures/figures9.png")
```
![http://students.washington.edu/ryscott5/epar/figures/figures9.png](figures/figures9.png)

```{r}
word_heatmap(tdm2,6)
saveWidget(word_heatmap(tdm2,6),"figures10.html")
```
[Example 10: CLICK HERE](http://students.washington.edu/ryscott5/epar/figures/figures10.html)

```{r}
tdm3<-tdm
tdm3$v<-(tdm3$v/colSums(as.matrix(tdm3))[tdm3$j])*100
word_heatmap(tdm3,6)
saveWidget(word_heatmap(tdm3,6),"figures11.html")
rm(tdm2,tdm3)
```

[Example 11: CLICK HERE](http://students.washington.edu/ryscott5/epar/figures/figures11.html)


One command, assocPrettyOneStep(), takes a wordlist as an argument and returns a list of associated words above a correlation threshold. This thus informs what words are most likely to cooccur accross a corpus of documents.

```{r}
assocPrettyOneStep(c("women","farmer","effect"),tdm, corpus2,.5)
saveWidget(assocPrettyOneStep(c("women","farmer","effect"),tdm, corpus2,.5),"figures12.html")
```

[Example 12: CLICK HERE](http://students.washington.edu/ryscott5/epar/figures/figures12.html)


Bigrams or trigrams (rather than word-based tokenization) can also be used to process text by creating a custom ngram tokenizer and inserting it into the TermDocumentMatrix() command. All of the previous commands, with the exception of the word association table, can be used to explore word frequencies and correlations.

```{r}
BigramTokenizer <-function(x){unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
tdm_bi <- TermDocumentMatrix(corpus2, control = list(tokenize = BigramTokenizer))

freqterms2<-lapply(1:length(corpus2), function(X){findFreqTerms(tdm_bi[,X],lowfreq=10)})
tdm_bi$dimnames$Docs<-substring(tdm_bi$dimnames$Docs,nchar(tdm_bi$dimnames$Docs)-20)
word_heatmap(tdm_bi,10)
saveWidget(word_heatmap(tdm_bi,10), "figures13.html")
rm(tdm_bi)
```
[Example 13: CLICK HERE](http://students.washington.edu/ryscott5/epar/figures/figures13.html)


#Natural Language Processing
While description of word frequencies is useful as a baseline for exploring textual documents, such methods rely on a bag-of-words approach, meaning any natural meanings to words or meaning derived from ordering of text is lost. Natural Language Processing provides a methodology for incorporating structure and natural language meanings of words into analysis via detection of common patterns in text.

```{r}
y1<-lapply(corpus1,function(k){list(k,annotate(as.String(k$content),sent_token_annotator))})
y1<-y1[which(sapply(1:length(y1),function(i){length(y1[[i]][[2]]$type)})>1)]
y1<-lapply(y1,function(k){list(k[[1]],annotate(as.String(k[[1]]$content),word_token_annotator,k[[2]]))})

basic_annotates<-lapply(y1,function(k){list(k[[1]],annotate(as.String(k[[1]]$content), list(org.annotate,pers.annotate,location.annotate),k[[2]]))})

##Removing entities for corp3
corpEnt<-lapply(basic_annotates,function(k){as.String(k[[1]]$content)[k[[2]][k[[2]]$type=="entity"]]})
corpEnt2<-lapply(basic_annotates,function(k){
bv<-as.String(k[[1]]$content)[subset(k[[2]], type=="entity")]
for(e in bv){k[[1]]$content<-str_replace_all(k[[1]]$content,fixed(e),"")}
k})

#corpEnt2<-lapply(corpEnt2, function(k){
#y1<-annotate(as.String(k[[1]]$content), #list(sent_token_annotator,word_token_annotator))
#y1<-annotate(as.String(k[[1]]$content), #list(org.annotate,pers.annotate),y1)
#list(k[[1]],y1)
#})

corpEnt2<-doc_clean_process(do.call(c, lapply(corpEnt2,function(k) k[[1]])))

locations<-lapply(basic_annotates,function(k){list(k[[1]],k[[2]][which(sapply(subset(k[[2]],type=="entity")$features,function(x) {x=="location"})==TRUE)])})

locuts<-lapply(basic_annotates,function(X){as.String(X[[1]]$content)[X[[2]][sapply(subset(X[[2]],type=="entity")$features,function(x){x$kind=="location"})]]})
locuts[[1]]

```
While we later come back to natural language processing for extraction of verbs, subjects, and direct objects, once proper nouns are removed from the dataset one method of analyzing variability across a group of text documents is to build a topic model for the data.


###Topic Model
Once the proper nouns are (mostly) removed from the dataset, we can fit a topic model to the dataset. A topic model is an unsupervised machine learning method of categorizing words into common topics which then can be used to explore what topics different documents refer to. For fitting the topic model, we rely on the R package stm because stm allows for inclusion of covariates in the fitting of the topic model. For example, if a portfolio of projects has already been coded by a set of RAs for certain variables, these variables can be used to inform the modeling of topics.

Fitting a topic model has numerous benefits over simply viewing raw word counts, including that it allows exploration of the content of a batch of documents that can potentially reveal broad patterns missed by human coders. The stm package has its own text cleaning method which repeats many of the processing steps conducted above. Fiting the model can be somewhat slow given the settings used below, which includes

```{r}
processed <- textProcessor(sapply(corpEnt2,function(x) content(x[[1]])),metadata=do.call(rbind, lapply(corpEnt2,function(x){t(matrix(x[[1]]$meta,dimnames=list(names(x[[1]]$meta))))})))

out <- prepDocuments(processed$documents,processed$vocab,processed$meta)

#st1<-stm(out$documents,out$vocab,K=0, init.type="Spectral")

st1<-stm(out$documents,out$vocab,K=30, init.type="LDA")

toLDAvis(st1,out$documents,R=20,plot.opts=list(xlab="Component 1",ylab="Component 2"),lambda.step=.05,out.dir=tempfile(),open.browser=interactive())
saveWidget(toLDAvis(st1,out$documents,R=20,plot.opts=list(xlab="Component 1",ylab="Component 2"),lambda.step=.05,out.dir="figures/figures14.html"))
```
[Example 14: CLICK HERE](http://students.washington.edu/ryscott5/epar/figures/figures14/index.html)


```{r}
ksearch<-searchK(out$documents, out$vocab, K = c(3,4,5),init.type="LDA")

mselect<-selectModel(out$documents, out$vocab, K = 20, prevalence=~datetimestamp+author,data=out$meta,max.em.its = 75, runs = 20, seed = 8458159)

plotModels(mselect,labels=1:length(mselect$runout))

```

##After making figures, run code below
```{r}
lapply(list.files() %>% .[str_detect(.,"figures\\w+.html")],function(X){file.copy(from=X,to=file.path("figures",X),overwrite=TRUE)})
file.remove(list.files() %>% .[str_detect(.,"figures\\w+.html")])
```


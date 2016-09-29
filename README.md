---
output: html_document
---
# eparTextTools

This tookit provides a set of resources for analyzing textual documents using the R programming language for the conduct of portfolio analysis and review. The tools rely on text mining, natural language processing, and machine learning programs developed by other R users and as such heavily relies on code developed by other packages. Thus, it may be thought of as a set of tools enabling portfolio analysis rather than a new package for conduct of text analysis.

The tools are set up to be run on a Google Cloud compute instance. this has the benefit of allowing you to run processes in the cloud rather than on your own local computer. While setting up and running a Google Instance can be a new trial for those who have never used a Linux shell, 99% of the coding can be done in RStudio simply by visiting the rstudio-server webpage that the Google Cloud Instance Startup code creates. 

In terms of sizing, a 2GPU 7.5GB RAM n1-standard machine should work fine so long as you increase the size of the JAVA heap space. If you need a job to complete more quickly or less quickly, you could by a larger instance. Cloud pricing is [here](https://cloud.google.com/compute/pricing). You can set up the job to be pre-emptable as well. This will save you lots of money and is highly recommended. When using a preemptible instance you must make sure to save your robjects to file regularly as Google can shut down your work at any time. However, this will reduce the computing costs by about 1/5th.

The tools can be used with Amazon AWS if that is a more familiar format, however, the installation instructions will be somewhat different and you will have to be flexible in porting the cloud_startup code to AWS.



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


corpEntS<-sapply(corpEnt2,function(x) as.character(as.String(content(x[[1]]))))

corpEnt3<-corpEnt2
corpEnt3<-lapply(names(corpEnt2[[1]][[1]]$meta),function(K){meta(do.call(c,lapply(corpEnt2,function(x){x[[1]]})),K)})

corpEnt3[[1]]<-melt(sapply(1:length(corpEnt3[[1]]),function(x) paste(corpEnt3[[1]][[x]],collapse="")))[,1]
corpEnt3[[2]]<-melt(sapply(1:length(corpEnt3[[2]]), function(x) ymd_hms(corpEnt3[[2]][[x]]),simplify=T))
corpEnt3[[2]]<-join(data.frame("L1"=1:length(corpEnt3[[1]])),corpEnt3[[2]])$value
corpEnt3[[3]]<-melt(sapply(1:length(corpEnt3[[3]]),function(x) paste(corpEnt3[[3]][[x]],collapse="")))[,1]
corpEnt3[[4]]<-melt(sapply(1:length(corpEnt3[[4]]),function(x) paste(corpEnt3[[4]][[x]],collapse="")))[,1]
corpEnt3[[5]]<-melt(sapply(1:length(corpEnt3[[5]]),function(x) paste(corpEnt3[[5]][[x]],collapse="")))[,1]
corpEnt3[[6]]<-melt(sapply(1:length(corpEnt3[[6]]),function(x) paste(corpEnt3[[6]][[x]],collapse="")))[,1]
corpEnt3[[7]]<-melt(sapply(1:length(corpEnt3[[6]]),function(x) paste(corpEnt3[[7]][[x]],collapse="")))[,1]
getTokenizers()
corpEnt3<-data.frame(corpEnt3)
colnames(corpEnt3)<-names(meta(corpEnt2[[1]][[1]]))
class(corpEntS)
?substr
substr(corpEntS[1],1,1000)
?textProcessor
processed <-textProcessor(corpEntS,metadata=corpEnt3,sparselevel=1,)
missingINF<-unlist(unique(sapply(colnames(corpEnt3),function(x){which(is.na((corpEnt3[,x])))}),simplify=T))
processed$vocab[2000]
out <- prepDocuments(processed$documents[-missingINF],processed$vocab,processed$meta[-missingINF],lower.thresh=2)

#st1<-stm(out$documents,out$vocab,K=0, init.type="Spectral")
st1<-stm(out$documents,out$vocab,data=out$meta, K=10,prevalence=~author+as.numeric(datetimestamp)+heading, init.type="LDA",max.em.its=5)

toLDAvis(st1,out$documents,R=20,plot.opts=list(xlab="Component 1",ylab="Component 2"),lambda.step=.05,out.dir=tempfile(),open.browser=interactive())
saveWidget(toLDAvis(st1,out$documents,R=20,plot.opts=list(xlab="Component 1",ylab="Component 2"),lambda.step=.05,out.dir="figures/figures14.html"))

ggplot(melt(st1$theta))+geom_bar(aes(x=Var1,y=value,fill=as.factor(Var2)),stat="identity",position="stack")+scale_fill_tableau()+theme_minimal()
colnames(out$meta)


st1R<-estimateEffect(1:10~author+as.numeric(datetimestamp),stmobj=st1,metadata=out$meta,nsims=1, documents=out$documents,)

sapply(1:10,function(i){st1R$parameters[[i]][[1]]$est[32]})

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
wd<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/1ng7I5QoYu_NzegZvO9568M42q52iVCsLz-D0WhVa--8/pub?gid=0&single=true&output=csv")),stringsAsFactors=FALSE)
findThoughts(st1,texts=out$documents,topics=1,n=1)
findTopic(st1, wd$Up.Words, n=10, type=c("frex"), verbose=TRUE)

assocPrettyOneStep(wd$Up.words,, corpus2,.5)

pf1<-read.csv("pred1.csv",stringsAsFactors=FALSE)
head(pf1)

options(java.parameters = "-Xmx4g")

list.of.packages <- c("ggplot2", "Rcpp","tm","ggthemes","SnowballC","rvest","downloader","DT","wordcloud","d3heatmap","plyr","reshape2","dplyr","qdapTools","stringr","openNLP","NLP","stm","LDAvis","servr","Rtsne","geometry","downloader","corrplot","pryr","openNLPmodels.en","lubridate","pbapply")
if("StanfordCoreNLP"%in%c(installed.packages()[,"Package"])==FALSE){install.packages('StanfordCoreNLP',repos="http://datacube.wu.ac.at/",type="source")}
if("openNLPmodels.en"%in%c(installed.packages()[,"Package"])==FALSE){install.packages('openNLPmodels.en',repos="http://datacube.wu.ac.at/",type="source")}
packages.Req <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(packages.Req)) install.packages(packages.Req)
lapply(list.of.packages, function(X) library(X,character=TRUE))

#package depends on xpdf... on mac, install via brew install xpdf and follow instructions.

#the check for tika command will install tika to the current working directory (on a mac)

checkForTika<-function(directory=getwd()){if("tika-app-1.13.jar"%in%list.files(path=directory)) {cat("success")} else {download.file("http://apache.claz.org/tika/tika-app-1.13.jar",file.path(directory,"tika-app-1.13.jar"))}}

getTextR<-function(fname,tika=FALSE,tikapath="tika-app-1.13.jar"){
  if(tika==TRUE){
    pdoc<-system(command=paste("java -jar",tikapath,"-t",gsub(" ","\\ ",fname,fixed=TRUE)),intern=TRUE,wait=TRUE)
  } else {
    pdoc<-if(str_detect(fname,".docx+$")==TRUE){read_docxtm(fname)} else {
      if(str_detect(fname,".doc+$")==TRUE){readDOC()(language="en",elem=list(uri=fname))} else {pdoc<-if(str_detect(fname, fixed(".pdf"))==TRUE){readPDF2(engine="xpdf")(elem=list(uri=fname), language="en")} else {if(str_detect(fname,fixed(".txt"))==TRUE){readPlain(elem=list(uri=fname,content=iconv(enc2utf8(readLines(fname)), sub = "byte")),language="en")} else {"FILETYPE NA"}}}}}
  pdoc
}

read_docxtm<-function (file, skip = 0) {
  tmp <- tempfile()
  if (!dir.create(tmp)) 
    stop("Temporary directory could not be established.")
  utils::unzip(file, exdir = tmp)
  xmlfile <- file.path(tmp, "word", "document.xml")
  doc <- XML::xmlTreeParse(xmlfile, useInternalNodes = TRUE)
  xml_metadata<-read_xml(file.path(tmp, "docProps", "core.xml")) 
  unlink(tmp, recursive = TRUE)
  nodeSet <- XML::getNodeSet(doc, "//w:p")
  pvalues <- sapply(nodeSet, XML::xmlValue)
  pvalues <- pvalues[pvalues != ""]
  if (skip > 0) 
    pvalues <- pvalues[-seq(skip)]
  tempdes<-xml_text(xml_metadata)
  xml_metadata<-xml_metadata %>% as_list() 
  PlainTextDocument(x=pvalues,author=xml_metadata$creator[[1]],description=tempdes,datetimestamp=xml_metadata$modified[[1]], id=file)}

processURI2 <-function(uri) {
  uri <- as.character(uri)
  if (identical(substr(uri, 1, 7), "file://"))
    uri <- substr(uri, 8, nchar(uri))
  uri
}

readPDF2<-function (engine = c("xpdf", "Rpoppler", "ghostscript", "Rcampdf","custom"), control = list(info = NULL, text = NULL)) {
  stopifnot(is.character(engine), is.list(control))
  engine <- match.arg(engine)
  pdf_info <- switch(engine, xpdf = function(x) tm:::pdf_info_via_xpdf(x,control$info), Rpoppler = Rpoppler::PDF_info, ghostscript = tm:::pdf_info_via_gs,Rcampdf = Rcampdf::pdf_info, custom = control$info)
  pdf_text <- switch(engine, xpdf = function(x) system2("pdftotext", c(control$text, shQuote(x), "-"), stdout = TRUE), Rpoppler = Rpoppler::PDF_text, ghostscript = pdf_text_via_gs, Rcampdf = Rcampdf::pdf_text, custom = control$text)
  if (!is.function(pdf_info) || !is.function(pdf_text)) 
    stop("invalid function for PDF extraction")
  function(elem, language, id) {
    uri <- processURI2(elem$uri)
    meta <- pdf_info(uri)
    content <- pdf_text(uri)
    content<-iconv(enc2utf8(content), sub = "byte")
    tm::PlainTextDocument(content, meta$Author, meta$CreationDate, 
                          meta$Subject, meta$Title, basename(elem$uri), language, 
                          meta$Creator)
  }
}

allDocs<-function(directory){do.call(c,lapply(file.path(directory,list.files(directory)),getTextR))}
doc_clean_process<-function(corpusname){
  stopWords <- function(x) removeWords(x, stopwords("en"))
  funs <- list(stripWhitespace,
               stopWords,
               removePunctuation,
               stemDocument,
               content_transformer(tolower))
  corpus2<-tm_map(corpusname, FUN = tm_reduce, tmFuns = funs, mc.cores=1)
  corpus2}
assocPTable<-function(assoctable,corpus,ngram=FALSE){
  #assoctable<-assoctable[sapply(assoctable,length)>0]
  dft<-do.call(rbind,lapply(1:length(assoctable),function(i){tryCatch({data.frame("Word"=names(assoctable)[i],"Match"=names(assoctable[[i]]),"Association"=c(assoctable[[i]]))},error=function(e){data.frame("Word"=names(assoctable)[i],"Match"="too few words","Association"=c(0))})}))
  dft$Word<-as.character(dft$Word)
  dft$Match<-as.character(dft$Match)
  #dft$Word[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])]<-stemCompletion(dft$Word[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])],dictionary=corpus,type="prevalent")
  dft$Match[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])]<-stemCompletion(dft$Match[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])],dictionary=corpus,type="prevalent")
  datatable(data=dft,rownames=FALSE,filter="top")}
assocPrettyOneStep<-function(wordlist,termDocumentMatrix,corpus,corrVal=.8){
  assocPTable(findAssocs(termDocumentMatrix,wordlist,corrVal),corpus)
}


wfplots<-function(termDocumentMatrix,typePlot=1,wordcount,minfreq=5,shortendoc=FALSE){
  mfcomframe<-data.frame(as.matrix(termDocumentMatrix[findFreqTerms(termDocumentMatrix, lowfreq=minfreq),]))
  mfcomframe<-mfcomframe[sort(rowSums(mfcomframe),index.return=TRUE,decreasing=TRUE)$ix[1:wordcount],]
  mfcomframe$word<-row.names(mfcomframe)
  mfcomframe$word<-factor(mfcomframe$word, levels = mfcomframe$word)
  mfcomframe<-melt(mfcomframe,id=c("word"))
  if(shortendoc==TRUE){
    mfcomframe$variable<-as.factor(mfcomframe$variable)
    levels(mfcomframe$variable)<-1:length(levels(mfcomframe$variable))}
  ?substring
  if(typePlot==1){
    plotout<-ggplot(mfcomframe)+geom_bar(aes(x=word,y=value,fill=variable),position="stack",stat="identity")+coord_flip()+theme_pander()+scale_fill_tableau(name="Document")+ylab("Frequency")+xlab("Word")}
  if(typePlot!=1){
    plotout<-ggplot(mfcomframe)+geom_bar(aes(x=variable,y=value,fill=word),position="stack",stat="identity")+coord_flip()+theme_pander()+scale_fill_tableau(name="Word",palette="tableau10")+ylab("Frequency")+xlab("Document")}
  plotout
}

word_heatmap<-function(termDocumentMatrix,wordcount,minfreq=2){
  mfcomframe<-data.frame(as.matrix(termDocumentMatrix[findFreqTerms(termDocumentMatrix, lowfreq=minfreq),]))
  mfcomframe<-mfcomframe[sort(rowSums(mfcomframe),index.return=TRUE,decreasing=TRUE)$ix[1:wordcount],]
  mfcomframe$word<-row.names(mfcomframe)
  mtcells<-as.matrix(termDocumentMatrix[mfcomframe$word,])
  d3heatmap(termDocumentMatrix[mfcomframe$word,], scale="column",colors="Blues",Rowv=FALSE,xaxis_font_size=8)}

interest_plot<-function(wordlist,termDocumentMatrix,by.var=NULL,byvarname=""){
  tempframe<-data.frame(as.matrix(termDocumentMatrix[wordlist,]))
  tempframe<-data.frame("Count"=rowSums(tempframe),"word"=row.names(tempframe))
  if(is.null(by.var)){
    ggplot(tempframe, aes(word, Count)) + geom_bar(fill="#8ebfad", position = "dodge", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+xlab("")+ylab("Frequency")}
  else {
    ggplot(tempframe, aes(word, Count, fill=by.var)) + geom_bar(position = "dodge", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+xlab("")+ylab("Frequency")+scale_fill_pander(name=as.character(byvarname))}
}

interest_plot_bydoc<-function(wordlist,termDocumentMatrix){
  tempframe<-data.frame(as.matrix(termDocumentMatrix[wordlist,]))
  tempframe$word<-row.names(tempframe)
  tempframe<-melt(tempframe,id=c("word"))
  if(length(wordlist)>1){
    ggplot(tempframe, aes(variable, value, fill=word)) + geom_bar(position = "stack", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+xlab("")+ylab("Frequency")+scale_fill_pander()
  } else {
    ggplot(tempframe, aes(variable, value)) + geom_bar(fill="#8ebfad", position = "stack", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12), panel.background=element_blank())+xlab("")+ylab("Frequency")+scale_fill_pander()
  }}


interest_plot_bydoc_char<-function(wordlist,termDocumentMatrix,doccharacteristic){
  termDocumentMatrix$dimnames$Docs<-doccharacteristic
  tempframe<-data.frame(as.matrix(termDocumentMatrix[wordlist,]))
  tempframe$word<-row.names(tempframe)
  tempframe<-melt(tempframe,id=c("word"))
  if(length(wordlist)>1){
    ggplot(tempframe, aes(variable, value, fill=word)) + geom_bar(position = "stack", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+xlab("")+ylab("Frequency")+scale_fill_pander()
  } else {
    ggplot(tempframe, aes(variable, value)) + geom_bar(fill="#8ebfad", position = "stack", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12), panel.background=element_blank())+xlab("")+ylab("Frequency")+scale_fill_pander()
  }}


#nlp
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator<- Maxent_POS_Tag_Annotator()
org.annotate<-Maxent_Entity_Annotator(language = "en", kind="organization", probs = FALSE,model = NULL)
pers.annotate<-Maxent_Entity_Annotator(language = "en", kind="person", probs = FALSE,model = NULL)
location.annotate<-Maxent_Entity_Annotator(language = "en", kind="location", probs = FALSE,model = NULL)
#misc.annotate<-Maxent_Entity_Annotator(language = "en", kind="misc", probs = FALSE,model = NULL)
money.annotate<-Maxent_Entity_Annotator(language = "en", kind="money", probs = FALSE,model = NULL)
parse_annotator <- Parse_Annotator()


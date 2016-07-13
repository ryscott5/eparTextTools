library(tm)
library(stringr)
library(qdapTools)
library(tm)
library(SnowballC)
library(rvest)
library(downloader)
library(DT)
#package depends on xpdf... on mac, install via brew install xpdf and follow instructions.

#the check for tika command will install tika to the current working directory (on a mac)

checkForTika<-function(directory=getwd()){if("tika-app-1.13.jar"%in%list.files(path=directory)) {cat("success")} else {download.file("http://apache.claz.org/tika/tika-app-1.13.jar",file.path(directory,"tika-app-1.13.jar"))}}

getTextR<-function(fname,tika=FALSE,tikapath="tika-app-1.13.jar"){
  if(tika==TRUE){
    pdoc<-system(command=paste("java -jar",tikapath,"-t",gsub(" ","\\ ",fname,fixed=TRUE)),intern=TRUE,wait=TRUE)
  } else {
    pdoc<-if(str_detect(fname,".docx+$")==TRUE){read_docxtm(fname)} else {
      if(str_detect(fname,".doc+$")==TRUE){readDOC()(language="en",elem=list(uri=fname))} else {
    pdoc<-if(str_detect(fname,fixed(".docx"))==TRUE){read_docxtm(fname)} else {
      if(str_detect(fname,fixed(".doc"))==TRUE){readDOC()(language="en",elem=list(uri=fname))} else {
        if(str_detect(fname, fixed(".pdf"))==TRUE){readPDF2(engine="xpdf")(elem=list(uri=fname), language="en")} else {if(str_detect(fname,fixed(".txt"))==TRUE){readPlain(elem=list(uri=fnames[57],content=iconv(enc2utf8(readLines(fnames[57])), sub = "byte")),language="en")} else {"FILETYPE NA"}}}}}
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
assocPTable<-function(assoctable,corpus){
  #assoctable<-assoctable[sapply(assoctable,length)>0]
  dft<-do.call(rbind,lapply(1:length(assoctable),function(i){tryCatch({data.frame("Word"=names(assoctable)[i],"Match"=names(assoctable[[i]]),"Association"=c(assoctable[[i]]))},error=function(e){data.frame("Word"=names(assoctable)[i],"Match"="too few words","Association"=c(0))})}))
  dft$Word<-as.character(dft$Word)
  dft$Match<-as.character(dft$Match)
  #dft$Word[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])]<-stemCompletion(dft$Word[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])],dictionary=corpus,type="prevalent")
  dft$Match[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])]<-stemCompletion(dft$Match[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])],dictionary=corpus,type="prevalent")
  datatable(data=dft,rownames=FALSE,filter="top",)}
assocPrettyOneStep<-function(wordlist,termDocumentMatrix,corpus,corrVal=.8){
  assocPTable(findAssocs(termDocumentMatrix,wordlist,corrVal),corpus)
}
assocPrettyOneStep<-function(wordlist,termDocumentMatrix,corpus,corrVal){
  assocPTable(findAssocs(termDocumentMatrix,wordlist,corrVal),corpus)
}


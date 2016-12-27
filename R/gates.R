library(docxtractr)
library(tm)
library(plyr)
library(dplyr)
library(xml2)
library(stringr)
getwd()
tfiles<-list.files("../epar/PracticeDocs2/files",full.names=T)
tfiles<-tfiles[str_detect(tolower(tfiles),"docx")]

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

readfiletab_gates<-function(tfile){
  print(tfile)
  propnar1<-read_docx(tfile)
  docx_extract_tbl(propnar1,1)
  propnartab<-docx_extract_tbl(propnar1,2)
  if(length(propnartab)>0){  
    keepr<-which(nchar(propnartab[,1])>0)
    outro<-t(propnartab[keepr,2])
    colnames(outro)<-propnartab[keepr,1]
    keepr<-which(nchar(propnartab[,3])>0)
    outro2<-t(propnartab[keepr,4])
    colnames(outro2)<-propnartab[keepr,3]
    outro<-data.frame(cbind(outro,outro2))
    outro$file<-tfile
    outro} else {data.frame("file"=tfile)}}

cell_extractor<-function(docxml,string1,string2){paste(docxml[c(which(str_detect(docxml, string1))+1):c(which(str_detect(docxml, string2))-1)],collapse="\n")}

gates_template_extractor<-function(file){
  print(file)
  file<-tfiles[1]
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
data.frame("Executive Summary"=cell_extractor(pvalues,"Provide a brief summary of your proposal.","Problem Statement"),"Problem Statement"=cell_extractor(pvalues,"Describe the problem","Scope and Approach"), "Scope and Approach"=cell_extractor(pvalues,"Scope and Approach","Measurement and Evaluation"),"Learning objetives"=cell_extractor(pvalues,"List the specific learning","If you are planning a formal evaluation"), "Formal eval plan"=cell_extractor(pvalues,"If you are planning a formal evaluation","Describe the resources"), "Resources"=cell_extractor(pvalues,"If you are planning a formal evaluation","Describe the resources"), "Risks"=cell_extractor(pvalues,"describe any significant risks","Organizational Fit and Capacity"), "OrgFit"=cell_extractor(pvalues,"What experience does your organization have to implement","Geographic Areas to Be Served"),"Location Beneficiaries"=cell_extractor(pvalues,"List all countries and regions/states that would benefit","Geographic Location of Work"),"Location Work Areas"=cell_extractor(pvalues,"List all countries and regions/states where this work","Intellectual Property"),"Intellectual Property"=cell_extractor(pvalues,"Intellectual Property","Clinical Studies"),"Data"=cell_extractor(pvalues,"We anticipate this investment","Results Framework"))
}
pvalues[c(which(str_detect(pvalues,"List all countries and regions/states where this work"))+10)]
outcome_extractor<-function(file){
 #file<-"../epar/PracticeDocs2/files/OPP1140962_2015_Ombech_Proposal_Narrative.docx"
 # file<-tfiles[14]
  print(file)
           temptab<-docx_extract_all_tbls(read_docx(file)) %>% .[str_detect(.,"Primary Outcomes")]
           if(length(temptab)>=1){
             temptab<-temptab[[1]]
           temptab$Unstandard<-NA
           }
           if(length(temptab)<1){temptab=data.frame("Primary Outcomes"=NA,"Intermediate Outcomes"=NA,"Outputs"=NA,"Unstandard"=NA)}
           if(ncol(temptab)<4){temptab=data.frame("Primary Outcomes"=NA,"Intermediate Outcomes"=NA,"Outputs"=NA,"Unstandard"=paste(temptab[,1:ncol(temptab)],collapse="\n",sep="\n"))}
           colnames(temptab)<-c("Primary Outcomes","Intermediate Outcomes","Outputs","Unstandard")
           for(i in 1:ncol(temptab)){temptab[,i]<-as.character(temptab[,i])}
temptab$file<-file
temptab
           }
  
gates<-bind_rows(lapply(tfiles,readfiletab_gates))
gates2<-lapply(tfiles,gates_template_extractor)
gates3<-bind_rows(lapply(tfiles,outcome_extractor))








   
pvalues[99]
pvalues <- pvalues[pvalues != ""]
if (skip > 0) 
  pvalues <- pvalues[-seq(skip)]
tempdes<-xml_text(xml_metadata)
xml_metadata<-xml_metadata %>% as_list() 

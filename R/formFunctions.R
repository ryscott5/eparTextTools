
#' This function allows building a form extactor using the simple approach of selecting the text string that occurs before and after a desired string.
#'
#' @param docxml The xml table for a word document.
#' @param string1 String to start capture on.
#' @param string2 String to end capture on.

#' @return a character vector of length one
#' @export
#' @description Read the primary table from a Gates proposal word document, keeping column names. 
#' @examples
#' gates3<-bind_rows(lapply(tfiles,outcome_extractor))
cell_extractor<-function(docxml,string1,string2){paste(docxml[c(which(str_detect(docxml, string1))+1):c(which(str_detect(docxml, string2))-1)],collapse="\n")}


#' Read in a Gates proposal template.
#'
#' @param tfile File to read in for scraping table
#' @return a data frame, if only file exists, there was an error.
#' @export
#' @description Read the primary table from a Gates proposal word document, keeping column names. 
#' @examples
#' gates<-bind_rows(lapply(tfiles,readfiletab_gates))
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



#' Extract Outcomes from Gates proposal documents
#'
#' @param file File to read in for scraping.
#' @return a data frame, if only file exists, there was an error.
#' @export
#' @description Extracts the outcome table from a Gates proposal document.
#' @examples
#' outcome_extractor()
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


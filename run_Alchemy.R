
rm(list=ls())
library(httr)
library(plyr)
library(dplyr)
library(stringr)
library(jsonlite)
source('../alchey.R')
workingfolder<-"../Research.Grants"
args = commandArgs(trailingOnly=TRUE)
FillFolder<-function(PREPFRAME,FOLDERNAME){
  library(httr)
  if(dir.exists(file.path(FOLDERNAME,"ALCHEMY"))==FALSE) {dir.create(file.path(FOLDERNAME,"ALCHEMY"))}
  for(i in args[1]:nrow(PREPFRAME)){
    X<-PREPFRAME$Sent[i]
    req <- POST("http://access.alchemyapi.com/calls/text/TextGetRelations", 
                body = list(apikey=ALKEY,text=X,keywords=1,outputMode="json",disambiguate=0), encode = "form",write_disk(file.path(FOLDERNAME,"ALCHEMY",paste("al",i,".txt",sep=""))))
    Sys.sleep(1)
  }}
recombine<-readRDS(file.path(workingfolder,'ProcessedFrame.rds'))
FillFolder(recombine,workingfolder)

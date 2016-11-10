#fittopicmodel
library(plyr)
library(dplyr)
library(stm)
workingfolder<-"../Ag.Nutrition.Grants"
baseinput<-readRDS(file.path(workingfolder,"basefile.rds"))
  st1<-stm(baseinput$out$documents,baseinput$out$vocab,data=baseinput$out$meta,prevalence=eval(parse(text=readLines(file.path(workingfolder,"formula1.txt")))),K=0, init.type="Spectral",max.em.its=1000)
saveRDS(st1, file.path(workingfolder,"topicmodel.rds"))




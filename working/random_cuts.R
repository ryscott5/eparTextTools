

ansub<-filter(anf,top.topics%in%c(56))
pfsub<-dplyr::filter(pf1, OpID%in%ansub$OpID)

saglabs<-sageLabels(st1,n=3)
saglabs$marginal$prob[56,]
?sageLabels
library(shiny)
library(shinyapps)
pred2<-pfsub
library(plotly)
head(pfsub)
runMap(pfsub[,2:ncol(pfsub)],path.file=FALSE,paste("Children's Nutrition Target Countries\nTopic including\n", paste(saglabs$marginal$prob[56,],collapse="-")))
data_mapper(pfsub[,2:ncol(pfsub)],unique(pfsub$OpID))
pf_fake<-pfsub
pf_fake$OpID<-as.factor(pf_fake$OpID)
runMap(pf_fake[,2:ncol(pf_fake)],path.file=FALSE,paste("Children's Nutrition Target Countries\nTopic including\n", paste(saglabs$marginal$prob[56,],collapse="-")))
pred2<-pf_fake
pf_fake$OpID<-as.character(pf_fake$OpID)

levels(pf_fake$OpID)<-paste("Grant",round(runif(10,1000,2000)))
data_mapper(pf_fakce],unique(pf_fake))

head(pfsub)



ansub<-filter(anf,top.topics%in%c(56))
pfsub<-dplyr::filter(pf1, OpID%in%ansub$OpID)

saglabs<-sageLabels(st1,n=3)
saglabs$marginal$prob[56,]
?sageLabels
library(shiny)
library(shinyapps)
pred2<-pfsub
library(plotly)
head(pfsub)
runMap(pfsub[,2:ncol(pfsub)],path.file=FALSE,paste("Children's Nutrition Target Countries\nTopic including\n", paste(saglabs$marginal$prob[56,],collapse="-")))
data_mapper(pfsub[,2:ncol(pfsub)],unique(pfsub$OpID))
pf_fake<-pfsub
pf_fake$OpID<-as.factor(pf_fake$OpID)
runMap(pf_fake[,2:ncol(pf_fake)],path.file=FALSE,paste("Children's Nutrition Target Countries\nTopic including\n", paste(saglabs$marginal$prob[56,],collapse="-")))
pred2<-pf_fake
pf_fake$OpID<-as.character(pf_fake$OpID)

levels(pf_fake$OpID)<-paste("Grant",round(runif(10,1000,2000)))
data_mapper(pf_fakce],unique(pf_fake))

head(pfsub)
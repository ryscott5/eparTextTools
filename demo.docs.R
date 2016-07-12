demo.docs<-c("http://docs.gatesfoundation.org/documents/guide-to-actionable-measurement.pdf","http://docs.gatesfoundation.org/Documents/EvaluationsHS.pdf","http://docs.gatesfoundation.org/Documents/The%20Strategy%20Lifecycle.pdf","http://docs.gatesfoundation.org/Documents/GD%20Progress%20Report%20Guidelines.doc","http://docs.gatesfoundation.org/Documents/Guideline_GD_Proposal.doc")
dir.create("demo.docs.folder")
library(downloader)
lapply(demo.docs,function(X) {download(X, destfile=file.path("demo.docs.folder",tail(unlist(strsplit(X,split="/",fixed=TRUE)),n=1)))})

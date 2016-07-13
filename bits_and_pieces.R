bits and pieces


freqterms<-lapply(1:length(corpus2), function(X){findFreqTerms(tdm[,X],lowfreq=15)})
lapply(freqterms,wordcloud)
mfcomframe<-data.frame(inspect(tdm[unique(do.call(c,freqterms)),]))
mfcomframe<-mfcomframe[sort(rowSums(mfcomframe),index.return=TRUE,decreasing=TRUE)$ix[1:10],]
mfcomframe$word<-row.names(mfcomframe)
mfcomframe$word<-factor(mfcomframe$word, levels = mfcomframe$word)
mfcomframe<-melt(mfcomframe,id=c("word"))

sort(rowSums(as.matrix(tdm[unique(do.call(c,freqterms)),])),decreasing=TRUE)
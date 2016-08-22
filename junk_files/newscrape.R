library(rvest)
basepage<-lapply(0:11,function(X){Sys.sleep(2)
  read_html(paste("http://evans.uw.edu/centers-and-projects/epar/research?combine=&&&&&&&page=",X,sep=""))})
tlinks<-do.call(c,lapply(basepage,function(X){X %>% html_nodes(".views-field-title a") %>% rvest::html_attr("href")}))
tlinks<-paste("http://evans.uw.edu",tlinks,sep="")
tlinks2<-lapply(tlinks,function(X){Sys.sleep(1)
  read_html(X) %>% html_nodes("#main_content a") %>% html_attr("href")})
tlinks2<-melt(tlinks2)
validlinks<-unique(do.call(c,lapply(c(".pdf",".doc",".docx"),function(K){which(str_detect(tlinks2$value,fixed(K))==TRUE)})))
tlinks.run<-tlinks2$value[validlinks] 
tlinks.run<-gsub("https","http",tlinks.run)
write.csv(data.frame("links"=tlinks.run),"demodoc_links.csv")
lapply(tlinks.run, function(X){download(X, destfile=file.path("demo.docs.folder",tail(unlist(strsplit(X,split="/",fixed=TRUE)),n=1)))})



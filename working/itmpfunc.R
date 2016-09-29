
interactiveTable<-function(TITLE,OUTPUTFILENAME,RDATAFILE){
X<-as.character(readLines("InteractiveM_template.txt")))
X<-gsub("DTITLENAME",TITLE,X,fixed=T)
X<-gsub("DFILENAME",RDATAFILE,X,fixed=T)
markdownToHTML(output=OUTPUTFILENAME,text=X)
view("demo.html")}
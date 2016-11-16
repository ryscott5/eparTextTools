library(shiny)
library(shinyapps)
library(data.table)
library(DT)
DT::Data
head(matchtable[,3])
library(plyr)
library(dplyr)
colnames(matchtable)


tableapp<-function(basic_table,TOPICMODEL){
  basic_table<-basic_table %>% select(OpID,id,TopTopics,subject.keywords,action.lemmatized,object.keywords,everything())
  wordpossibles<-colnames(basic_table)[unlist(sapply(c("subject","action","object"),function(X) which(stringr::str_detect(colnames(basic_table),X))))]
  basic_table<-basic_table %>% select(-one_of("Sent","Orig"))
  for(i in which(sapply(1:ncol(basic_table), function(X) class(basic_table[,X]))=="list")){
    basic_table[,i]<-sapply(basic_table[,i],function(X){paste(unlist(X),collapse=";")})}
basic_table$sentence<-stringr::str_trim(basic_table$sentence)
  shinyApp(ui = fluidPage(sidebarLayout(
      sidebarPanel(selectizeInput("OpIDselect","Opportunity ID",choices=unique(basic_table$OpID),multiple=TRUE),
        selectInput("NOV", label = "Select Word Type",choices = wordpossibles, selected = wordpossibles[1]),
                   textInput("word",label= "Word", value=""),
                   checkboxInput("sent",label="Show Sentence",value=FALSE),
                  checkboxInput("ltops","Label Topics with High P Words?",value=FALSE),
                   sliderInput("Kclusters", label = "Number of Words",min = 1, max = 25, value = 5, step = 1),
        uiOutput("wordselect"),
                   checkboxInput('Shorten',label='Shorten Strings?', value=TRUE)),
      mainPanel(dataTableOutput("table1")))), 
  server = function(input, output){
    matchwords<-reactive({names(nearest_to2(TOPICMODEL,input$word,n=input$Kclusters,fixword=FALSE,limitwords=NULL)[[2]])})
    matchwords2<-reactive({matchwords()[matchwords()%in%input$topicWords]})
    t1t<-reactive({
      t1<-basic_table
      if(is.null(input$OpIDselect)==FALSE){t1<-dplyr::filter(t1, OpID%in%input$OpIDselect)}
      if(input$sent==FALSE){t1 <- t1 %>% select(-one_of("sentence"))}
      if(nchar(input$word)>1){
        t1<-t1[unlist(sapply(matchwords2(),function(X) {which(stringr::str_detect(t1[,input$NOV],X))})),]
      }
      if(input$ltops==TRUE){
          labsOut<-stm::sageLabels(TOPICMODEL,n=5)$marginal$prob
          sapply(1:nrow(labsOut),function(i) paste(labsOut[i,],collapse=";"))
          t1$TopTopics<-sapply(1:nrow(labsOut),function(i) paste(labsOut[i,],collapse=";"))[t1$TopTopics]}
      t1
    })
    output$table1<-renderDataTable({
      if(input$Shorten==TRUE){
        datatable(t1t(),rownames=FALSE,filter ='bottom',extensions = 'Buttons', options = list(dom = 'Bfrtip',scrollX=TRUE, buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'),columnDefs = list(list(targets =  c(which(names(t1t()) %in% c("sentence","ents"))-1),render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 10 ?","'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;","}")))), callback = JS('table.page(3).draw(false);'))} else {datatable(t1t(),rownames=FALSE,filter ='bottom',extensions = 'Buttons', options = list(dom = 'Bfrtip',scrollX=TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')), callback = JS('table.page(3).draw(false);'))}
    })
    output$choice <- DT::renderDataTable({matchwords()})
    output$wordselect<-renderUI({checkboxGroupInput("topicWords", "Words to Include",choices=matchwords(),selected=matchwords())})
  }
    )}
  
fullapp<-function(basic_table,TOPICMODEL){
  basic_table<-basic_table %>% select(OpID,id,TopTopics,subject.keywords,action.lemmatized,object.keywords,everything())
  wordpossibles<-colnames(basic_table)[unlist(sapply(c("subject","action","object"),function(X) which(stringr::str_detect(colnames(basic_table),X))))]
  basic_table<-basic_table %>% select(-one_of("Sent","Orig"))
  for(i in which(sapply(1:ncol(basic_table), function(X) class(basic_table[,X]))=="list")){
    basic_table[,i]<-sapply(basic_table[,i],function(X){paste(unlist(X),collapse=";")})}
  basic_table$sentence<-stringr::str_trim(basic_table$sentence)
  shinyApp(ui = fluidPage(sidebarLayout(
    sidebarPanel(selectizeInput("OpIDselect","Opportunity ID",choices=unique(basic_table$OpID),multiple=TRUE),
                 selectInput("NOV", label = "Select Word Type",choices = wordpossibles, selected = wordpossibles[1]),
                 textInput("word",label= "Word", value=""),
                 checkboxInput("sent",label="Show Sentence",value=FALSE),
                 checkboxInput("ltops","Label Topics with High P Words?",value=FALSE),
                 sliderInput("Kclusters", label = "Number of Words",min = 1, max = 25, value = 5, step = 1),
                 uiOutput("wordselect"),
                 checkboxInput('Shorten',label='Shorten Strings?', value=TRUE)),
    mainPanel(dataTableOutput("table1")))), 
    server = function(input, output){
      matchwords<-reactive({names(nearest_to2(TOPICMODEL,input$word,n=input$Kclusters,fixword=FALSE,limitwords=NULL)[[2]])})
      matchwords2<-reactive({matchwords()[matchwords()%in%input$topicWords]})
      t1t<-reactive({
        t1<-basic_table
        if(is.null(input$OpIDselect)==FALSE){t1<-dplyr::filter(t1, OpID%in%input$OpIDselect)}
        if(input$sent==FALSE){t1 <- t1 %>% select(-one_of("sentence"))}
        if(nchar(input$word)>1){
          t1<-t1[unlist(sapply(matchwords2(),function(X) {which(stringr::str_detect(t1[,input$NOV],X))})),]
        }
        if(input$ltops==TRUE){
          labsOut<-stm::sageLabels(TOPICMODEL,n=5)$marginal$prob
          sapply(1:nrow(labsOut),function(i) paste(labsOut[i,],collapse=";"))
          t1$TopTopics<-sapply(1:nrow(labsOut),function(i) paste(labsOut[i,],collapse=";"))[t1$TopTopics]}
        t1
      })
      output$table1<-renderDataTable({
        if(input$Shorten==TRUE){
          datatable(t1t(),rownames=FALSE,filter ='bottom',extensions = 'Buttons', options = list(dom = 'Bfrtip',scrollX=TRUE, buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'),columnDefs = list(list(targets =  c(which(names(t1t()) %in% c("sentence","ents"))-1),render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 10 ?","'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;","}")))), callback = JS('table.page(3).draw(false);'))} else {datatable(t1t(),rownames=FALSE,filter ='bottom',extensions = 'Buttons', options = list(dom = 'Bfrtip',scrollX=TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')), callback = JS('table.page(3).draw(false);'))}
      })
      output$choice <- DT::renderDataTable({matchwords()})
      output$wordselect<-renderUI({checkboxGroupInput("topicWords", "Words to Include",choices=matchwords(),selected=matchwords())})
    }
  )}


tableapp(matchtable,st1)
runmap
colnames(matchtable)

names(nearest_to2(st1,"potato",n=10,fixword=FALSE,limitwords=NULL)[[2]])
, options = list(
  dom = 'Bfrtip',
  scrollX=TRUE,
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'), callback = JS('table.page(3).draw(false);'))
?renderTable
?conditionalPanel
conditionalPanel(
  condition = "length(input.word)>=1",
            )

runMap

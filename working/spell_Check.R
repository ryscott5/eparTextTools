install.packages('qdap')
library(qdap)
qdap::check_spelling(
length(st1$vocab)
compVocab<-tm::stemCompletion(st1$vocab,readRDS("savedObjects/basecorpus.rds"))
compVocab %>% 
chspell<-qdap::check_spelling(compVocab)
chspell$row
st1$vocab[chspell$row][1:100]


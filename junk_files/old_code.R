#Template for text analysis in R
#--------------------------------------------------#

#set colors for Graphs
EPAR_Colors_1 <-  c("#ffffff", "#e5f0ec", "#8ebfad", "#417361", "#e8e3ee", "#9179af", "#3d2f4f")
EPAR_Colors_2 <-  c("#9179af", "#8ebfad")


#SECTION 2: Most frequent terms overall

#calculate column totals (number of occurences per term across all documents)
totals <- colSums(df[,-1], na.rm = TRUE) #[,-1] specifies that we want all rows and all columns except column 1, na.rm=TRUE means we omit the missing variables
#save as a dataframe for graphing, re-sort highest to lowest frequency
colSums(tdm)
names(tdm)
#Template for text analysis in R

#Input: term-document matrix .csv file from Python
#Output: text analysis graphs

#clear workspace
rm(list = ls())

#--------------------------------------------------#
#SECTION 1: Set-up

#install packages (uncomment if you haven't installed them on your computer yet)
#install.packages("reshape2")
#install.packages("ggplot2")

#load packages (do not comment out)
library("reshape2")
library("ggplot2")

#set colors for Graphs
EPAR_Colors_1 <-  c("#ffffff", "#e5f0ec", "#8ebfad", "#417361", "#e8e3ee", "#9179af", "#3d2f4f")
EPAR_Colors_2 <-  c("#9179af", "#8ebfad")

#import .csv file


df <- read.csv("R:/Project/EPAR/EPAR Templates and Guidelines/Text Analysis Procedures and Templates/Project_Template_Python/Code/frequencyMatrix - Copy.csv")
#Replace NA values with zeros
df[is.na(df)] <- 0
#--------------------------------------------------#
#SECTION 2: Most frequent terms overall

#calculate column totals (number of occurences per term across all documents)
totals <- colSums(df[,-1], na.rm = TRUE) #[,-1] specifies that we want all rows and all columns except column 1, na.rm=TRUE means we omit the missing variables
#save as a dataframe for graphing, re-sort highest to lowest frequency
totals <- as.data.frame(sort(totals, decreasing = TRUE)) 
#Rename the column name as "Frequency"
names(totals)[1]<-"Frequency"
#add the rownames as a real column
totals <- cbind(Word=rownames(totals), totals)
#make it a factor so the order is maintained in a graph - ordering the Words based on their Frequency
totals$Word <- factor(totals$Word, levels=totals[order(totals$Frequency, decreasing=TRUE), "Word"])

#Graph the top 15 terms in a bar plot
graph_1 <- ggplot(totals[1:15,], aes(Word, Frequency)) +   
  geom_bar(fill="#8ebfad", position = "dodge", stat="identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Frequency")+
  ggtitle("Most Common Words Across All Grants")
graph_1

#--------------------------------------------------#
#SECTION 3: Creating a sub-list with terms of interest

#Creating a dataframe with only gendered words (binding selected columns from the larger dataframe)
df.gender <- df[, c("filename", "woman", "girl", "wife", "female", "men", "boy", "husband", "male")]
#total frequencies per word across all documents:
totals.gender <- colSums(df.gender[, -1], na.rm = TRUE)
df.gender <- cbind(df.gender, totals.gender)
#add word names to the gender totals dataframe
totals.gender <- as.data.frame(totals.gender)
totals.gender <- cbind(word=rownames(totals.gender), totals.gender)
#make it a factor so the order is maintained in a graph - ordering the words based on their frequency
totals.gender$word <- factor(totals.gender$word, levels=totals.gender[order(totals.gender$totals.gender, decreasing=TRUE), "word"])

#Graph the overall frequencies
graph_2 <- ggplot(totals.gender, aes(word, totals.gender)) +   
  geom_bar(fill="#8ebfad", position = "dodge", stat="identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Frequency")+
  ggtitle("Gender Word Frequencies Across all Documents")
graph_2

#Alternative graph: color-coding by gender
#add a column to the totals.gender data frame that specifies the gender of the word
totals.gender <- cbind(totals.gender, gender = c("female", "female", "female", "female", "male", "male", "male", "male"))
#graph - color fill is dependent on the "gender" variable
graph_3 <- ggplot(totals.gender, aes(word, totals.gender, fill=gender)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=14),panel.background=element_blank())+
  xlab("")+
  ylab("Word count, all proposals")+
  guides(fill=FALSE)+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_3

#--------------------------------------------------#
#SECTION 4: Comparing the selected words across documents

#Making a graph that shows the relative frequency of female versus male terms between grant documents
#New dataframe
count.df <- as.data.frame(df$filename)
#rename the columns
colnames(count.df) <- "filename"
#Calculate the sum of frequencies of all female words in a given grant
count.df$total.female <- df$woman + df$girl + df$female + df$wife
#Calculate the sum of frequencies of all male words in a given grant
count.df$total.male <- df$boy + df$men + df$male + df$husband
#Calculate total number of words per grant
count.df$total.words <- rowSums(df[-1])
#Calcuate relative frequency of female words per grant (total female words / total words)
count.df$freq.female <- count.df$total.female/count.df$total.words
#Calcuate relative frequency of male words per grant (total male words / total words)
count.df$freq.male <- count.df$total.male/count.df$total.words

#new data frame for frequency
frequency.df <- count.df[, c("filename", "freq.female", "freq.male")]
#melt the data
frequency.df.melt <- melt(frequency.df, id.vars="filename")
#Graph relative frequencies by grant document
graph_4 <-ggplot(frequency.df.melt, aes(x=filename, y=value)) +   
  geom_bar(aes(fill=variable), position = "dodge",  stat = "identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Relative Frequency of Document Words")+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_4

#Same as above, but this time graph the total counts of words per grant
count.df <- count.df[, c("filename", "total.female", "total.male")]
#melt the data
count.df.melt <- melt(count.df, id.vars="filename")
#Graph counts by grant document
graph_4 <-ggplot(count.df.melt, aes(x=filename, y=value)) +   
  geom_bar(aes(fill=variable), position = "dodge",  stat = "identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Count of Document Words")+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_4

#Same as above, but only for the Asian grants
count.asia.melt <- melt(count.df[c(2, 9, 12:13, 17:20),], id.vars="filename") #selecting only the rows of Asian grants
graph_5 <-ggplot(count.asia.melt, aes(x=filename, y=value)) +   
  geom_bar(aes(fill=variable), position = "dodge",  stat = "identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Count of Document Words")+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_5

totals <- as.data.frame(sort(totals, decreasing = TRUE)) 
#Rename the column name as "Frequency"
names(totals)[1]<-"Frequency"
#add the rownames as a real column
totals <- cbind(Word=rownames(totals), totals)
#make it a factor so the order is maintained in a graph - ordering the Words based on their Frequency
totals$Word <- factor(totals$Word, levels=totals[order(totals$Frequency, decreasing=TRUE), "Word"])

#Graph the top 15 terms in a bar plot
graph_1 <- ggplot(totals[1:15,], aes(Word, Frequency)) +   
  geom_bar(fill="#8ebfad", position = "dodge", stat="identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Frequency")+
  ggtitle("Most Common Words Across All Grants")
graph_1

#--------------------------------------------------#
#SECTION 3: Creating a sub-list with terms of interest

#Creating a dataframe with only gendered words (binding selected columns from the larger dataframe)
df.gender <- df[, c("filename", "woman", "girl", "wife", "female", "men", "boy", "husband", "male")]
#total frequencies per word across all documents:
totals.gender <- colSums(df.gender[, -1], na.rm = TRUE)
df.gender <- cbind(df.gender, totals.gender)
#add word names to the gender totals dataframe
totals.gender <- as.data.frame(totals.gender)
totals.gender <- cbind(word=rownames(totals.gender), totals.gender)
#make it a factor so the order is maintained in a graph - ordering the words based on their frequency
totals.gender$word <- factor(totals.gender$word, levels=totals.gender[order(totals.gender$totals.gender, decreasing=TRUE), "word"])

#Graph the overall frequencies
graph_2 <- ggplot(totals.gender, aes(word, totals.gender)) +   
  geom_bar(fill="#8ebfad", position = "dodge", stat="identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Frequency")+
  ggtitle("Gender Word Frequencies Across all Documents")
graph_2

#Alternative graph: color-coding by gender
#add a column to the totals.gender data frame that specifies the gender of the word
totals.gender <- cbind(totals.gender, gender = c("female", "female", "female", "female", "male", "male", "male", "male"))
#graph - color fill is dependent on the "gender" variable
graph_3 <- ggplot(totals.gender, aes(word, totals.gender, fill=gender)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=14),panel.background=element_blank())+
  xlab("")+
  ylab("Word count, all proposals")+
  guides(fill=FALSE)+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_3

#--------------------------------------------------#
#SECTION 4: Comparing the selected words across documents

#Making a graph that shows the relative frequency of female versus male terms between grant documents
#New dataframe
count.df <- as.data.frame(df$filename)
#rename the columns
colnames(count.df) <- "filename"
#Calculate the sum of frequencies of all female words in a given grant
count.df$total.female <- df$woman + df$girl + df$female + df$wife
#Calculate the sum of frequencies of all male words in a given grant
count.df$total.male <- df$boy + df$men + df$male + df$husband
#Calculate total number of words per grant
count.df$total.words <- rowSums(df[-1])
#Calcuate relative frequency of female words per grant (total female words / total words)
count.df$freq.female <- count.df$total.female/count.df$total.words
#Calcuate relative frequency of male words per grant (total male words / total words)
count.df$freq.male <- count.df$total.male/count.df$total.words

#new data frame for frequency
frequency.df <- count.df[, c("filename", "freq.female", "freq.male")]
#melt the data
frequency.df.melt <- melt(frequency.df, id.vars="filename")
#Graph relative frequencies by grant document
graph_4 <-ggplot(frequency.df.melt, aes(x=filename, y=value)) +   
  geom_bar(aes(fill=variable), position = "dodge",  stat = "identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Relative Frequency of Document Words")+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_4

#Same as above, but this time graph the total counts of words per grant
count.df <- count.df[, c("filename", "total.female", "total.male")]
#melt the data
count.df.melt <- melt(count.df, id.vars="filename")
#Graph counts by grant document
graph_4 <-ggplot(count.df.melt, aes(x=filename, y=value)) +   
  geom_bar(aes(fill=variable), position = "dodge",  stat = "identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Count of Document Words")+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_4

#Same as above, but only for the Asian grants
count.asia.melt <- melt(count.df[c(2, 9, 12:13, 17:20),], id.vars="filename") #selecting only the rows of Asian grants
graph_5 <-ggplot(count.asia.melt, aes(x=filename, y=value)) +   
  geom_bar(aes(fill=variable), position = "dodge",  stat = "identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Count of Document Words")+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_5


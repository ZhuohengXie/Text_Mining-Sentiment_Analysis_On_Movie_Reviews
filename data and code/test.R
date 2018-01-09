rm(list=ls()) 
setwd("F:\\4Data Mining\\project\\aclImdb\\test\\test_new") 
data <- readLines("F:\\4Data Mining\\project\\aclImdb\\test\\test_new\\file.txt")  
x<-c(1,1)

a=sample(c(1:25000),size=5000)

for(i in 1:5000){
  c=data[a[i]] 
  x[i]=read.csv(c, header=FALSE, sep="\t" ) 
  ##x[i]
  ##x$i<-x[i]
}



##reviews = x
##A1=review[,1]
##A2=review[,2]
##review = review[,c<- x

library(plyr)
library(ggplot2)
library(tm)
library(lsa)
corpus = Corpus(VectorSource(x))
corpus
##pre-process
## check if the texts are in lower case
corpus = tm_map(corpus, content_transformer(tolower))
inspect(corpus[1:3])
## check if punctuations are removed
corpus = tm_map(corpus, removePunctuation)
inspect(corpus[1:3])
## check if numbers are removed
corpus = tm_map(corpus, removeNumbers)
inspect(corpus[1:3])
## check if stopwords are removed
myStopwords <- c(stopwords('english')
                 , "my","movie","film","one","just","character","just","get","even","can") 
corpus = tm_map(corpus, function(x) removeWords(x, myStopwords))
inspect(corpus[1:3]) 
## check the stemming
corpus = tm_map(corpus, stemDocument, language = "english")
inspect(corpus[1:3]) 
# check corpus
corpus = tm_map(corpus, stripWhitespace)
inspect(corpus[1:3]) 
corpus





##term-document matrix
##corpus <- tm_map(corpus, PlainTextDocument)
td.mat = TermDocumentMatrix(corpus)
##td.mat.w = TermDocumentMatrix(corpus,control=list(weighting=weightTfIdf))
terms=findFreqTerms(td.mat, lowfreq=1000)
td.mat=td.mat[terms,]
##td.mat.w=td.mat.w[terms,]

##select=which(test$V1%in%a)
classify=data.frame(classify[a,])

td.mat=removeSparseTerms(td.mat, sparse=0.998)
library(RTextTools)
container <- create_container(
  t(td.mat), classify[,1], 
   trainSize=1:4500,
   testSize=4501:5000, virgin=FALSE)

SVM <- train_model(container,"SVM")
RF <- train_model(container,"RF")
##MAXENT <- train_model(container,"MAXENT")
##TREE <- train_model(container,"TREE")

SVM_CLASSIFY <- classify_model(container, SVM)
RF_CLASSIFY <- classify_model(container, RF)
##MAXENT_CLASSIFY <-classify_model(container,MAXENT)
##TREE_CLASSIFY <-classify_model(container,TREE)


analytics <- create_analytics(container,
                              cbind(SVM_CLASSIFY,RF_CLASSIFY))
summary(analytics)
SVM <- cross_validate(container, 4, "SVM")
RF <- cross_validate(container, 4, "RF")
##MAXENT <- cross_validate(container, 4, "MAXENT")
##TREE <- cross_validate(container, 4, "TREE")






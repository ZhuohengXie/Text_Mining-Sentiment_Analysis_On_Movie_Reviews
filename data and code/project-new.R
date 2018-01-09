rm(list=ls()) 
setwd("F:\\4Data Mining\\project\\aclImdb\\train\\pos") 
data <- readLines("F:\\4Data Mining\\project\\aclImdb\\train\\pos\\file.txt")  
x<-c(1,1)

a=sample(c(1:12500),size=3000)

for(i in 1:3000){
  c=data[a[i]] 
  x[i]=read.csv(c, header=FALSE, sep="\t" ) 
  ##x[i]
  ##x$i<-x[i]
}

train_positive=train_positive[a,]

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
corpus = tm_map(corpus, function(x) removeWords(x, stopwords("english")))
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
td.mat = TermDocumentMatrix(corpus))
td.mat.w = TermDocumentMatrix(corpus,control=list(weighting=weightTfIdf))
terms=findFreqTerms(td.mat, 1000)
td.mat=td.mat[terms,]
td.mat.w=td.mat.w[terms,]

lsa.space = lsa(td.mat.w,dims=3)  ## create LSA space
dist.mat = dist(t(as.textmatrix(lsa.space)))  ## compute distance matrix
dist.mat 

doc.mds = cmdscale(dist.mat, k = 2)
data = data.frame(x = doc.mds[, 1], y = doc.mds[, 2], topic = train_positive$V2, id = row.names(train_positive))
ggplot(data, aes(x = x, y = y, color=topic)) + 
  geom_point() + 
  geom_text(aes(x = x, y = y - 0.2, label = id))


##cloud
library(wordcloud)
m = as.matrix(td.mat)
## calculate the frequency of words
v = sort(rowSums(m), decreasing=TRUE) 
words = names(v)
wc = data.frame(word=words, freq=v)
wc[1:3,]
wordcloud(wc$word, wc$freq, min.freq=2)

## run LDA topic model
library(topicmodels)
lda = LDA(td.mat, 4)
terms(lda)
topics(lda)

## run NMF
library(NMF)
# V ~ WH' 
# V is an n x p matrix
# W = n x r  term feature matrix
# H = r x p  doc feature matrix
set.seed(12345)
res = nmf(as.matrix(td.mat), 3,"lee") # lee & seung method
V.hat = fitted(res) 
dist.mat = dist(t(as.textmatrix(V.hat))) 
doc.mds = cmdscale(dist.mat, k = 2)
data = data.frame(x = doc.mds[, 1], y = doc.mds[, 2], topic = train_positive$V2, id = row.names(train_positive))
ggplot(data, aes(x = x, y = y, color=topic)) + 
  geom_point() + 
  geom_text(aes(x = x, y = y - 0.2, label = id))









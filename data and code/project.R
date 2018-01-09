rm(list=ls()) #擦出所有变量 
setwd("F:\\4Data Mining\\project\\aclImdb\\train\\pos") #设置工作路径 
file.txt #准备路径及文件名文件 
data <- readLines("F:\\4Data Mining\\project\\aclImdb\\train\\pos\\file.txt") #路径文件读入 
x<-c(1,1)
for(i in 1:12500){
c=data[i] #提取单元格中的路径信息 
x[i]=read.csv(c, header=FALSE, sep="\t" ) #使用路径导入索引文件 data
x[i]
##x$i<-x[i]
}


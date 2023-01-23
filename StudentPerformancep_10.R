library(readxl)
library(tidyr)
library(janitor)
library(caTools)
library(caret)
library(tidyverse)
library(class)
library(cluster)
library(ggplot2)
library(ggpubr)
library(kernlab)
library(arules)
library(readr)
library(lattice)
library(MASS)
library(factoextra)
library(fpc)
library(e1071)
library(rpart)
library(rpart.plot)
#Read the data
StudentsPerformance <- read_excel("StudentsPerformance (1).xlsx")

View(StudentsPerformance)
#data properties
class(StudentsPerformance)
StudentsPerformance <- as.data.frame(StudentsPerformance)#convert the data to data farme
class(StudentsPerformance)
dim(StudentsPerformance)#395 row & 16 col
summary(StudentsPerformance)
#number of missing data
sum(is.na(StudentsPerformance)) # 0
#remove duplicates
StudentsPerformance <- unique(StudentsPerformance)
#------------------------------convert the data to numeric-----------------------
#make female =1 and male = 0
StudentsPerformance$sex<-ifelse(StudentsPerformance$sex=="F",1,0)
#convert GP to 1 and MS to 0
StudentsPerformance$school<-ifelse(StudentsPerformance$school=="GP",1,0)
#convert yes to 1 and no to 0
StudentsPerformance$internet<-ifelse(StudentsPerformance$internet=="yes",1,0)
StudentsPerformance$romantic<-ifelse(StudentsPerformance$romantic=="yes",1,0)
#convert each job to value from 0 to 4
StudentsPerformance[StudentsPerformance == "teacher"] <- 0 
StudentsPerformance[StudentsPerformance == "other"] <- 1
StudentsPerformance[StudentsPerformance == "services"] <- 2 
StudentsPerformance[StudentsPerformance == "health"] <- 3 
StudentsPerformance[StudentsPerformance == "at_home"] <- 4 
#change the data type of two columns to numeric data tpe
StudentsPerformance$Fjob <- as.numeric(StudentsPerformance$Fjob)
StudentsPerformance$Mjob <- as.numeric(StudentsPerformance$Mjob)
#----------------------------------Visualization---------------------------------
#Examine G1
summary(StudentsPerformance$G1)
sd(StudentsPerformance$G1)
var(StudentsPerformance$G1)
plot(density(StudentsPerformance$G1),col = rainbow(2))

#Examine G2
summary(StudentsPerformance$G2)
sd(StudentsPerformance$G2)
var(StudentsPerformance$G2)
plot(density(StudentsPerformance$G2),col = rainbow(1))

#Examine G3
summary(StudentsPerformance$G3)
sd(StudentsPerformance$G3)
var(StudentsPerformance$G3)
plot(density(StudentsPerformance$G3),col = rainbow(21))

# Examine sex
summary(StudentsPerformance$sex)
median(StudentsPerformance$sex)
plot(as.factor(StudentsPerformance$sex),col = rainbow(2))

#Examine age
summary(StudentsPerformance$age)
sd(StudentsPerformance$age)
var(StudentsPerformance$age)
plot(density(StudentsPerformance$age),col = rainbow(22))

#boxplot for different cols
boxplot(StudentsPerformance$age,  main="age" ,ylab="age")
#outline=FALSE if we want to hide the outliers from the boxplot
boxplot(StudentsPerformance$age,  main="age" ,ylab="age" ,outline=FALSE) #ex for removing outliers


boxplot(StudentsPerformance$studytime, main="studytime",ylab="Stime")

#boxblot for G1 and G2 and G3
boxplot(StudentsPerformance$G1, StudentsPerformance$G2, StudentsPerformance$G3,col = rainbow(12),ylab="grades" ,main="G1...G2....G3" )

#boxblot for G1 and sex
boxplot(G1 ~ sex, data = StudentsPerformance, main="G1 and sex",ylab="G1",col = rainbow(6))

#boxblot for G2 and sex
boxplot(G2 ~ sex, data = StudentsPerformance, main="G2 and sex", ylab="G2",col = rainbow(10))

#boxblot for G3 and sex
boxplot(G3 ~ sex, data = StudentsPerformance, main="G3 and sex",   ylab="G3",col = rainbow(2))

#histogram
with(StudentsPerformance, { hist(G1, main="G1 distribution",   freq=FALSE,col = rainbow(2))})

with(StudentsPerformance, {hist(G2, main="G2 distribution",   freq=FALSE,col = rainbow(3))})

with(StudentsPerformance, {hist(G3, main="G3 distribution",   freq=FALSE,col = rainbow(4))})

with(StudentsPerformance, {hist(age, main="age distribution",   freq=FALSE,col = rainbow(4))})

#ggplot for G1 and studytime
ggplot(data=StudentsPerformance, aes(x=as.factor(studytime), y=G1) ) +geom_point(position="identity", alpha=0.2 ) +  
  geom_boxplot(alpha=0.1, outlier.size=3 ) 

#relation between grades and study time
ggplot(data=StudentsPerformance) + geom_density(aes(x=G1, colour=as.factor(studytime))) 
ggplot(data=StudentsPerformance) + geom_density(aes(x=G2, colour=as.factor(studytime))) 
ggplot(data=StudentsPerformance) + geom_density(aes(x=G3, colour=as.factor(studytime))) 
#------------------------------------Removing outliers---------------------------
#count the outliers in age col
StudentsPerformance$age[StudentsPerformance$age %in% boxplot.stats(StudentsPerformance$age)$out]
#remove the outlier
StudentsPerformance<- subset(StudentsPerformance,StudentsPerformance$age >=0 & StudentsPerformance$age<22)
#count the outliers in G2 col
StudentsPerformance$G2[StudentsPerformance$G2 %in% boxplot.stats(StudentsPerformance$G2)$out]
#remove the out lier
StudentsPerformance<- subset(StudentsPerformance,StudentsPerformance$G2 >=2.4 & StudentsPerformance$G2<21)
#box plot after removing the outliers

boxplot(StudentsPerformance$G2, main = "Grade 2",
        xlab = "Grades",
        col = "orange", border = "brown",
        horizontal = TRUE, notch = TRUE)
#count the outliers in G3 col
StudentsPerformance$G3[StudentsPerformance$G3 %in% boxplot.stats(StudentsPerformance$G3)$out]
#remove the out lier
StudentsPerformance<- subset(StudentsPerformance,StudentsPerformance$G3 >=3.9 & StudentsPerformance$G3<21)

#---------------------------Kmeans------------------------------------------#
#split the data to training and testing 
split <- sample.split(StudentsPerformance$internet, SplitRatio = 0.7)
traind <- subset(StudentsPerformance, split == "TRUE")
testd <- subset(StudentsPerformance, split == "FALSE")
#Apply kmean clustering
km <- kmeans(traind, centers = 3, nstart = 100)
#To know the best value for k
wss<- numeric(15)
for (i in 1:15) wss[i]<-sum( kmeans(traind,centers = i)$withinss)
plot(1:15 , wss , type = "b" , xlab="num of clusters" , ylab="within groups")
#Plot clusters
plotcluster(traind, km$cluster)

#--------------------------------- Hierarchical Clustering -------------------------------------------
distance <- dist(traind, method = 'euclidean')
Hierarchical <- hclust(distance, method = "average")
plot(Hierarchical)
k_clust <- cutree(Hierarchical, k = 3 )
k_clust
table(k_clust)
rect.hclust(Hierarchical, k = 3, border = 2:5)
#----------------------------------Decission tree-------------------------------------------
tree1 <- rpart(internet ~school+sex+age+goout+studytime+failures+absences+G1+G2+G3, data=traind, method = 'class')
rpart.plot(tree1)
#summary(tree1)
predict_unseen <-predict(tree1, testd, type = 'class')
print(predict_unseen)

matrix1=table(testd[,8],predict_unseen)
matrix1
accuracy <- sum(diag(matrix1))/sum(matrix1)
accuracy

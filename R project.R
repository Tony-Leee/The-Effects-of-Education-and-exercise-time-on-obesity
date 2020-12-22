# The project will study the obesity rate of American adults in 2014, and explore the influence of exercise time and education on the obesity rate of adults by regression
# Because the obesity rate is subject to normal distribution through AD test, linear regression is done





library(ggplot2)
library(car)
library(Hmisc)
library(gplots)

work1=read.csv(file = "F:\\NPL.csv")
head(work1)
names(work1)


work2<-work1[,c(1,4,8,11,12,21,31)]# Choose the right data
names(work2)






# Data cleaning
work3<-(which(work2[,7]=='Education'))# select the data set for education
work3
data1=work2[work3,]
head(data1)

data2<-(which(data1[,3]=="Percent of adults aged 18 years and older who have obesity"))
data_question<-data1[data2,]
head(data_question)# select the data set of adult obesity rate



data3<-(which(data_question[,2]!="National"))# Get rid of the three regions that are not the United States
data_location=data_question[data3,]



data4<-(which(data_location[,2]!="Guam"))
data_location2=data_location[data4,]


data5<-(which(data_location2[,2]!="Puerto Rico"))
work_end=data_location2[data5,]
head(work_end)


# Visualize the data and observe the adult obesity rate of college graduates in American States in 2014

names(work_end)



data14<-(which(work_end[,1]=="2014"))# select data for 2014
data2014=work_end[data14,]
head(data2014)



da<-data2014[,c(2,4,6)]
head(da)


data_edu<-(which(da[,3]=="College graduate"))# choose the data of university graduation
collage_data<-da[data_edu,]
head(collage_data)

ggplot(collage_data,aes(Data_Value,LocationDesc))+geom_bar(stat="identity",fill="steelblue")




# Test the normality of the dependent variable

library("nortest")
ad.test(work_end$Data_Value)# Dependent variable obesity rate obeys normal distribution

summary(work_end)
describe(work_end)

# Do multiple regression linear regression
a<-which(work_end$Education=='Less than high school' )# Assign four kinds of education. High school education and below are assigned as 0. High school education and above are assigned as 1

    work_end$Education[a]<-0

    
b<-which(work_end$Education=='High school graduate' )
    work_end$Education[b]<-0
    
  
c<-which(work_end$Education=='Some college or technical school' )
    work_end$Education[c]<-1
    

d<-which(work_end$Education=='College graduate' )
    work_end$Education[d]<-1

head(work_end)

work_1<-(which(work_end[,1]=="2014"))# select data for 2014
work_gre=work_end[work_1,]
head(work_gre)
opar <- par(no.readonly=TRUE) 
par(mfrow=c(1,3)) 
attach(work_gre)
hist(work_gre$Data_Value, breaks=20, xlab="obesity rate", col = "blue",
     main="Distribution of obesity rate") 
boxplot(work_gre$Data_Value ~ work_gre$Education, xlab="education category", main="Group education",col="red") 
hist(work_gre$Exercise.time,xlab = "exercise time",main="distribution of exercise time", col="pink")
par(opar)






 
 
# Fitting regression equation
 
 fit <- lm(Data_Value~ Exercise.time+Education, data=work_gre)
 
 summary(fit)
 confint(fit)

 exp(coef(fit)) 
 coef(fit)

library(ggplot2)

 
ggplot(work_gre,aes(x=Exercise.time,y=Data_Value), shape = sex, colour = sex,ylab("Obesity rate")) + geom_point()
 
summary(work_gre)
describe(work_gre) 
sd(work_gre$Exercise.time)
 
 
 
 
 
 
 
 
 
 
 
























































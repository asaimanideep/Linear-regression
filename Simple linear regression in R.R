#simple linear regression

#Importing the dataset
dataset<-read.csv("SalaryData.csv")

#Splitting the dataset into training set and testing set
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary,SplitRatio = 0.8)
training_set<-subset(dataset,split == TRUE)
testing_set<-subset(dataset,split == FALSE)

#feature scaling
#training_set[,2:3]=scale(training_set[,2:3])
#testing_set[,2:3]=scale(testing_set[,2:3])

#Fitting simple linear regression to training set
regressor=lm(formula = Salary ~ YearsExperience,
              data=training_set)
summary(regressor)

#predict the testset results
y_pred=predict(regressor,newdata = testing_set)
y_pred

#visualizing the training set results
#install.packages("ggplot2")
library(ggplot2)
ggplot()+
  geom_point(aes(x=training_set$YearsExperience,y=training_set$Salary),
             colour="red")+
  geom_line(aes(x=training_set$YearsExperience,y=predict(regressor,newdata = training_set)),
            colour='blue')+
  ggtitle('Salary vs Experience(Training_set)')+
  xlab('Years of experience') +
  ylab('Salary')

#visualizing the testing set results
#install.packages("ggplot2")
library(ggplot2)
ggplot()+
  geom_point(aes(x=testing_set$YearsExperience,y=testing_set$Salary),
             colour="red")+
  geom_line(aes(x=training_set$YearsExperience,y=predict(regressor,newdata = training_set)),
            colour='blue')+
  ggtitle('Salary vs Experience(Training_set)')+
  xlab('Years of experience') +
  ylab('Salary')


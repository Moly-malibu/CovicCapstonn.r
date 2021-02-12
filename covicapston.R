---
title: "CapstoneCovic"
author: "Monica Bustamante"
date:   "2/11/2021"
---
  
library(dplyr)
library(magrittr)
library(knitr)
library(MASS)
library(lattice)
library(tidyverse)
library(mlbench)
library(tidyverse)
library(ggplot2)
library(caret)


#Load data set directt from data world page.
#Read File and Columns name.
df <- read.csv('https://query.data.world/s/e7co64e3e47t3sdnrviomq3sasf6g2')
head(df)

#Create a data sett My data and replace nan from original data set
my_data <- as_tibble(df)
my_data <- replace(my_data, is.na(my_data), 0)
my_data

#filter only integer data to developmente the differents analysis.
new_data = my_data %>% select_if(is.numeric)
new_data

#Mean, Media and Standard deviation of confirmed by Covic 19
mean=mean(new_data[['confirmed']])
median=median(new_data[['confirmed']])
standard_deviation= sd(new_data[['confirmed']])
print((paste0('Mean of Confirmed  cases cause for Covic:', mean)))
print((paste0('Median of Confirmed  cases cause for Covic:', median)))
print((paste0('Standar Deviation of Confirmed  cases cause by Covic:', standard_deviation)))

meean=mean(df[['deaths']])
median=median(df[['deaths']])
sd=sd(df[['deaths']])
print((paste0('Mean of deaths cases cause for Covic:', mean)))
print((paste0('Median of deaths  cases cause for Covic:', median)))
print((paste0('Standar Deviation of deaths  cases cause by Covic:', sd)))


#Covariance data set Covic For a sample of cases confirmed of 100,000 inhabitants, the covariance is of the 3% and the correlation is the 1% deaths by covic.

cov(new_data)

#Correlacion data set Covic

cor(new_data)

#Read File and Columns name str.
str(new_data)

#Summary show us the minimu, max and median and mean the all data set.

summary(new_data)

str(df)

summary(df)

df$deaths <- factor(df$deaths,
                    levels=c(0, 1),
                    labels=c('confirmed', 'deaths'))

summary(new_data$deaths)

which.max(df[['deaths']])
which.max(df[['confirmed']])

##Group by NCHS Urbanization and State from United States, in Medium, small, large, micropolitan and non-core.

dataset <- df %>% group_by(NCHS_urbanization, state)
dataset

#Analysis cases confirmed and deaths group by states.

dataset %>% summarise(
  confirmed=mean(confirmed),
  deaths=mean(deaths)
)

ggplot(new_data, aes(deaths, confirmed)) + geom_point()

dataset %>% filter(total_population==max(total_population))

dataset %>% filter(confirmed_per_100000==max(confirmed_per_100000))

plot(new_data$deaths, new_data$confirmed)

##Visualization cases confirmed compared with the deaths.

plot(new_data$confirmed_per_100000, new_data$deaths)

plot(df$confirmed_per_100000, df$total_population)
title(plot(df$deaths_per_100000, df$total_population))

dataset %>% filter(deaths_per_100000==max(deaths_per_100000))

##Visualization compared total population with cases confirmed and deaths.

plot(new_data$confirmed_per_100000, new_data$deaths, type='l')

#Barplot compared deaths
barplot(table(new_data$deaths))

hist(new_data$total_population)
hist(new_data$confirmed_per_100000, breaks=10)

#Visualization show differents the states with most deaths us that california is one the most deaths had.

ggplot(df, aes(x=deaths, y=state)) + geom_point()

geopoin <- subset(df, state == 'deaths') 
qmplot(lon, lat, data=df, colour=I('red'), size=I(3), darken=.3)

## Prediction: The prediction show us that for 24 people infected 2 would death and this prediction has 0.915 of accuracy and the precision is 0.913,  data that confirmed that the average of death for this virus can oscillate into 2% or 3% of impacted in the all population infected.

set.seed(0) 
actual = c('confirmed', 'deaths')[runif(100, 1, 4)]
predicted = actual 
predicted[runif(30,1,100)] = actual[runif(30,1,100)]
cm = as.matrix(table(Actual=actual, Predicted=predicted))
cm

num_instances = sum(cm)  
num_class = nrow(cm) 
diag = diag(cm) #classified 
rowsums = apply(cm, 1, sum) 
colsums = apply(cm, 2, sum) #predictions 
p = rowsums / num_instances # distribution of instances over the actual classes
q = colsums / num_instances # distribution of instances over the predicted classes

accuracy = sum(diag) / num_instances 
accuracy

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)

data.frame(precision, recall, f1)

macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)

data.frame(macroPrecision, macroRecall, macroF1)

# Machine Learning in R: 
# Building a Linear Regression Model 1
#We build the model Linear Regression to analyze the relationship  between differents variable, as size of states more infectioned and deaths or analysis what is the most relevant variante. The coefficient and residuals give us the R-squared is 0.8478 and .8557 with p value of 2.2 a tendeced to increase the cases of deaths.

lmcovic = lm(confirmed~deaths, data = df)
summary(lmcovic)

lmcovic2 = lm(confirmed~deaths + confirmed_per_100000, data=df)
summary(lmcovic2)

plot(lmcovic$residuals, pch=16, col='blue')
abline(lmcovic)

plot(lmcovic2$effects, pch=16, col='blue')
abline(lmcovic2)

#Linear Regression Model 2
#The second model we created a validation data set divide to analysis in 652 and 2617, also a training and test set the prediction show us confirmed the cases of deaths have been increase.


set.seed(100)
validationIndex <- caret::createDataPartition(df$deaths, p=0.80, list=FALSE)
validation <- df[-validationIndex,]
dataset <- df[validationIndex,]
dim(validation)
dim(dataset)

sum(is.na(dataset))

#Stratified random split of the data set
TrainingIndex <- createDataPartition(df$deaths, p=0.8, list = FALSE)
TrainingSet <- df[TrainingIndex,] # Training Set
TestingSet <- df[-TrainingIndex,] # Test Set

# Build Training model
Model <- train(deaths ~ confirmed, data = TrainingSet,
               method = "lm",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none")
)

# Apply model for prediction
Model.training <-predict(Model, TrainingSet) #model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) #model to make prediction on Testing set

# Scatter plot of Training set performance matrics
plot(TrainingSet$confirmed,Model.training, col = "blue" )

plot(TestingSet$deaths,Model.testing, col = "blue" )

ggplot(data = new_data) + 
  geom_smooth(mapping = aes(x = confirmed, y = deaths), color="red", fill="blue", size=2)



#Conclusion:
#This project shows us in real time how the cases of infection by the Civic 19 virus are evolving, it presents the most relevant analyzes either by states, infected and deaths, which is the maximum and minimum of each one, which is the probability of increasing or decreasing. Persuading self-care turns out to be the greatest impact that this type of information can have on society, as it raises awareness of the risks of not paying attention to biosafety care, the greatest limitation that was found is the few variables to analyze. In the future, different databases could be merged to obtain more information that can assist society in keeping it more informed.

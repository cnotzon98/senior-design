
# Load in library
library(leaps)
library(rpart)
library(rpart.plot)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(tree)

FBDataCSV<- read.csv("FBDataCSV.csv")
fbd<- FBDataCSV         #19,437-rows     8-columns
str(fbd)
names(fbd)
head(fbd)

fbd$Client.Preferred.Agency <- with(fbd, ifelse(grepl("Interfaith Food Bank", Client.Preferred.Agency), 1, 0))
fbd$City <- as.factor(fbd$City)
fbd$Household.Primary.Income.Source <- as.factor(fbd$Household.Primary.Income.Source)
fbd$Housing.Type<- as.factor(fbd$Housing.Type)
fbd$Client.Disability <- as.factor(fbd$Client.Disability)

#make sure you attach the data otherwise the variables do not apper 
# and dollar sign is necessary to find the names of the var
attach(fbd)

#count total missing values in entire data frame
sum(is.na(fbd))     #10 values missing
fbd <- fbd[complete.cases(fbd),]     #19,432     8
sum(is.na(fbd))   #now we have 0 values missing in the data
#glimpse(fbd)

#summary(Client.Age)
#hist(Client.Age)
trainfb<- sample(1:nrow(fbd), nrow(fbd)/2)
treefb<- tree(Household.Size~., fbd, subset = trainfb)
summary(treefb)

plot(treefb)
text(treefb, pretty = 0)

cvfb<- cv.tree(treefb)
plot(cvfb$size, cvfb$dev, type = 'b')
prunefb<- prune.tree(treefb, best=6)
plot(prunefb)
text(prunefb, pretty=0)

yhat<- predict(treefb, newdata = fbd[-trainfb, ])
mean(yhat)
fbtest<- fbd[-trainfb, "Household.Size"]
mean(fbtest)
mean((yhat-fbtest)^2)

#The MSE associated with the regression tree is 2.260551. The square
#root of the MSE is therefore around 1.503513 indicating that this 
#model leads to test predictions that are within around 1.5 of the 
#Household size for the dataset



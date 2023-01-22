rm(list = ls())

library(pacman)
p_load(ggplot2, dplyr, tidyverse, tidyr, dplyr, BiocManager, corplot, corrplot, 
       RColorBrewer,stargazer, caret, car)


#------------------------------------------------------------------------------#
#                           DATA LOADING                                       #
#------------------------------------------------------------------------------#

#Read the data
#the file is heavy so we will work with R native format
#data = read.csv("01 data/Car_Insurance_Claim.csv", stringsAsFactors = T)
#saveRDS(data, "01 data/Car_Insurance_Claim.rds")

data = readRDS("01 data/Car_Insurance_Claim.rds")

names(data)
names(data) = tolower(names(data))

str(data)

#View(data)
#head(data)


#------------------------------------------------------------------------------#
#                             DATA UNDERSTANDING                               #
#------------------------------------------------------------------------------#

# $ id                 : int  569520 750365 199901 478866 731664 877557 930134 461006 68366 445911 ...
# $ age                : Factor w/ 4 levels "16-25","26-39",..: 4 1 1 1 2 3 4 2 3 3 ...
# $ gender             : Factor w/ 2 levels "female","male": 1 2 1 2 2 1 2 1 1 1 ...
# $ race               : Factor w/ 2 levels "majority","minority": 1 1 1 1 1 1 1 1 1 1 ...
# $ driving_experience : Factor w/ 4 levels "0-9y","10-19y",..: 1 1 1 1 2 3 4 1 3 1 ...
# $ education          : Factor w/ 3 levels "high school",..: 1 2 1 3 2 1 1 3 3 1 ...
# $ income             : Factor w/ 4 levels "middle class",..: 3 2 4 4 4 3 3 4 4 3 ...
# $ credit_score       : num  0.629 0.358 0.493 0.206 0.388 ...
# $ vehicle_ownership  : num  1 0 1 1 1 1 0 0 0 1 ...
# $ vehicle_year       : Factor w/ 2 levels "after 2015","before 2015": 1 2 2 2 2 1 1 1 2 2 ...
# $ married            : num  0 0 0 0 0 0 1 0 1 0 ...
# $ children           : num  1 0 0 1 0 1 1 1 0 1 ...
# $ postal_code        : int  10238 10238 10238 32765 32765 10238 10238 10238 10238 32765 ...
# $ annual_mileage     : num  12000 16000 11000 11000 12000 13000 13000 14000 13000 11000 ...
# $ vehicle_type       : Factor w/ 2 levels "sedan","sports car": 1 1 1 1 1 1 1 1 1 1 ...
# $ speeding_violations: int  0 0 0 0 2 3 7 0 0 0 ...
# $ duis               : int  0 0 0 0 0 0 0 0 0 0 ...
# $ past_accidents     : int  0 0 0 0 1 3 3 0 0 0 ...
# $ outcome            : num  0 1 0 0 1 0 0 1 0 1 ...

#------------------------------------------------------------------------------#
#                              DATA PREPARATION                                #
#------------------------------------------------------------------------------#
#check NA values
data_copy = data

x = data %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))

library(xtable)
xtable(t(x), caption = "Variables with NA values", )
#View(t(x))

hist(data$credit_score)
levels(data$income)

# mean(data$credit_score[data$income=="middle class"], na.rm = T)
# 0.5236585
# median(data$credit_score[data$income=="middle class"], na.rm = T)
# 0.5267473

data$credit_score[data$income=="middle class" & is.na(data$credit_score)] = median(data$credit_score[data$income=="middle class"], na.rm = T)
data$credit_score[data$income=="poverty" & is.na(data$credit_score)] = median(data$credit_score[data$income=="poverty"], na.rm = T)
data$credit_score[data$income=="upper class" & is.na(data$credit_score)] = median(data$credit_score[data$income=="upper class"], na.rm = T)
data$credit_score[data$income=="working class" & is.na(data$credit_score)] = median(data$credit_score[data$income=="working class"], na.rm = T)

hist(data$annual_mileage)
data$annual_mileage[is.na(data$annual_mileage)] = mean(data$annual_mileage, na.rm = T)

str(data$education)
levels(data$education)
table(data$education)


#------------------------------------------------------------------------------#
#                               EDA UNIVARIATE                                 #
#------------------------------------------------------------------------------#

hist(data$outcome)

#------------------------------------------------------------------------------#
#                               EDA MULTIVARIATE                               #
#------------------------------------------------------------------------------#
str(data)

###############
#   AGE
###############
# Stacked barplot with multiple groups
dat <- data.frame(table(data$age,data$outcome))
names(dat) <- c("Age","Outcome","Count")
#ggplot(data=dat, aes(x=Age, y=Count, fill=Outcome)) + geom_bar(stat="identity")
a=ggplot(data=dat, aes(x=Age, y=Count, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge())

###############
#   GENDER
###############
# Stacked barplot with multiple groups
dat <- data.frame(table(data$gender,data$outcome))
names(dat) <- c("Gender","Outcome","Count")
#ggplot(data=dat, aes(x=Gender, y=Count, fill=Outcome)) + geom_bar(stat="identity")
b=ggplot(data=dat, aes(x=Gender, y=Count, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge())

###############
#   race
###############
dat <- data.frame(table(data$race,data$outcome))
names(dat) <- c("Race","Outcome","Count")
c=ggplot(data=dat, aes(x=Race, y=Count, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge())

###############
#   married
###############
table(data$married)
data$married = factor(data$married, c(0,1), labels = c("No married", "Married"))
dat <- data.frame(table(data$married,data$outcome))
names(dat) <- c("Married","Outcome","Count")
d=ggplot(data=dat, aes(x=Married, y=Count, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge())

###############
#   EDUCATION
###############
data$education = factor(data$education, levels = c("none","high school","university"), 
                        labels = c("no grade", "high school","university"))
#data$education = factor(data$education, levels = c("none", "high school", "university"))
# Stacked barplot with multiple groups
dat <- data.frame(table(data$education,data$outcome))
names(dat) <- c("Education","Outcome","Count")
#ggplot(data=dat, aes(x=Age, y=Count, fill=Outcome)) + geom_bar(stat="identity")
e =ggplot(data=dat, aes(x=Education, y=Count, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge())

###############
#   income
###############
data$income = factor(data$income, levels = c("poverty", "working class", "middle class", "upper class"))
dat <- data.frame(table(data$income,data$outcome))
names(dat) <- c("Income","Outcome","Count")
f=ggplot(data=dat, aes(x=Income, y=Count, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge())

###############
#   vehicle ownership
###############
table(data$vehicle_ownership)
data$vehicle_ownership = factor(data$vehicle_ownership, labels =c("No owner", "Owner"))
dat <- data.frame(table(data$vehicle_ownership,data$outcome))
names(dat) <- c("Vehicle_ownership","Outcome","Count")
g=ggplot(data=dat, aes(x=Vehicle_ownership, y=Count, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge())

###############
#   driving_experience
###############
dat <- data.frame(table(data$driving_experience,data$outcome))
names(dat) <- c("Driving_experience","Outcome","Count")
h=ggplot(data=dat, aes(x=Driving_experience, y=Count, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge())

###############
#   vehicle year
###############
data$vehicle_year = factor(data$vehicle_year, levels = c("before 2015", "after 2015"))
table(data$vehicle_year)
dat <- data.frame(table(data$vehicle_year,data$outcome))
names(dat) <- c("Vehicle_year","Outcome","Count")
i=ggplot(data=dat, aes(x=Vehicle_year, y=Count, fill=Outcome)) +
  geom_bar(stat="identity", position=position_dodge())

# table(data$vehicle_type)
# table(data$speeding_violations)
# table(data$duis)
# table(data$past_accidents)

str(data)

library(gridExtra)
library(ggpubr)
ggarrange(a, b,c,d,e,f, 
           labels = c("A", "B","C","D","E","F"),
           ncol = 2, nrow = 3)

ggarrange(g, h,i, 
          labels = c("G", "H","I"),
          ncol = 1, nrow = 3)
#------------------------------------------------------------------------------#
#                               FEATURES MATRIX                                #
#------------------------------------------------------------------------------#
#saveRDS(data, "01 data/Car_Insurance_Claim_clean.rds")
rm(list = ls())
data = readRDS("01 data/Car_Insurance_Claim_clean.rds")
data = data %>% select(-c("id","postal_code"))
str(data)

table(data$outcome)
data$outcome = factor(data$outcome, levels = c(0,1), labels = c("No Claim", "Claim"))

dat2 <- data.frame(lapply(data[1:16], function(x) as.numeric(x)))
data = cbind(dat2, data$outcome)
rm(dat2)

#### normalize
normalize = function(x) {return((x-min(x))/(max(x) - min(x)))}

data_n = as.data.frame(lapply(data[1:16], normalize))

set.seed(24)
num_obs = nrow(data)
train_index = sample(num_obs, size = trunc(0.80 * num_obs))

data_train = data_n[train_index, ]
data_test = data_n[-train_index, ]

data_train_labels = data[train_index, 17]
data_test_labels = data[-train_index, 17]

str(data_n)

################################################################################
#------------------------------------------------------------------------------#
#                         MODELING -  KNN                        #
#------------------------------------------------------------------------------#
################################################################################
library(class)

# x_train = data_train[1:16]
# y_train = data_train[17]
# x_test = data_test[1:16]
# y_test = data_test[17]

##fitting model
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}

calc_class_err(actual = data_test_labels,
               predicted = knn(train = data_train,
                               test  = data_test,
                               cl    = data_train_labels,
                               k     = 5))

set.seed(24)
k_to_try = 1:100
err_k = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
  pred = knn(train = data_train, 
             test  = data_test, 
             cl    = data_train_labels, 
             k     = k_to_try[i])
  err_k[i] = calc_class_err(data_test_labels, pred)
}


# plot error vs choice of k
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors")
# add line for min error seen
abline(h = min(err_k), col = "darkorange", lty = 3)
# add line for minority prevalence in test set
abline(h = mean(data_test_labels == "Claim"), col = "grey", lty = 2)


min(err_k)
which(err_k == min(err_k))
max(which(err_k == min(err_k)))

knn_pred = knn(train = data_train,
               test = data_test,
               cl = data_train_labels,
               k=39)

#install.packages("gmodels")
library(gmodels)
CrossTable(x = data_test_labels, 
           y= knn_pred, prop.chisq = FALSE)

CFM = table(data_test_labels, knn_pred)
CFM
xtable(CFM, caption = "Confusion Matrix KNN model")

Class_accuracy = sum(diag(CFM)/sum(CFM))
Class_accuracy


################################################################################
#------------------------------------------------------------------------------#
#                         MODELING - Random Forest                          #
#------------------------------------------------------------------------------#
################################################################################
#install.packages("randomForest")
library(randomForest)

x = cbind(data_train, data_train_labels)

RFM = randomForest(data_train_labels~., 
                   data = x,
                   ntree=500)

summary(RFM)
predicted = predict(RFM, data_test)
#getTree(RFM, 1, labelVar=TRUE)


# install.packages("devtools")
# library(devtools)
# devtools::install_github('araastat/reprtree')
# library(reprtree)
#library(reprtree)
#reprtree:::plot.getTree(RFM)
#View(as.data.frame(cbind(data_test_labels, predicted)))


CrossTable(x = data_test_labels, 
           y= predicted, prop.chisq = FALSE)

CFM = table(data_test_labels, predicted)
Class_accuracy = sum(diag(CFM)/sum(CFM))
Class_accuracy

################################################################################
#------------------------------------------------------------------------------#
#                              PRESENTATION                                    #
#------------------------------------------------------------------------------#
################################################################################


######## REFERENCES

# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

# https://www.youtube.com/watch?v=acFviblzijU
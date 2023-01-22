################################################################################
#------------------------------------------------------------------------------#
#                         MODELING -  SVM                        #
#------------------------------------------------------------------------------#
################################################################################
str(data)
?svm
fit_svm = svm(severity~., data=data_train,
              type="C-classification", kernal="radial", 
              gamma=0.1, cost=10) 

summary(fit_svm)

svm_pred = predict(fit_svm, data_test)

View(cbind(data_test$severity, svm_pred))
table(data_test$severity, svm_pred)

CrossTable(x = data_test$severity, 
           y= svm_pred, prop.chisq = FALSE)

CFM = table(data_test$severity, svm_pred)
Class_accuracy = sum(diag(CFM)/sum(CFM))
Class_accuracy




################################################################################
#------------------------------------------------------------------------------#
#                         MODELING -  KNN                        #
#------------------------------------------------------------------------------#
################################################################################
rm(list = ls())

#saveRDS(data, "01 data/US_Accidents_2016to2020_clean.rds")
data = readRDS("01 data/US_Accidents_2016to2020_clean.rds")
str(data)

data = data[,-c(2,3,6,7)]

library(class)

table(data$severity)
data$severity = factor(data$severity, levels = c(1,2,3,4), labels = c("Lower", "Mid-lower", "Mid-high", "High"))

dat2 <- data.frame(lapply(data[2:30], function(x) as.numeric(x)))
data = cbind(dat2, data$severity)
rm(dat2)

data %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))

#### normalize
normalize = function(x) {return((x-min(x))/(max(x) - min(x)))}

data_n = as.data.frame(lapply(data[1:29], normalize))

data_n = data_n[,-25]

set.seed(24)
num_obs = nrow(data)
train_index = sample(num_obs, size = trunc(0.80 * num_obs))

data_train = data_n[train_index, ]
data_test = data_n[-train_index, ]

data_train_labels = data[train_index, 30]
data_test_labels = data[-train_index, 30]

#round(sqrt(nrow(data_train)))
### 869

##fitting model
knn_pred = knn(train = data_train,
               test = data_test,
               cl = data_train_labels,
               k=89)

#install.packages("gmodels")
library(gmodels)
CrossTable(x = data_test_labels, 
           y= knn_pred, prop.chisq = FALSE)


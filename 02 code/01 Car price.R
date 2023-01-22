#------------------------------------------------------------------------------#
#                             LIBRARIES                                        #
#------------------------------------------------------------------------------#
rm(list = ls())

library(pacman)
p_load(ggplot2, dplyr, tidyverse, tidyr, dplyr, BiocManager, corplot, corrplot, 
       RColorBrewer,stargazer, caret, car)

#------------------------------------------------------------------------------#
#                           DATA LOADING                                       #
#------------------------------------------------------------------------------#

#Read the data
data = read.csv("01 data/data_carprice.csv", stringsAsFactors = T)

names(data)
names(data) = tolower(gsub("\\.", "_", names(data)))
names(data)

str(data)
#View(data)
#head(data)

#------------------------------------------------------------------------------#
#                             DATA UNDERSTANDING                               #
#------------------------------------------------------------------------------#

        # 11,914 rows x 16 columns
## columns
# Make: car brand
        # Independent variable with 48 levels
# Model: model of the car
        # Independent variable with 915
# Year: year of the car
        # Independent Variable between 1990 and 2017
# Engine_fuel_type: type
        # Independent Variable with 11 levels
        # [1] ""                                             "diesel"                                      
        # [3] "electric"                                     "flex-fuel (premium unleaded recommended/E85)"
        # [5] "flex-fuel (premium unleaded required/E85)"    "flex-fuel (unleaded/E85)"                    
        # [7] "flex-fuel (unleaded/natural gas)"             "natural gas"                                 
        # [9] "premium unleaded (recommended)"               "premium unleaded (required)"                 
        # [11] "regular unleaded" 
# Engine_HP: engine horsepower
        # Independent Variable - Integer
# Engine_cylinders: engine cylinder
        # Independent Variable - Integer        
# Transmision_type: if the car us Manual or Automatic
        # Independent Variable with 5 levels
# Driven_wheels:  if the driver has control of all the wheels
        # Independent Variable with 4 levels
        #can colapse category
# Number_of_doors: how many doors the car has
        # Independent Variable - Integer
# Market_category: main concepts which describe the car
    # there are many problems with the variable, also a lot of NA
    # we can eliminate the variable
        # Independent Variable with 72 levels
# vehicle size: the size of the vehicle
        # Independent Variable with 3 levels
# vehicle style: type of vehicle
        # Independent Variable with 16 levels
# Highway.mpg: how many miles per galon
        # Independent Variable - Integer
# city.mpg: city miles per galon
        # Independent Variable - Integer
# Popularity: ranking of popularity based of comments and mention of the brand
        # Independent Variable - Integer (RANGE from 2 to 5657)
# msrp: manufacturer's suggested retail price (MSRP) 
        # Dependent Variable - Integer
        # [1]    range to 2,000-2,065,902

#------------------------------------------------------------------------------#
#                              DATA PREPARATION                                #
#------------------------------------------------------------------------------#

#check NA values
x = data %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))

xtable(t(x), caption = "Variables with NA values")
###### Imputing by web searching

####ENGINE HP
      # hp = kW x 1.34102
#            FOR TRANSFORM KILOWHATS TO HP
na_engine_hp = data[is.na(data$engine_hp),]

#data %>% filter(make=="FIAT" & model=="500e")
data$engine_hp[data$make=="FIAT" & data$model=="500e"] = 13
data$engine_hp[data$make=="Lincoln" & data$model=="Continental" & data$engine_fuel_type=="premium unleaded (recommended)"] = 400
#data %>% filter(make=="Ford" & model=="Escape")
#Ford Escape 2017               regular unleaded       168 
data$engine_hp[data$make=="Ford" & data$model=="Escape" & data$engine_fuel_type=="regular unleaded"] = 168
data$engine_hp[data$make=="Honda" & data$model=="Fit EV"] = 123
data$engine_hp[data$make=="Ford" & data$model=="Focus" & data$engine_fuel_type=="electric"] = 143
data$engine_hp[data$make=="Ford" & data$model=="Freestar" & data$year==2005 & data$engine_fuel_type=="regular unleaded"] = 193
data$engine_hp[data$make=="Mitsubishi" & data$model=="i-MiEV"] = 66
data$engine_hp[data$make=="Chevrolet" & data$model=="Impala" & data$engine_fuel_type=="flex-fuel (unleaded/natural gas)"] = 305
data$engine_hp[data$make=="Nissan" & data$model=="Leaf" & data$engine_fuel_type=="electric"] = 107
data$engine_hp[data$make=="Mercedes-Benz" & data$model=="M-Class" & data$engine_fuel_type=="diesel"] = 240
data$engine_hp[data$make=="Lincoln" & data$model=="MKZ" & is.na(data$engine_hp)==T] = 188
data$engine_hp[data$make=="Tesla" & data$model=="Model S" & data$year==2014] = 380 #mean of the year sum(302,362,416)/3
data$engine_hp[data$make=="Tesla" & data$model=="Model S" & data$year==2015] = 302
data$engine_hp[data$make=="Tesla" & data$model=="Model S" & data$year==2016] = 315
data$engine_hp[data$make=="Toyota" & data$model=="RAV4 EV" & data$engine_fuel_type=="electric"] = 154
data$engine_hp[data$make=="Kia" & data$model=="Soul EV" & data$engine_fuel_type=="electric"] = 109
na_engine_hp = data[is.na(data$engine_hp),]
rm(na_engine_hp)

na_engine_cylinders = data[is.na(data$engine_cylinders),]
  #data %>% filter(engine_fuel_type=="electric")
data$engine_cylinders[data$engine_fuel_type=="electric"] = 0
data$engine_cylinders[data$make=="Mazda" & data$model=="RX-7" | data$model=="RX-8"] = 4
na_engine_cylinders = data[is.na(data$engine_cylinders),]
rm(na_engine_cylinders)

na_number_of_doors = data[is.na(data$number_of_doors),]
data$number_of_doors[data$make=="Tesla" & is.na(data$number_of_doors)==T] = 4
data$number_of_doors[data$make=="Ferrari" & is.na(data$number_of_doors)==T] = 2
na_number_of_doors = data[is.na(data$number_of_doors),]
rm(na_number_of_doors)

#remove NA
data = na.omit(data) #we solved all the NA by missing information
str(data)

#------------------------------------------------------------------------------#
#                               EDA UNIVARIATE                                 #
#------------------------------------------------------------------------------#

stargazer(data,
          title = "Statistical Descriptive",
          digits = 2,
          label = "tab1")

summary(data$msrp)
        # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
        # 2000   21000   29995   40595   42231 2065902

range(data$msrp)
        # [1]    2000 2065902

data$log_msrp = log(data$msrp)

# plot_msrp = ggplot(data, aes(x= msrp)) +
#   geom_histogram(bins = 8, binwidth = 50000, aes(fill = ..count..)) +
#   scale_x_continuous(breaks = seq(0,max(data$msrp), 250000)) +
#   scale_y_continuous(breaks = seq(0,10000,1000)) +
#   labs(title = "Plot showing count of car MSRP (Price)",
#        x = "Price", y = "Count of cars")
# 
# plot_msrp
h1 = hist(data$msrp, main = "Histogram of MSRP")
h2 = hist(data$log_msrp, main = "Histogram of log MSRP")


################################################################################
#reorder the factor of make by price
# table(data$make)
# make_tab=data %>% group_by(make) %>% summarise(avg=mean(msrp))
# make_tab=make_tab[order(make_tab$avg),]
# make_list=c(make_tab$make)
# #data$make_list = data$make
# data$make = factor(data$make, levels = make_list)
# levels(data$make)
################################################################################


#table(data$model) #plot(data$model)
#hist(data$year)
#View(table(data$engine_fuel_type))
        ### have to recode

# data$engine_fuel_type[data$engine_fuel_type=="flex-fuel (premium unleaded recommended/E85)" |
#                         data$engine_fuel_type=="flex-fuel (premium unleaded required/E85)" |
#                         data$engine_fuel_type=="flex-fuel (unleaded/E85)" |
#                         data$engine_fuel_type=="flex-fuel (unleaded/natural gas)"]
# tab_1 = data %>% group_by(engine_fuel_type) %>% summarize(mean(msrp))
# View(tab_1)
data %>% filter(engine_fuel_type=="")
data %>% filter(make=="Suzuki" & model=="Verona")
data$engine_fuel_type[data$engine_fuel_type==""] = "regular unleaded"


hist(data$engine_hp)
hist(data$engine_cylinders)


plot(data$transmission_type)
table(data$transmission_type)
#data %>% filter(transmission_type=="UNKNOWN")
      #recoding
data$transmission_type[data$make=="Oldsmobile" & data$model=="Achieva" & 
                         data$year==1997] = "MANUAL"
data$transmission_type[data$make=="Pontiac" & data$model=="Firebird" & 
                         data$transmission_type=="UNKNOWN"] = "MANUAL"
data$transmission_type[data$make=="GMC" & data$model=="Jimmy" & 
                         data$transmission_type=="UNKNOWN"] = "MANUAL"
data$transmission_type[data$make=="Chrysler" & data$model=="Le Baron" & 
                         data$transmission_type=="UNKNOWN"] = "AUTOMATIC"
data$transmission_type[data$make=="Dodge" & data$model=="RAM 150" & 
                         data$transmission_type=="UNKNOWN"] = "MANUAL"
#data %>% filter(transmission_type=="UNKNOWN")
table(data$transmission_type, data$make)
data$transmission_type = factor(data$transmission_type, levels = c("MANUAL","AUTOMATED_MANUAL","AUTOMATIC","DIRECT_DRIVE"))
plot(data$transmission_type)

table(data$driven_wheels)
data$driven_wheels[data$driven_wheels=="all wheel drive"] = "four wheel drive"
data$driven_wheels = factor(data$driven_wheels, levels = c("front wheel drive", "rear wheel drive", "four wheel drive"))
table(data$driven_wheels)

hist(data$number_of_doors)
table(data$market_category) #this variable won't be useful

table(data$vehicle_size)
data$vehicle_size = factor(data$vehicle_size, levels = c("Compact", "Midsize", "Large"))
table(data$vehicle_size)

table(data$vehicle_style) #probably won't use again

hist(data$highway_mpg)
hist(log(data$highway_mpg))

hist(data$city_mpg)
hist(log(data$city_mpg))

hist(data$popularity)
#hist(log(data$popularity))


#------------------------------------------------------------------------------#
#                               EDA MULTIVARIATE                               #
#------------------------------------------------------------------------------#

#library(caret)
featurePlot(x = data[ , c("year", "engine_hp", "engine_cylinders", 
                          "number_of_doors", "highway_mpg", "city_mpg", 
                          "popularity")], y = data$log_msrp)

plot(data$highway_mpg, data$msrp)

hist(log(data$highway_mpg))

plot(log(data$highway_mpg), data$msrp) 
plot(log(data$highway_mpg), data$log_msrp) 


###############
#   make
###############
?summarise
brand = data %>% group_by(make) %>%  
  summarise(average = mean(msrp)) %>%
  arrange(desc(round(average, digits=0)))

xtable(brand)

###############
#   year
###############
ggplot(data=data, aes(x=year, y=log_msrp)) +
  geom_point(size=2, shape=23) +
  geom_smooth()




#------------------------------------------------------------------------------#
#                               FEATURES MATRIX                                #
#------------------------------------------------------------------------------#
#data_copy = data
#saveRDS(data, "01 data/data_Carprice_clean.rds")
rm(list = ls())

data = readRDS("01 data/data_Carprice_clean.rds")
str(data)

table(data$number_of_doors)
data$number_of_doors = factor(data$number_of_doors, labels = c("Two", "Three", "Four"))
table(data$number_of_doors)



data = data %>% select(make, year, engine_fuel_type, engine_hp, engine_cylinders,
                       transmission_type,driven_wheels,number_of_doors, vehicle_size,
                       highway_mpg,city_mpg,popularity, msrp,log_msrp)


data_num = data[ , c("year", "engine_hp", "engine_cylinders", 
                     "highway_mpg", "city_mpg", 
                     "popularity", "msrp", "log_msrp")]


# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# Plotting the correlation matrix
pairs(data_num,
      upper.panel = panel.smooth,    # Correlation panel
      lower.panel = panel.cor) # Smoothed regression lines

cr=cor(data_num)
corrplot(cr, type="lower")
corrplot(cr, type="lower", method = "number", tl.col = 'black')
#?corrplot

################## The best graph of cor
## circle + colorful number
corrplot(cr, order = 'AOE', type = 'upper', tl.pos = 'lt', tl.col = 'black')
corrplot(cr, add = TRUE, type = 'lower', method = 'number', order = 'AOE',
         diag = FALSE, tl.pos = 'n', cl.pos = 'n')


###################################
# SPLIT THE DATA SET
###################################

#saveRDS(data, "01 data/data_Carprice_features.rds")
rm(list = ls())
data = readRDS("01 data/data_Carprice_features.rds")

data = data[, -13]
data = data[, -12]

str(data)

set.seed(24)
num_obs = nrow(data)
train_index = sample(num_obs, size = trunc(0.80 * num_obs))
data_train = data[train_index, ]
data_test = data[-train_index, ]


################################################################################
#------------------------------------------------------------------------------#
#                         MODELING - LINEAR REGRESSION                         #
#------------------------------------------------------------------------------#
################################################################################

#attach(data_train)
#detach(data_train)

##### All numerics
str(data_train)

model_00 = lm(log_msrp~1, data=data_train)
summary(model_00)

model_01 = lm(log_msrp ~ .,data=data_train)
summary(model_01)

library(texreg)
texreg(list(model_00, model_01), 
       scalebox = 0.3, 
       caption = "Comparison between Null model vs. Full model",
       caption.above = TRUE, 
       label = "regtab1")

model_02 = lm(log_msrp ~ . -highway_mpg, data=data_train)
summary(model_02)

model_03 = lm(log_msrp ~ . -highway_mpg -engine_fuel_type, data=data_train)
summary(model_03)

model_04 = lm(log_msrp ~ . -highway_mpg -engine_fuel_type -driven_wheels, data=data_train)
summary(model_04)

model_05 = lm(log_msrp ~ . -highway_mpg -engine_fuel_type -driven_wheels - number_of_doors, data=data_train)
summary(model_05)

model_06 = lm(log_msrp ~ . -highway_mpg -engine_fuel_type -driven_wheels 
              - number_of_doors -engine_cylinders -transmission_type
              - vehicle_size - city_mpg, data=data_train)
summary(model_06)

model_07 = lm(log_msrp ~ make + year + engine_hp + I(year^2), data=data_train)
summary(model_07)

#?texreg
texreg(list(model_02, model_03, model_04, model_05, model_06),
       custom.model.names = c("Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
       scalebox = 0.3, 
       caption = "Backward Analysis",
       caption.above = TRUE, 
       label = "regtab2")

texreg(model_07,
       custom.model.names = "Best Model - Model 7",
       scalebox = 0.5, 
       caption = "Best Model",
       caption.above = TRUE, 
       label = "regtab3")

bestmodel = model_07

#Standard diagnostic plots
par(mfrow=c(2,2))
plot(bestmodel)

#Details of the Fitted Model
fitted(bestmodel)
residuals(bestmodel)

#Checking for Independence of Errors
#durbinWatsonTest(lm4_1)
library(car)
durbinWatsonTest(bestmodel)
        #you don't want to reject

#Checking for Multicollinearity
?vif
# https://www.statology.org/r-aliased-coefficients-in-the-model/
vif(bestmodel)

#Checking for Influential Data Points
cooks.distance(bestmodel)
par(mfrow=c(1,1))
influencePlot(model = bestmodel, scale = 3, main = "Influence Plot")

#The check_model function in the performance function provides atttractive diagnostic plots
library(performance)
library(see)
library(patchwork)

check_model(bestmodel)

######

data_test$y_hat = predict(bestmodel, data_test)

#plot(data_test$log_msrp, data_test$y_hat)

##### PLOT EVALUATION
hist(data_test$y_hat)
hist(data_test$log_msrp)

library(ggplot2)
# Basic scatter plot
ggplot(data_test, aes(x=y_hat, y=log_msrp)) + geom_point() +
  geom_smooth(method="lm")

cor(data_test$log_msrp, data_test$y_hat)

RMSE = function(actual, predicted){sqrt(0.5*sum(actual - predicted)^2)}
MAE = function(actual, predicted) {mean(abs(actual - predicted))}
#Minimun absolute error

MAE(data_test$log_msrp, data_test$y_hat)
# 0.2460209
MAE(exp(data_test$log_msrp), exp(data_test$y_hat))
#9006.566

RMSE(data_test$log_msrp, data_test$y_hat)
#22.75009


mean(model_07$residuals^2)
#0.1259173
#MSE(exp(data_test$log_msrp), exp(data_test$y_hat))

#############################################
data_lm_pred <- data.frame(actual= data_test$log_msrp, predicted=data_test$y_hat)
head(data_lm_pred)

ggplot(data_lm_pred, aes(x=predicted, y= actual)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
#  geom_smooth()+
  labs(x='Predicted Values', y='Actual Values')#, title='Predicted vs. Actual Values')


################################################################################
#------------------------------------------------------------------------------#
#                      MODELING - Regression Tree                          #
#------------------------------------------------------------------------------#
################################################################################

rm(list = ls())
data = readRDS("01 data/data_Carprice_features.rds")

data = data[, -14]

data$make2 = 0
data$make2[data$make=="Aston Martin"] = 1
data$make2[data$make=="Bentley"] = 1
data$make2[data$make=="Bugatti"] = 1
data$make2[data$make=="Ferrari"] = 1
data$make2[data$make=="Lamborghini"] = 1
data$make2[data$make=="Maybach"] = 1
data$make2[data$make=="Maclaren"] = 1
data$make2[data$make=="Rolls-Royce"] = 1
data$make2[data$make=="Spyker"] = 1

table(data$make2)

set.seed(24)
num_obs = nrow(data)
train_index = sample(num_obs, size = trunc(0.80 * num_obs))
data_train = data[train_index, ]
data_test = data[-train_index, ]


# library(fastDummies)
# features_train <- fastDummies::dummy_cols(data_train) #, remove_first_dummy = TRUE
# ##knitr::kable(features)
# features_train = features_train %>% select(-make, -engine_fuel_type, - transmission_type, 
#                                -driven_wheels, -number_of_doors, -vehicle_size)

# x_train = features_train %>% select(-log_msrp)
# y_train = features_train$log_msrp
# base_train <- cbind(x_train,y_train)
# 
# ###### TEST
# features_test <- fastDummies::dummy_cols(data_test) #, remove_first_dummy = TRUE
# #knitr::kable(features)
# features_test = features_test %>% select(-make, -engine_fuel_type, - transmission_type, 
#                                      -driven_wheels, -number_of_doors, -vehicle_size)
# 
# x_test = features_test %>% select(-log_msrp)
# y_test = features_test$log_msrp
# base_test <- cbind(x_test,y_test)
  
library(rpart)
library(rpart.plot)
#base_train <- cbind(x_train,y_train)
#base_test <- cbind(x_test,y_test)

# grow tree 
?rpart

#fit_tree <- rpart(log(msrp) ~ year + engine_hp + I(year^2), data = data_train, method="anova")



fit_tree <- rpart(msrp ~ make2 + year + engine_hp, data = data_train, method="anova")
fit_tree
summary(fit_tree)

rpart.plot(fit_tree, type=3, digit=2, fallen.leaves = TRUE)

#Predict Output 
data_test$tree_predicted= predict(fit_tree, data_test)

data_test_comparison= data_test %>% select(msrp, tree_predicted)
data_test_comparison$tree_predicted = round(data_test_comparison$tree_predicted)
#data_test_comparison$tree_predicted_exp = round(exp(data_test_comparison$tree_predicted))

#ggplot(data_test_comparison, aes(x=tree_predicted_exp, y=msrp)) + geom_point() +
  #geom_smooth(method="lm", se=T)


ggplot(data_test_comparison, aes(x=tree_predicted, y= msrp)) +
  geom_point() +
#  geom_abline(intercept=0, slope=1) +
  #  geom_smooth()+
  labs(x='Predicted Values', y='Actual Values')#, title='Predicted vs. Actual Values')

#cor(data_test_comparison$msrp, data_test_comparison$tree_predicted_exp)
cor(data_test_comparison$msrp, data_test_comparison$tree_predicted)

RMSE = function(actual, predicted){sqrt(0.5*sum(actual - predicted)^2)}
MAE = function(actual, predicted) {mean(abs(actual - predicted))}
#Minimun absolute error

MAE(data_test_comparison$msrp, data_test_comparison$tree_predicted)
#10540.75

RMSE(log(data_test_comparison$msrp), log(data_test_comparison$tree_predicted))
#65.4706
################################################################################
#------------------------------------------------------------------------------#
#                              PRESENTATION                                    #
#------------------------------------------------------------------------------#
################################################################################



######## REFERENCES

# https://www.statology.org/r-aliased-coefficients-in-the-model/

# https://daviddalpiaz.github.io/r4sl/linear-models.html#choosing-a-model

# https://www.pluralsight.com/guides/validating-machine-learning-models-with-r

# https://www.r-bloggers.com/2021/11/how-to-plot-observed-and-predicted-values-in-r/


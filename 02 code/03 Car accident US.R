rm(list = ls())

library(pacman)
p_load(ggplot2, dplyr, tidyverse, tidyr, dplyr, BiocManager, corplot, corrplot, 
       RColorBrewer,stargazer, caret, car)


#------------------------------------------------------------------------------#
#                           DATA LOADING                                       #
#------------------------------------------------------------------------------#

#Read the data
  #the file is heavy so we will work with R native format
#data = read.csv("01 data/US_Accidents_2016to2020.csv", stringsAsFactors = T)
#saveRDS(data, "01 data/US_Accidents_2016to2020.rds")

data = readRDS("01 data/US_Accidents_2016to2020.rds")

names(data)
names(data) = tolower(sub("\\.", "_", names(data)))
names(data) = tolower(sub("\\.", "", names(data)))
names(data)[names(data) == 'humidity_.'] <- 'humidity'
names(data)

str(data)

#View(data)
#head(data)

hist(data$severity)

#------------------------------------------------------------------------------#
#                             DATA UNDERSTANDING                               #
#------------------------------------------------------------------------------#

# 1	ID	This is a unique identifier of the accident record.	No
# 2	Severity	Shows the severity of the accident, a number between 1 and 4, where 1 indicates the least impact on traffic (i.e., short delay as a result of the accident) and 4 indicates a significant impact on traffic (i.e., long delay).	No
# 3	Start_Time	Shows start time of the accident in local time zone.	No
# 4	End_Time	Shows end time of the accident in local time zone. End time here refers to when the impact of accident on traffic flow was dismissed.	No
# 5	Start_Lat	Shows latitude in GPS coordinate of the start point.	No
# 6	Start_Lng	Shows longitude in GPS coordinate of the start point.	No
# 7	End_Lat	Shows latitude in GPS coordinate of the end point.	Yes
# 8	End_Lng	Shows longitude in GPS coordinate of the end point.	Yes
# 9	Distance(mi)	The length of the road extent affected by the accident.	No
# 10	Description	Shows natural language description of the accident.	No
# 11	Number	Shows the street number in address field.	Yes
# 12	Street	Shows the street name in address field.	Yes
# 13	Side	Shows the relative side of the street (Right/Left) in address field.	Yes
# 14	City	Shows the city in address field.	Yes
# 15	County	Shows the county in address field.	Yes
# 16	State	Shows the state in address field.	Yes
# 17	Zipcode	Shows the zipcode in address field.	Yes
# 18	Country	Shows the country in address field.	Yes
# 19	Timezone	Shows timezone based on the location of the accident (eastern, central, etc.).	Yes
# 20	Airport_Code	Denotes an airport-based weather station which is the closest one to location of the accident.	Yes
# 21	Weather_Timestamp	Shows the time-stamp of weather observation record (in local time).	Yes
# 22	Temperature(F)	Shows the temperature (in Fahrenheit).	Yes
# 23	Wind_Chill(F)	Shows the wind chill (in Fahrenheit).	Yes
# 24	Humidity(%)	Shows the humidity (in percentage).	Yes
# 25	Pressure(in)	Shows the air pressure (in inches).	Yes
# 26	Visibility(mi)	Shows visibility (in miles).	Yes
# 27	Wind_Direction	Shows wind direction.	Yes
# 28	Wind_Speed(mph)	Shows wind speed (in miles per hour).	Yes
# 29	Precipitation(in)	Shows precipitation amount in inches, if there is any.	Yes
# 30	Weather_Condition	Shows the weather condition (rain, snow, thunderstorm, fog, etc.)	Yes
# 31	Amenity	A POI annotation which indicates presence of amenity in a nearby location.	No
# 32	Bump	A POI annotation which indicates presence of speed bump or hump in a nearby location.	No
# 33	Crossing	A POI annotation which indicates presence of crossing in a nearby location.	No
# 34	Give_Way	A POI annotation which indicates presence of give_way in a nearby location.	No
# 35	Junction	A POI annotation which indicates presence of junction in a nearby location.	No
# 36	No_Exit	A POI annotation which indicates presence of no_exit in a nearby location.	No
# 37	Railway	A POI annotation which indicates presence of railway in a nearby location.	No
# 38	Roundabout	A POI annotation which indicates presence of roundabout in a nearby location.	No
# 39	Station	A POI annotation which indicates presence of station in a nearby location.	No
# 40	Stop	A POI annotation which indicates presence of stop in a nearby location.	No
# 41	Traffic_Calming	A POI annotation which indicates presence of traffic_calming in a nearby location.	No
# 42	Traffic_Signal	A POI annotation which indicates presence of traffic_signal in a nearby loction.	No
# 43	Turning_Loop	A POI annotation which indicates presence of turning_loop in a nearby location.	No
# 44	Sunrise_Sunset	Shows the period of day (i.e. day or night) based on sunrise/sunset.	Yes
# 45	Civil_Twilight	Shows the period of day (i.e. day or night) based on civil twilight.	Yes
# 46	Nautical_Twilight	Shows the period of day (i.e. day or night) based on nautical twilight.	Yes
# 47	Astronomical_Twilight	Shows the period of day (i.e. day or night) based on astronomical twilight.	Yes

#------------------------------------------------------------------------------#
#                              DATA PREPARATION                                #
#------------------------------------------------------------------------------#
#check NA values
x = data %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.)))) 

library(xtable)
?xtable
xtable(t(x), caption = "Variables with NA values")

#View(data)
data_copy = data

data = data %>% select(-id, -start_lat,-start_lng,-end_lat,-end_lng,
                       -description,-number,-street,-zipcode,-country,
                       -timezone,-airport_code,-weather_timestamp)


x = data %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.)))) 
library(xtable)
xtable(t(x), caption = "Variables with NA values")

data <- na.omit(data)

nrow(data)

1516064 - nrow(data)

str(data)
str(data_copy)
data = data %>% filter(civil_twilight!="")

data$sunrise_sunset = factor(data$sunrise_sunset)
data$civil_twilight  = factor(data$civil_twilight )
data$nautical_twilight = factor(data$nautical_twilight)
data$astronomical_twilight = factor(data$astronomical_twilight)

table(data$sunrise_sunset)
table(data$civil_twilight)
table(data$nautical_twilight)
table(data$astronomical_twilight)

#------------------------------------------------------------------------------#
#                               EDA UNIVARIATE                                 #
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#                               EDA MULTIVARIATE                               #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#                               FEATURES MATRIX                                #
#------------------------------------------------------------------------------#

rm(list = ls())

#saveRDS(data, "01 data/US_Accidents_2016to2020_clean.rds")
data = readRDS("01 data/US_Accidents_2016to2020_clean.rds")
str(data)

data_num = data[,c(4,9,10,11,12,13, 15, 16)]

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
#pairs(data_num,
 #     upper.panel = panel.smooth,    # Correlation panel
  #    lower.panel = panel.cor) # Smoothed regression lines

cr=cor(data_num)
#corrplot(cr, type="lower")
#corrplot(cr, type="lower", method = "number", tl.col = 'black')
#?corrplot

################## The best graph of cor
## circle + colorful number
corrplot(cr, order = 'AOE', type = 'upper', tl.pos = 'lt', tl.col = 'black')
corrplot(cr, add = TRUE, type = 'lower', method = 'number', order = 'AOE',
         diag = FALSE, tl.pos = 'n', cl.pos = 'n')



#data$start_time= as.POSIXct(data$start_time, tz = "GMT")
#data$end_time= as.POSIXct(data$end_time, tz = "GMT")

#data$duration_time = (data$end_time - data$start_time)
data = data[,-c(2,3,6,7)]
data$severity = factor(data$severity)

####################################################
data = data[,-c(3, 10, 13, 15:26, 28,29,30)]
str(data)
####################################################

set.seed(24)
num_obs = nrow(data)

train_index = sample(num_obs, size = trunc(0.80 * num_obs))
data_train = data[train_index, ]
data_test = data[-train_index, ]

################################################################################
#------------------------------------------------------------------------------#
#                         MODELING - Naivy Bayes                         #
#------------------------------------------------------------------------------#
################################################################################

library(e1071)
#?naiveBayes
fit_nb = naiveBayes(severity~., data=data_train) 
fit_nb
summary(fit_nb)
predicted_nb = predict(fit_nb, data_test[,2:12])

#View(cbind(data_test$severity, predicted_nb))
table(data_test$severity, predicted_nb)

library(gmodels)
CrossTable(x = data_test$severity, 
           y= predicted_nb, prop.chisq = FALSE)

CFM = table(data_test$severity, predicted_nb)
CFM
Class_accuracy = sum(diag(CFM)/sum(CFM))
Class_accuracy

predict_2 = predicted_nb
predict_2[predict_2==1] = 2
predict_2[predict_2==3] = 2
predict_2[predict_2==4] = 2
CFM = table(data_test$severity, predict_2)
CFM
Class_accuracy = sum(diag(CFM)/sum(CFM))
Class_accuracy

###past = 0.73
### now = 0.79
### now = 0.80 #just cleaning the repeated variables


################################################################################
#------------------------------------------------------------------------------#
#                              PRESENTATION                                    #
#------------------------------------------------------------------------------#
################################################################################


######## REFERENCES

#   Moosavi, Sobhan, Mohammad Hossein Samavatian, Srinivasan Parthasarathy, and Rajiv Ramnath. “A Countrywide Traffic Accident Dataset.”, 2019.
#   Moosavi, Sobhan, Mohammad Hossein Samavatian, Srinivasan Parthasarathy, Radu Teodorescu, and Rajiv Ramnath. "Accident Risk Prediction based on Heterogeneous Sparse Data: New Dataset and Insights." In proceedings of the 27th ACM SIGSPATIAL International Conference on Advances in Geographic Information Systems, ACM, 2019.

#   https://medium.com/@ODSC/build-a-multi-class-support-vector-machine-in-r-abcdd4b7dab6
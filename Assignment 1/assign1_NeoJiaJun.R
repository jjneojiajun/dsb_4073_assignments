###################
# Assignment 1 Q1 #
###################

# Load Data of Wine 
wine_data <- read.csv('assign1_WineData.csv')

##################################
# EDA (Exploratory Data Analysis)#
##################################

# Dimension of the data
dim(wine_data) # 1000 rows and 12 columns 

# Names of columns of the data
names(wine_data) 
# 'FixedAcidity' (4.6 - 15.9) - In fact, acids impart the sourness or tartness that is a fundamental feature in wine taste. 
# 'VolatileAcidity' (0.12 - 1.33) - Basically the process of wine turning into vinegar. 
# 'CitricAcid' (0.0 - 1.0)- Citric acid is found only in very minute quantities in wine grapes. 
# 'ResidualSugar' (0.9-15.5) - Refers to any natural grape sugars that are leftover after fermentation ceases 
# 'Chlorides'(0.012 - 0.611) - Cholorides Level 
# 'FreeSulphurDioxide' (1.00 - 72.00):  
#  A good winemaker will try to get the highest proportion of free sulphur 
#  to bound that he can. At best this will be about half the amount bound.
# 'TotalSulphurDioxide' (6.00 - 289.00) - 
# 'Density' (0.9901 - 1.0037) - 
# 'pH' (2.74 - 4.01) - pH Level of wine usually from 2.6 - 5.0 
# 'Sulphates' (0.37 - 2.00) - Sulphates Level
# 'Alcohol' (8.4- 14.9) - Alcohol Level 
# 'Quality' (3.00 - 8.00) (Target) 

# Structure of data 
str(wine_data) 

# Summary of Data
summary(wine_data)

# Head of Data (6)
head(wine_data)

# Tail of Data (6)
tail(wine_data)

#########################
# Data Visualization    #
#########################

help(boxplot)

# Boxplot - Fixed Acidity 
boxplot(wine_data$FixedAcidity, horizontal = T, 
        main="Wine Data (Fixed Acidity)", 
        xlab="Fixed Acidity Level")

# Boxplot - Volatile Acidity 
boxplot(wine_data$VolatileAcidity, horizontal = T, 
        main="Volatile Acidity", 
        xlab='Volatile Acidity Level')

# Boxplot - Citric Acid
boxplot(wine_data$CitricAcid, horizontal = T, 
        main="Critic Acid", 
        xlab='Citric Acid Level')

# Boxplot - Residual Sugar 
boxplot(wine_data$ResidualSugar, horizontal = T, main="Residual Sugar", 
        xlab='Residual Sugar Level')

# Boxplot - Chlorides 
boxplot(wine_data$Chlorides, horizontal = T, main="Chlorides", 
        xlab='Chlorides Level')

# Boxplot - FreeSulphurDioxide 
boxplot(wine_data$FreeSulphurDioxide, horizontal = T, main="Free SulphurDioxide", 
        xlab='Free SulphurDioxide Level')

# Boxplot - Total Sulphur Dioxide 
boxplot(wine_data$TotalSulphurDioxide, horizontal = T, main="TotalSulphurDioxide", 
        xlab='Total Sulphur Dioxide Level')

# Boxplot - Density
boxplot(wine_data$Density, horizontal = T, main='Density',
        xlab='Density')

# Boxplot - pH
boxplot(wine_data$pH, horizontal = T, main="Wine Data (PH)", 
        xlab="pH Level")

### Plotting Linear Regression Model For Each Columns
par(mfrow = c(4,3))
for (i in c(1:11)) {
  plot(wine_data[, i], jitter(wine_data[, "Quality"]), xlab = names(wine_data)[i],
       ylab = "Quality", col = "blue", cex = 0.8, cex.lab = 1.3)
  abline(lm(wine_data[, "Quality"] ~ wine_data[ ,i]), lty = 2, lwd = 2)
}
par(mfrow = c(1, 1))

##############################################################
# Learning as of the Linear Regression Model for each chart: 
# As VolatileAcidity Level increase, the quality is expected to decrease
# As Chlorides Level increases, the quality is expected to decrease
# As CitricAcid Level increases, the quality is expected to increase
# As Fixed Acidity Level increases, the quality is expected to slightly increase
# As Density Level increases, the quality is expected to drop slightly 
# Residual Sugar, FreeSulphurDioxide, pH is not a big factor in part to increase quality 
# As TotalSulphurDioxide increases, the quality is expected to drop
# As the Sulphates Level increases, the quality is expected to increase
# As the Alcohol Level increases, the quality is expected to increase 
##############################################################


##########################
# Correlation            #
##########################
library(corrplot)
cor_wine_data <- cor(wine_data)
corrplot.mixed(cor_wine_data)

##############################################################
# Learning: 
# Fixed Acidity has a weak relationship with Quality
# Residual Sugar has a weak relationship with Quality
# FreeSulphurDioxide has a weak relationship with Quality
# Chlorides has a weak relationship with quality
# pH has a weak relationship with quality 
##############################################################

##########################
# Linear Model           #
##########################

lmfit <- lm(Quality ~ ., data=wine_data)
# Summary of the linear model
summary(lmfit)

# Residual Standard Error: 0.6584 on 988 degrees of freedom
# Multiple R-squared:  0.348 && Adjusted R-squared:  0.3407 

#Lowest Significance and Probability Variable
# FixedAcidity 

# Remove Fixed Acidity
lmfit2 <- update(lmfit, ~. - FixedAcidity, data=wine_data)
summary(lmfit2)

# Residual standard error: 0.6581 on 989 degrees of freedom
# Multiple R-squared:  0.3479,	Adjusted R-squared:  0.3413 

#Lowest Significance and Probability Variable
# CriticAcid

# Remove Critic Acid
lmfit3 <- update(lmfit2, ~. - CitricAcid, data=wine_data)
summary(lmfit3)

# Residual standard error: 0.6578 on 990 degrees of freedom
# Multiple R-squared:  0.3479,	Adjusted R-squared:  0.3419 

# Lowest Significance and Probability Variable
# ResidualSugar 

lmfit4 <- update(lmfit3, ~. - ResidualSugar, data=wine_data)
summary(lmfit4)

# Residual standard error: 0.6578 on 991 degrees of freedom
# Multiple R-squared:  0.3472,	Adjusted R-squared:  0.342 

# Lowest Significance and Probability Variable
# Density

lmfit5 <- update(lmfit4, ~. - Density, data=wine_data)
summary(lmfit5)

# Residual standard error: 0.6576 on 992 degrees of freedom
# Multiple R-squared:  0.3469,	Adjusted R-squared:  0.3423

# Lowest Significance and Probability Variable
# FreeSulphurDioxide 

lmfit6 <- update(lmfit5, ~. - FreeSulphurDioxide, data=wine_data)
summary(lmfit6)

# Residual standard error: 0.6586 on 993 degrees of freedom
# Multiple R-squared:  0.3443,	Adjusted R-squared:  0.3403  

# Realize that the Adjusted R square dropped! This mean that we have pretty
# much removed all the values that are really insignificant. 

# Thus we will use lmfit5 

plot(lmfit5)

# Which plot is the most non linear? 
lmfit7 <- update(lmfit5, ~. + I(Sulphates^2), data=wine_data)
summary(lmfit7)

# Residual standard error: 0.6467 on 991 degrees of freedom
# Multiple R-squared:  0.3691,	Adjusted R-squared:  0.364 

lmfit8 <- update(lmfit7, ~. - FreeSulphurDioxide, data=wine_data)
summary(lmfit8)

# Residual standard error: 0.6472 on 992 degrees of freedom
# Multiple R-squared:  0.3675,	Adjusted R-squared:  0.363

lmfit9 <- update(lmfit8, ~. + I(pH^2), data=wine_data)
summary(lmfit9)

# However, not all the values are significant now! 

cd <- cooks.distance(lmfit9)
wine_data.clean <- wine_data[abs(cd) < 4/nrow(wine_data),]
nrow(wine_data.clean)

lmfit10 <- lm(formula(lmfit9), data=wine_data.clean)
summary(lmfit10)

# Residual standard error: 0.5629 on 939 degrees of freedom
# Multiple R-squared:  0.4386,	Adjusted R-squared:  0.4326

###################
# Assignment 1 Q2 #
###################

# Load Data of Cars 
car_data <- read.csv('assign1_CarData.csv')

##################################
# EDA (Exploratory Data Analysis)#
##################################

# Dimensions
dim(car_data) # 300 rows and 6 columns

# Names
names(car_data)

# Structure of the Data
str(car_data)

# Head of the Data
head(car_data)

# Tail of the data 
tail(car_data)

# Summary of the data 
summary(car_data)

# Learnings

########################################################
# Cyclinders - Minimum: 3, Max: 8, Mean: 5.573
# Displacement - Minimum: 107, Max: 455, Mean: 200.8
# HorsePower - Minimum: 46, Max: 230, Mean: 106.3
# Weight - Minimum: 1649, Max: 5140, Mean: 3035
# Acceleration - Min: 8.00, Max: 23.7, Mean: 15.46
# Mpg - Min: 9.00, Max: 46.6, Mean: 21.55 (Target)
#########################################################

#########################
# Data Visualization    #
#########################

par(mfrow = c(4,3))
for (i in c(1:5)) {
  plot(car_data[, i], jitter(car_data[, "mpg"]), xlab = names(car_data)[i],
       ylab = "mpg", col = "blue", cex = 0.8, cex.lab = 1.3)
  abline(lm(car_data[, "mpg"] ~ car_data[ ,i]), lty = 2, lwd = 2)
}
par(mfrow = c(1, 1))

##########################
# Correlation            #
##########################
library(corrplot)
cor_car_data <- cor(car_data)
corrplot.mixed(cor_car_data)

################################################
# Learning:
# Mpg is heavily correlated to cylinders, displacement, horsepower and weight
# Acceleration is not that highly correlated
################################################

##########################
# Linear Model           #
##########################

lmfit <- lm(mpg ~ ., data=car_data)
# Summary of the linear model
summary(lmfit)

# Residual standard error: 4.273 on 294 degrees of freedom
# Multiple R-squared:  0.7166,	Adjusted R-squared:  0.7118


# Least Significantce and Highest Probability of Going 0
# Acceleration

lmfit2 <- update(lmfit, ~. - acceleration, data = car_data)
# Summary of the linear model
summary(lmfit2)

# Residual standard error: 4.266 on 295 degrees of freedom
# Multiple R-squared:  0.7165,	Adjusted R-squared:  0.7127 

# Least Significantce and Highest Probability of Going 0
# Cylinders 

lmfit3 <- update(lmfit2, ~. - cylinders, data = car_data)
summary(lmfit3)

# Residual standard error: 4.261 on 296 degrees of freedom
# Multiple R-squared:  0.7163,	Adjusted R-squared:  0.7134 

# Least Significance and Highest Probability of Going 0
# displacement

lmfit4 <- update(lmfit3, ~. - displacement, data = car_data)
summary(lmfit4)

# Residual standard error: 4.265 on 297 degrees of freedom
# Multiple R-squared:  0.7149,	Adjusted R-squared:  0.7129 

# Now is the decision between having variables that are more significant 
# or having a Adjusted R-squared. Personally, for me, the adjusted R-square
# decrease was only 0.005 which is insignificance compared to getting
# more significance variable and thus i will still use lmfit4

lmfit5 <- update(lmfit4, ~. + I(horsepower^2), data=car_data)
summary(lmfit5)

lmfit6 <- update(lmfit5, ~. + I(weight^2), data=car_data)
summary(lmfit6)

# Combine both
lmfit7 <- update(lmfit6, ~. + horsepower:weight, data=car_data)
summary(lmfit7)

# When we combined them both, we found out that it became insignificant,
# thus i will not use the combined lmfit for my best model

# Plot lmfit6

plot(lmfit6)

cd <- cooks.distance(lmfit6)
car_data.clean <- car_data[abs(cd) < 4/nrow(car_data),]
nrow(car_data.clean)

lmfit7 <- lm(formula(lmfit6), data=car_data.clean)
summary(lmfit7)

# Residual standard error: 3.076 on 275 degrees of freedom
# Multiple R-squared:  0.8291,	Adjusted R-squared:  0.8266
# Best Model! :)
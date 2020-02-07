# Project Name: Bike Rental Prediction

# Clean the environment 
rm(list=ls())

# Set working directory
setwd("C:/Users/Hp/Desktop/Project2")

# Check current working directory
getwd()


# Load the required libraries for analysis of data
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced","C50",
      "dummies", "e1071", "Information", "MASS", "rpart", "gbm", "ROSE",
      'sampling', 'DataCombine', 'inTrees',"scales","psych","gplots")

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

# Load the csv file
bike_rental= read.csv("day.csv",header = T,na.strings = c(""," ",NA))

# Explore the data
#Check the dimensions
dim(bike_rental)


#Rename the variables-
names(bike_rental)[4]  = "year"
names(bike_rental)[5]  = "month"
names(bike_rental)[9]  = "weather"
names(bike_rental)[10] = "temperature"
names(bike_rental)[12] = "humidity"
names(bike_rental)[16] = "count"


#Check top(first) rows of dataset 
head(bike_rental)

#Check bottom(last) rows of dataset 
tail(bike_rental)

#Check structure of dataset
str(bike_rental)

#Check summary of dataset 
summary(bike_rental)

# Variable Identification 
# In this dataset cnt is our target variable and it is continous variable 
str(bike_rental$count) 

# Remove these variables 
# instant variable
# casual and registered variable as count is sum of these two variables
# count = casual + registered 

bike_rental = subset(bike_rental,select=-c(instant,dteday,casual,registered))

# Lets check dimensions of data after removing some variables
dim(bike_rental)

# Make Seperate categorical and numerical variables dataframe 
# Continous Variables 
cnames= c("temperature","atemp","humidity","windspeed","count")

# Categorical varibles-
cat_cnames= c("season","year","month","holiday","weekday","workingday","weather")


# EDA or Data Preprocessing

# Duplicate Values
duplicated(bike_rental)# No duplicates in dataset


# Missing Value anlysis
# Check missing values in dataset
sum(is.na(bike_rental))
# there is no missing values present in this dataset


# Outlier Analysis and treatment
# Lets save copy of dataset before preprocessing
df = bike_rental 
bike_rental = df 

# Lets use boxplot to detect the outliers 
# We use ggplot library to plot boxplot for each numeric variable 
for(i in 1:length(cnames))
{
  assign(paste0("gn",i),ggplot(aes_string(y=(cnames[i]),x = 'count'),
                               data=subset(bike_rental))+
           stat_boxplot(geom = "errorbar",width = 0.5) +
           geom_boxplot(outlier.color = "red",fill="grey",
                        outlier.shape = 18,outlier.size = 1,notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y = cnames[i],x='count')+
           ggtitle(paste("boxplot of count for",cnames[i])))
}

# using library(gridExtra)
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,ncol = 2)


# Loop to remove outliers by capping upperfence and lower fence values
for(i in cnames){
  print(i)
  #Quartiles
  Q1 = quantile(bike_rental[,i],0.25)
  Q3 = quantile(bike_rental[,i],0.75)
  
  #Inter quartile range 
  IQR = Q3-Q1
  
  # Upperfence and Lower fence values 
  UL = Q3 + (1.5*IQR(bike_rental[,i]))
  LL = Q1 - (1.5*IQR(bike_rental[,i]))
  
  # No of outliers and inliers in variables 
  No_outliers = length(bike_rental[bike_rental[,i] > UL,i])
  No_inliers = length(bike_rental[bike_rental[,i] < LL,i])
  
  # Capping with upper and inner fence values 
  bike_rental[bike_rental[,i] > UL,i] = UL
  bike_rental[bike_rental[,i] < LL,i] = LL
  
}

# Lets plot boxplots after removing outiers 
for(i in 1:length(cnames))
{
  assign(paste0("gn",i),ggplot(aes_string(y=(cnames[i]),x = 'count'),
                               data=subset(bike_rental))+
           stat_boxplot(geom = "errorbar",width = 0.5) +
           geom_boxplot(outlier.color = "red",fill="grey",
                        outlier.shape = 18,outlier.size = 1,notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y = cnames[i],x='count')+
           ggtitle(paste("boxplot of count for",cnames[i])))
}

# using library(gridExtra)
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,ncol = 2)


#visualization

# Continous Variables 
cnames= c("temperature","atemp","humidity","windspeed","count")

# Categorical varibles-
cat_cnames= c("season","year","month","holiday","weekday","workingday","weather")

# Univariate Analysis
# Histogram for continuous variables to check  distribution of each variable 
for(i in 1:length(cnames))
{
  assign(paste0("h",i),ggplot(aes_string(x=(cnames[i])),
                              data=subset(bike_rental))+
           geom_histogram(fill="darkslateblue",colour = "black")+geom_density()+
           scale_y_continuous(breaks =pretty_breaks(n=10))+
           scale_x_continuous(breaks = pretty_breaks(n=10))+
           theme_bw()+xlab(cnames[i])+ylab("Frequency")+
           ggtitle(paste("distribution of ",cnames[i])))
}

# using library(gridExtra)
gridExtra::grid.arrange(h1,h2,h3,h4,h5,ncol = 2)

# Bivariate Analysis
# Lets check impact of continous variables on target variable
for(i in 1:length(cnames))
{
  assign(paste0("s",i),ggplot(aes_string(y='count',x = (cnames[i])),
                              data=subset(bike_rental))+
           geom_point(alpha=0.5,color="brown") +
          labs(title = "Scatter Plot of count vs", x = (cnames[i]), y = "count")+
           ggtitle(paste("Scatter Plot of count vs",cnames[i])))
}

# using library(gridExtra)
gridExtra::grid.arrange(s1,s2,s3,s4,s5,ncol = 2)

# count vs temperature(atemp) : as temperature increase Bike rent count also increases 
# count vs humidity : humidity doesnt have any effect on bikerent count
# count vs windspeed : windspeed doesnt have any effect on bikerent count
# count vs count : please ignore this plot as it is our target variable 

options(scipen = 999)
# Let us check impact of categorical variables on count

for(i in 1:length(cat_cnames))
{
  assign(paste0("b",i),ggplot(aes_string(y='count',x = (cat_cnames[i])),
                              data=subset(bike_rental))+
           geom_bar(stat = "identity",fill = "blue") +
           labs(title = "Scatter Plot of count vs", x = (cat_cnames[i]), y = "count")+
           ggtitle(paste("Number of bikes rented with respect to",cat_cnames[i])))+
    theme(axis.text.x = element_text( color="black", size=8))+
    theme(plot.title = element_text(face = "bold"))
}

# using library(gridExtra)
gridExtra::grid.arrange(b1,b2,b3,b4,ncol = 2)
gridExtra::grid.arrange(b5,b6,b7,ncol = 2)

# From barplot we can observe below points 
# Season:Bike rent count is high in season 3(fall) and low in season 1(springer)
aggregate(count ~ season ,sum,data = bike_rental)

# year : Bike rent count is high in year 1 (in 2012)
aggregate(count ~ year ,sum,data = bike_rental)

# month : Bike rent count is high in month of august and low in jan
aggregate(count ~ month,sum,data = bike_rental)

# holiday : Bike rent count is high on holidays ie 0
aggregate(count ~ holiday ,sum,data = bike_rental)

# weekday :From bar plot we can see maximum bikes rented on 5th day and least bikes on day 0.
aggregate(count ~ weekday ,sum,data = bike_rental)

# workingday : Bike rent count is high on working day  ie 1
aggregate(count ~ workingday,sum,data = bike_rental)

# weather : Bike rent count is high on weather 1: ie when the weather is 
# Clear, Few clouds, Partly cloudy, Partly cloudy
aggregate(count ~ weather,sum,data = bike_rental)


# Bikes rented with respect to temp and humidity
ggplot(bike_rental,aes(temperature,count)) + 
  geom_point(aes(color=humidity),alpha=0.5) +
  labs(title = "Bikes rented with respect to variation in temperature and humidity", x = "temperature")+ theme_bw()
# maximum bike rented between temp 0.50 to 0.75 and humidity 0.50 to 0.75 

#Bikes rented with respect to temp and weather
ggplot(bike_rental, aes(x = temperature, y = count))+
  geom_point(aes(color=weather))+
  labs(title = "Bikes rented with respect to temperature and weather", x = "temperature")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()

# maximum bike rented with windspeed and normalized temp between 0.50 to 0.75 and when the weathersite is 1 

# Bikes rented with respect to temp and season
ggplot(bike_rental, aes(x = temperature, y = count))+
  geom_point(aes(color=season))+
  labs(title = "Bikes rented with respect to temperature and season", x = "temperature")+
   theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()
# From figure it is clear that maximum bike count is for season 2 and 3,when the temp between 0.5 to 0.7

# Feature Selection
# Lets save dataset after outlier analysis 
df =  bike_rental
bike_rental = df

# Using corrplot library we do correlation analysis for numeric variables
# Let us derive our correlation matrix 
Correlation_matrix = cor(bike_rental[,cnames])
Correlation_matrix
# By looking at correlation matrix we can say temperature and atemp are highly correlated.

#Lets plot correlation plot using corrgram library 
corrgram(bike_rental[,cnames],order = F,upper.panel = panel.pie,
         text.panel = panel.txt,main="Correlation plot for numeric variables")

# From correlation analysis temp and atemp variables are highly correlated so delete atemp variable 

# Lets find significant categorical variables usig ANOVA test 
# Anova analysis for categorical variable with target numeric variable
for(i in cat_cnames){
  print(i)
  Anova_result= summary(aov(formula = count~ bike_rental[,i],bike_rental))
  print(Anova_result)
}

# From the anova result, we can observe working day,weekday and holiday 
# has p value > 0.05, so delete this variable not consider in model.

# Dimension reduction
bike_rental = subset(bike_rental,select = -c(atemp,holiday,weekday,workingday))


# Lets check dimensions after dimension reduction 
dim(bike_rental)

head(bike_rental)

# Lets check column names after dimension reduction 
names(bike_rental)

# Lets update  continous and categorical variables after dimension reduction
# Continuous variable
cnames= c('temperature','humidity', 'windspeed', 'count')

# Categorical variables
cat_cnames = c('season', 'year', 'month','weather')


# Feature Scaling

# normality  

# Normality check using normal qq plot
for(i in cnames){
  print(i)
  qqplot= qqnorm(bike_rental[,i])
}

# Normality check using histogram plot(we already plotted hist in data understanding)
gridExtra::grid.arrange(h1,h2,h3,h4,h5,ncol = 2)

#check summary of continuous variable to check the scaling- 
for(i in cnames){
  print(i)
  print(summary(bike_rental[,i]))
}

# From normal qq plot,histplot and by looking at summary of 
# numeric variables we can say data is normally distributed


# Model Development
# Let's clean R Environment, as it uses RAM which is limited
library(DataCombine)
rmExcept("bike_rental")

# Lets convert all categorical variables ito dummy variables 
# As we cant pass categorical variables directly in to regression problems
# Lets save our preprocessed data into df data set 
df = bike_rental
bike_rental = df

# Lets call Categorical varaibles after feature selection using ANOVA 
cat_cnames= c("season","year","month","weather")

# lets create dummy variables using dummies library
library(dummies)
bike_rental = dummy.data.frame(bike_rental,cat_cnames)
dim(bike_rental)
head(bike_rental)
# we can see dummy variables are created in Bike rent dataset 

# Divide data into train and test sets
set.seed(1234)
train.index = createDataPartition(bike_rental$count, p = .80, list = FALSE)
train = bike_rental[ train.index,]
test  = bike_rental[-train.index,]

# Function for Error metrics to calculate the performance of model
mape= function(y,yhat){
  mean(abs((y-yhat)/y))*100
}

# Function for r2 to calculate the goodness of fit of model
rsquare=function(y,yhat){
  cor(y,yhat)^2
}

# Function for RMSE value 
rmse = function(y,yhat){
  difference = y - yhat
  root_mean_square = sqrt(mean(difference^2))
  print(root_mean_square)
}


# Desicision Tree

# Lets Build decision tree model on train data using rpart library 
DT_model= rpart(count~.,train,method = "anova")
DT_model

# Lets plot the decision tree model using rpart.plot library 
library(rpart.plot)	
rpart.plot(DT_model,type=4,digits=3,fallen.leaves=T,tweak = 2)

# Prediction on train data
DT_train= predict(DT_model,train[-25])

# Prediction on test data
DT_test= predict(DT_model,test[-25])

# MAPE For train data
DT_MAPE_Train = mape(train[,25],DT_train)#52.7985

# MAPE For train data test data
DT_MAPE_Test = mape(test[,25],DT_test)#19.9308

# Rsquare  For train data
DT_r2_train= rsquare(train[,25],DT_train)

# Rsquare For test data       
DT_r2_test = rsquare(test[,25],DT_test)

# rmse For train data
DT_rmse_train = rmse(train[,25],DT_train)

# rmse For test data
DT_rmse_test = rmse(test[,25],DT_test)

# Random Forest

# Lets Build random forest model on train data using randomForest library 
RF_model= randomForest(count~.,train,ntree=100,method="anova")

# Prediction on train data
RF_train= predict(RF_model,train[-25])

# Prediction on test data
RF_test = predict(RF_model,test[-25])

# MAPE For train data
RF_MAPE_Train = mape(train[,25],RF_train)

# MAPE For test data
RF_MAPE_Test = mape(test[,25],RF_test)

# Rsquare  For train data
RF_r2_train=rsquare(train[,25],RF_train)

# Rsquare For test data       
RF_r2_test=rsquare(test[,25],RF_test)

# rmse For train data
RF_rmse_train = rmse(train[,25],RF_train)

# rmse For test data
RF_rmse_test = rmse(test[,25],RF_test)


# Random Forest
install.packages("randomForest")
library(randomForest)

# Lets Build random forest model on train data using randomForest library 
RF_model= randomForest(count~.,train,importance=TRUE , ntree=100)

# Prediction on train data
RF_train= predict(RF_model,train[-25])

# Prediction on test data
RF_test = predict(RF_model,test[-25])

# MAPE For train data
RF_MAPE_Train = mape(train[,25],RF_train)

# MAPE For test data
RF_MAPE_Test = mape(test[,25],RF_test)

# Rsquare  For train data
RF_r2_train=rsquare(train[,25],RF_train)

# Rsquare For test data       
RF_r2_test=rsquare(test[,25],RF_test)

# rmse For train data
RF_rmse_train = rmse(train[,25],RF_train)

# rmse For test data
RF_rmse_test = rmse(test[,25],RF_test)


# Linear Regression model

# Before building multiple linear regression model lets check the 
# vif for multicolinearity
# continous variables after feature selection using correlation analysis 
cnames= c("temperature","humidity","windspeed")
numeric_data= bike_rental[,cnames]

# VIF test  using usdm library
library(usdm)
vifcor(numeric_data,th=0.6)
# No variable from the 3 input variables has collinearity problem.

# Lets build multiple linear regression model on train data 
# we will use the lm() function in the stats package
LR_Model = lm(count ~.,data = train)

# Check summary
summary(LR_Model)

# Lets check the assumptins of ols regression 
# 1) Error should follow normal distribution - Normal qqplot
# 2) No heteroscadacity - Residual plot
par(mfrow = c(1, 1))# Change the panel layout to 1 x 1
plot(LR_Model)
# 3) No multicolinearity between Independent variables 
# 4) No autocorrelation between errors
library(car)
dwt(LR_Model)
# All Asumptions of regression are satisfied

# Lets predict on train data 
LR_train = predict(LR_Model,train[,-25])
# Now Lets predict on test data 
LR_test= predict(LR_Model,test[-25])

# Lets check performance of model
# MAPE For train data
LR_MAPE_Train =mape(train[,25],LR_train)
# MAPE For test data
LR_MAPE_Test=mape(test[,25],LR_test)

# Rsquare For train data
LR_r2_train=rsquare(train[,25],LR_train)
# Rsquare For test data
LR_r2_test=rsquare(test[,25],LR_test)

# rmse For train data
LR_rmse_train = rmse(train[,25],LR_train)
# rmse For test data
LR_rmse_test = rmse(test[,25],LR_test)




# Gradient Boosting
library(gbm)

# Lets build a Gradient Boosting model for regression problem
GB_model = gbm(count~., data = train,distribution = "gaussian", n.trees = 100, interaction.depth = 2)

# Model Prediction on train data
GB_train = predict(GB_model, train[-25], n.trees = 100)

# Model Prediction on test data
GB_test = predict(GB_model, test[-25], n.trees = 100)

# Mape for train data
GB_MAPE_Train=mape(train[,25],GB_train)

# Mape for test data
GB_MAPE_Test=mape(test[,25],GB_test)

# Rsqure for train data
GB_r2_train=rsquare(train[,25],GB_train)

# Rsquare for test data
GB_r2_test=rsquare(test[,25],GB_test)

# rmse For train data
GB_rmse_train = rmse(train[,25],GB_train)

# rmse For test data
GB_rmse_test = rmse(test[,25],GB_test)




# Results

Model = c('Decision Tree for Regression','Random Forest','Linear Regression','Gradient Boosting')
          
MAPE_Train = c(DT_MAPE_Train,RF_MAPE_Train,LR_MAPE_Train,
               GB_MAPE_Train)

MAPE_Test = c(DT_MAPE_Test,RF_MAPE_Test,LR_MAPE_Test,
              GB_MAPE_Test)

Rsquare_Train = c(DT_r2_train,RF_r2_train,
                  LR_r2_train,GB_r2_train)

Rsquare_Test = c(DT_r2_test,RF_r2_test,
                 LR_r2_test,GB_r2_test)
              

Rmse_Train = c(DT_rmse_train,RF_rmse_train,
               LR_rmse_train,GB_rmse_train)
              

Rmse_Test = c(DT_rmse_test, RF_rmse_test,
             LR_rmse_test,GB_rmse_test)
              

Final_results = data.frame(Model,MAPE_Train,MAPE_Test,Rsquare_Train,
                           Rsquare_Test,Rmse_Train,Rmse_Test)

Final_results

# From above results Random Forest model have optimum values and this algorithm is good for our data 

# Lets save the out put of finalized model (RF)
Pred_count_RF_test = predict(RF_model,test[-25])

# Exporting the output to hard disk for further use
test <- as.data.frame(cbind(test,Pred_count_RF_test))

Final_output <- as.data.frame(cbind(test$count,Pred_count_RF_test))

names(Final_output)[1] <- "bike_rental_Count"

write.csv(Final_output,"C:/Users/Hp/Desktop/Project2/RF_output_R.csv", row.names = FALSE)

###########################################################################################
####################################END MODEL##############################################
###########################################################################################


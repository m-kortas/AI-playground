library(caroline)
library(devtools)
library(openxlsx)
library(XML)
library(RCurl)
library(e1071)

library(ROCR)
library(text2vec)
library(data.table)
library(magrittr)
library(glmnet)


# Is there a relationship between the daily minimum and maximum temperature? 
# Can you predict the maximum temperature given the minimum temperature? 

weather <- read.csv("~/R-Projects/linear_regressions/weather.csv", header=TRUE)

head(weather)
str(weather)

weather2 <- weather[c("MinTemp", "MaxTemp")]
mapply(anyNA,weather2)
weather2 <- na.omit(weather2)

#####

scatter.smooth(x=weather2$MaxTemp, y=weather2$MinTemp, main="Max & Min") 
abline(lm(weather2$MinTemp~ weather2$MaxTemp), col="blue", lwd=3)

cor(weather2$MinTemp, weather2$MaxTemp) # 0.87 wysoka korelacja

par(mfrow=c(2,2))
boxplot(weather2$MinTemp, main="min") #  dużo outlayerów poniżej
boxplot(weather2$MaxTemp, main="max") #  dużo outlayerów poniżej i powyżej 


par(mfrow=c(2,2))
plot(density(weather2$MinTemp), main="Min", ylab="Częstotliwość",
     sub=paste("Skośność:", round(e1071::skewness(mtcars$mpg), 1)))  # duża skośność
plot(density(weather2$MaxTemp), main="Max", ylab="Częstotliwość",
     sub=paste("Skośność:", round(e1071::skewness(mtcars$cyl), 1)))  # duża skośność


######

dim(weather2)

setDT(weather2)  # do data table

set.seed(9999)  # randomowe
weather2$id <- seq.int(nrow(weather2))  #dodanie id
setkey(weather2, id)

weather_id = weather2$id
mapply(anyNA,weather2)
train_id = sample(weather_id, 83000)
test_id = setdiff(weather_id, train_id)
train = weather2[J(train_id)]
test = weather2[J(test_id)]

# Is there a relationship between the daily minimum and maximum temperature? YES
Y = train$MinTemp
X = train$MaxTemp
model1<- lm(Y~X)
model1
summary(model1) #Multiple R-squared:  0.7724
# Can you predict the maximum temperature given the minimum temperature? YES  

preds1 <- predict(model1, data.frame("Y"= test$MinTemp))
final_data <- cbind(test, preds1)

# policzyc MSE 
library(Metrics)  

mse(final_data$MaxTemp, final_data$preds1) ### 217.2219

plot(final_data$MaxTemp, final_data$preds1)
abline(model1, col="blue", lwd=3)

### dataset

weather3 <- weather[c("STA", "MinTemp", "MaxTemp")]

library(tidyverse)
rozne <- weather3 %>%
  group_by(STA) %>%
  summarise(sred_amplit = mean(MaxTemp-MinTemp), 
            med_aplit = median(MaxTemp-MinTemp), liczba = n()) 
rozne
# duża różnorodność geograficzna
# średnie amplitudy temperatur od 3,95 (salinas, ewador) do 17 (luxor, egypt) stopni


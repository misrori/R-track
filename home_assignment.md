---
title: "Home Assignment"
author: "Orsos_ Mihaly"
date: "2016. február 10."
output: html_document
---

```{r, message=FALSE, warning=FALSE}
library(nycflights13)
library(ggplot2)
library(data.table)
library(pander)
library(h2o)
library(randomForest)

```

#Exploratory data analyzis

Converting the data into data table
```{r}
d_airlines <- data.table(airlines)
d_airports <- data.table(airports)
d_flights <- data.table(flights)
d_planes <- data.table(planes)
d_weather <- data.table(weather)

str(d_weather)

str(flights)

```

##Airlines
```{r}
str(d_airlines)

```
##Airports
```{r}
str(d_airports)
#generating the altitude in meter
d_airports[, altitude_in_meter:= alt/3.280839]
```

```{r}
  ggplot(d_airports, aes(altitude_in_meter))+ geom_histogram(binwidth = 50)+ ggtitle("Histogram of altitude")+ xlab("meter")+ theme_bw()
```
##Flights
```{r}
str(d_flights)
```
###Number of flights by origin  
```{r}
pander(d_flights[, list(Number_of_flights_by_origin=.N), by=origin])
```

###Number of flights by destination 
```{r}
dt3<- d_flights[, list(Number_of_flights_by_origin=.N), by=dest]
setorder(dt3,-Number_of_flights_by_origin )
pander(head(dt3, 20))
```

###Number of flights by date
```{r}
#by month
flights_by_month <- d_flights[, list(Number_of_flights= .N), by= c("year", "month")]
setorder(flights_by_month, month)

ggplot(flights_by_month, aes(x=as.factor(month), y=Number_of_flights ))+ geom_bar(stat = "identity")+ ggtitle("Number of flights by month")+ xlab("Month")+ ylab("Number of flights")+ theme_gray()

```
###Departure and arrival delays
```{r}
pander(summary(d_flights$dep_delay))
ggplot(d_flights, aes(dep_delay))+ geom_histogram(binwidth = 10)+ ggtitle("Histogram of departure delay")+ xlab("Delay")+ theme_bw()

pander(summary(d_flights$arr_delay))
ggplot(d_flights, aes(arr_delay))+ geom_histogram(binwidth = 10)+ ggtitle("Histogram of arrival delay")+ xlab("Delay")+ theme_bw()

```
### The longest travel
```{r}

pander(summary(d_flights$air_time))
ggplot(d_flights, aes(air_time))+ geom_histogram(binwidth = 10)+ ggtitle("Histogram of time spent in the air")+ xlab("Minutes")+ theme_bw()
pander(d_flights[which.max(air_time)])


dt1 <- d_flights[air_time>500, list(origin, dest)]
dt1[, list(number_of_origin = .N), by= origin]
dt1[, list(number_of_origin = .N), by= dest]

dt2 <- d_flights[origin=="EWR"|origin=="JFK"][dest=="HNL", list(Average_flight_time=mean(arr_time, na.rm = T)), by= origin]

```
In this section I found out the longest travels in this dataset are flown from JFK, or EWR to HNL (Honolulu International Airport). 

```{r}
pander(dt2)
```

```{r}
pander(summary(d_flights$distance))
ggplot(d_flights, aes(distance))+ geom_histogram(binwidth = 10)+ ggtitle("Histogram of flown distance")+ xlab("Distance")+ theme_bw()

d_flights[distance>4500,]

```
It turns out the distance from JFK- HNL is 4983, and from EWR- HNL is 4963
##Planes
```{r}
pander(str(d_planes))

```
###Number of plane by features
```{r}
for (var in c("year", "type", "manufacturer", "seats", "engine")) {
  dtemp <- d_planes[, list(Number_of_plane=.N), by= var]
  setorder(dtemp, -Number_of_plane)
  pander(head(dtemp, 20))
}

```
##Weather
```{r}
str(d_weather)
pander(summary(d_weather))
```
Averiging the weather feature by month
```{r}
#deleting rows with NA
d_weather <- d_weather[complete.cases(d_weather),]
d_weather[,temp_celsius := ((temp-32)/1.8)]
dt7<- d_weather[, list(average_celsius= mean(temp_celsius)), by= month] 
pander(dt7)

```

```{r}
dt8<- d_weather[, list(average_daily_celsius= mean(temp_celsius),average_windspeeg = mean(wind_speed), average_wind_dir= mean(wind_dir),visibility = mean(visib), hum= mean(humid), presur= mean(pressure)), by= c("month","day")] 
setkey(d_weather, month, day)
setkey(dt8, month, day)
d_weather <- d_weather[dt8]

```
Warmer and the coldest hour in EWR
```{r}
pander(d_weather[origin=="EWR"][which.max(temp_celsius)])
pander(d_weather[origin=="EWR"][which.min(temp_celsius)])
```


```{r}
d_weather[, full_date:= paste(year,month, day, sep="-")]
d_weather$full_date <- as.Date(d_weather$full_date)
ggplot(d_weather, aes(x=full_date, y= average_daily_celsius ))+ geom_line()+ ggtitle("Tempreature in EWR in 2013")+ xlab("Date")+ ylab("Average daily temperature")+ theme_bw()
```

##Creating a bigger data to analysis
```{r}
setkey(d_flights, carrier)
setkey(d_airlines, carrier)
airlines_data <-d_flights[d_airlines]

setkey(airlines_data, tailnum)
setkey(d_planes, tailnum)

airlines_data <- airlines_data[d_planes]

setkey(airlines_data, month, day)
setkey(dt8, month, day)

airlines_data <- airlines_data[dt8]

#write.table(airlines_data, "airlines_data.R", sep = ",")

```

#Selecting the relevant columns
```{r}
del <- c(1,2,3,9,10,15,16,24)

airlines_data <- airlines_data[, -del, with=F]
airlines_data <- airlines_data[complete.cases(airlines_data$arr_delay),]

```



#Model predicting a flight will be late by 15+ minutes at the destination


##Preparing the data
```{r}
airlines_data$late_15min <- 0
airlines_data[arr_delay>15]$late_15min =1
#del2 <- c(4:6,11:13,16)
del2<- c(4:7,10:14,17)
airlines_data <- airlines_data[, -del2, with=F]

set.seed(123)
N <- nrow(airlines_data)
idx_train <- sample(1:N,N/2)
idx_valid <- sample(base::setdiff(1:N, idx_train), N/4)
idx_test <- base::setdiff(base::setdiff(1:N, idx_train),idx_valid)
d_train <- airlines_data[idx_train,]
d_valid <- airlines_data[idx_valid,]
d_test  <- airlines_data[idx_test,]
```

```{r, message=FALSE, warning=FALSE}
h2o.init(max_mem_size = "2g", nthreads = -1)
dx_train <- as.h2o(d_train) 
dx_train$late_15min <- as.factor(dx_train$late_15min)
dx_valid <- as.h2o(d_valid)
dx_valid$late_15min <- as.factor(dx_valid$late_15min)
dx_test <- as.h2o(d_test)
dx_test$late_15min <- as.factor(dx_test$late_15min)

```

##Random forest

```{r echo=FALSE, message=FALSE, include=FALSE, cache=FALSE}
rf_md<- h2o.randomForest(x = 1:13, y = 14, 
            training_frame = dx_train, 
            mtries = -1, ntrees = 200, max_depth = 50, nbins = 200) 
```

```{r}
#rf_md<- h2o.randomForest(x = 1:13, y = 14, 
#            training_frame = dx_train, 
#            mtries = -1, ntrees = 200, max_depth = 50, nbins = 200) 
rf_md
plot(rf_md)
h2o.auc(rf_md) 
rf<- h2o.performance(rf_md, dx_test)
h2o.auc(rf)
plot(rf, type = "roc", col = "blue", typ = "b")

```

#GBM with cross validation
```{r echo=FALSE, message=FALSE, include=FALSE, cache=FALSE}
gbm_md<- h2o.gbm(x = 1:13, y = 14, 
                 training_frame = dx_train,
                 max_depth = 1000, ntrees = 200, learn_rate = 1, nbins = 200, 
                 nfolds = 3,
                 stopping_rounds = 3, stopping_tolerance = 1e-3) 

```

```{r}
#gbm_md<- h2o.gbm(x = 1:13, y = 14, 
#                 training_frame = dx_train,
#                 max_depth = 1000, ntrees = 200, learn_rate = 1, nbins = 200, 
#                 nfolds = 3,
#                 stopping_rounds = 3, stopping_tolerance = 1e-3) 



gbm_md
h2o.auc(gbm_md)
gbm <- (h2o.performance(gbm_md, dx_test))
h2o.auc(gbm)
plot(gbm, type = "roc", col = "blue", typ = "b")
```
##Neural network

```{r echo=FALSE, message=FALSE, include=FALSE, cache=FALSE}
NN_md<- h2o.deeplearning(x = 1:13, y = 14, 
                         training_frame = dx_train, validation_frame = dx_valid,
                         activation = "Rectifier", hidden = c(200,200), epochs = 100,
                        stopping_rounds = 3, stopping_tolerance = 0)
``` 

```{r}
#NN_md<- h2o.deeplearning(x = 1:13, y = 14, 
#                         training_frame = dx_train, validation_frame = dx_valid,
#                         activation = "Rectifier", hidden = c(200,200), epochs = 100,
#                        stopping_rounds = 3, stopping_tolerance = 0)

NN_md
h2o.auc(NN_md) 
NN <- (h2o.performance(NN_md, dx_test))
h2o.auc(NN)
plot(NN, type = "roc", col = "blue", typ = "b")

```

##Results
```{r echo=FALSE, message=FALSE, include=FALSE, cache=FALSE}
resulttable <- data.frame(Model = c("Randomforest training", "Randomforest test", "GBM with cross validation training","GBM with cross validation test", "Neural network training", "Neural network test" ),
  MSE= c(h2o.auc(rf_md),h2o.auc(rf),h2o.auc(gbm_md),h2o.auc(gbm),h2o.auc(NN_md),h2o.auc(NN)) , 
  Time= c(rf_md@model$run_time,"Not measured", gbm_md@model$run_time,"Not measured",NN_md@model$run_time,"Not measured" ))

```

```{r} 
resulttable

```





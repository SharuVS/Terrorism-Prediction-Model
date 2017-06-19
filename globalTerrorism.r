library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(forecast)
library(tseries)
library(knitr)
library(rworldmap)

# load and subset the dataset
df1 <- read.csv("GlobalTerrorism.csv")
df1 <- subset(df1, select = c(iyear, country_txt))

# create time series
df <- count(df1, iyear)
df <- filter(df, iyear >=1994)
df <- zoo(df$n, order.by = df$iyear)

# Predictions 2014-2015 and Evaluation metrics
AR <- accuracy(f = predict(ar(df[1:20]), n.ahead = 2)$pred, x = df[21:22])
MA <- accuracy(f = predict(ma(df[1:20], order = 2), h = 2)$mean, x = df[21:22])
ARIMA <- accuracy(f = predict(auto.arima(df[1:20]), 
                              n.ahead = 2)$pred, x = df[21:22])
ETS <- accuracy(f = predict(ets(df[1:20])$fitted, h = 2)$mean, 
                x = df[21:22])
metrics  <- rbind(as.data.frame(AR, row.names = "AR"),
                  as.data.frame(MA, row.names = "MA"),
                  as.data.frame(ARIMA, row.names = "ARIMA"),
                  as.data.frame(ETS, row.names = "ETS"))

#### Table "Evaluation metrics" ####
kable(metrics)

### Selection of the most accurate model


# Selection of the most accurate function
index <- c("AR" = min(abs(AR)), 
           "MA" = min(abs(MA)), 
           "ARIMA" = min(abs(ARIMA)), 
           "ETS" = min(abs(ETS)))
index <- names(index[index == min(index)])

# Fitted and Predicted values
AR <- ar(df)$resid + as.vector(df)
MA <- ma(df, order = 2)
ARIMA <- auto.arima(df, stationary = F, seasonal = F)$resid + as.vector(df)
ETS <- ets(df)$fitted
pred_vls <- data.frame("AR" = c(AR, predict(ar(df), n.ahead = 2)$pred),
                       "MA" = c(MA, predict(ma(df, order = 2), h = 2)$mean),
                       "ARIMA" = c(ARIMA, predict(auto.arima(df, stationary = F, 
                                                             seasonal = F), 
                                                  n.ahead = 2)$pred),
                       "ETS" = c(ETS, 
                                 predict(ets(df)$fitted, h = 2)$mean),
                       "Real data" = c(as.vector(df), NA, NA),
                       "Year" = c(1994:2017))

# Predicted future terrorist activity
pred_vls <- gather(pred_vls, method, value, - Year)
pred_vls <- filter(pred_vls, method == index | method == "Real.data")
pred_vls$method <- as.factor(pred_vls$method)
levels(pred_vls$method) <- c("ARIMA", "Real data")


#### Plot "Predicted terrorist activity in the world" ####
ggplot(pred_vls, 
aes(x = Year, y = value, colour = method))+
geom_point(size = 2)+
geom_line(size = 1)+
ggtitle("Predicted terrorist activity in the world")+
labs(x = "Year",
y = "Terrorist attacks",
colour = " ")+
scale_x_continuous(breaks = c(1994:2018))+
theme(legend.position = "right",
legend.text = element_text(size = 14),
axis.text.x = element_text(size = 14, angle = 90), 
axis.title.x = element_text(size = 14),
axis.text.y = element_text(size = 14), 
axis.title.y = element_text(size = 14),
title = element_text(size = 16))


#### Predicted future terrorist activity by countries ####

fc <- data.frame("country" = character(), 
"2016" = integer(), 
"2017" = integer(), 
"method" = integer())

for(i in levels(df1$country_txt)){
  country <- filter(df1, country_txt == i)

  # Create time series
  country <- count(country, iyear)
  country <- filter(country, iyear >=1994)

  # If the length of time series is not enough for prediction, 
  # skip this country
  if(length(country$iyear) < 5){
    next
  }

  # Add years with zero values
  iyear <- c(1994:2015)[!c(1994:2015) %in% country$iyear]
  if(length(iyear) != 0){
    country <- rbind(country, data.frame("iyear" = iyear,
    "n" = 0))
  }

  # Create time series
  country <- zoo(country$n, order.by = country$iyear)

  # Predictions 2014-2015 and Evaluation metrics
  AR <- accuracy(f = predict(ar(country[1:20], aic = F), n.ahead = 2)$pred, x = country[21:22])
  AR <- replace(AR, is.infinite(AR), NA)
  MA <- accuracy(f = predict(ma(country[1:20], order = 2), h = 2)$mean, x = country[21:22])
  MA <- replace(MA, is.infinite(MA), NA)
  ARIMA <- accuracy(f = predict(auto.arima(country[1:20], 
  stationary = F, seasonal = F, 
  allowdrift = F), 
  n.ahead = 2)$pred, x = country[21:22])
  ARIMA <- replace(ARIMA, is.infinite(ARIMA), NA)
  ETS <- accuracy(f = predict(ets(country[1:20])$fitted, h = 2)$mean, 
  x = country[21:22])
  ETS <- replace(ETS, is.infinite(ETS), NA)

  # Selection of the most accurate function
  index <- c("AR" = min(abs(AR), na.rm = T), 
  "MA" = min(abs(MA), na.rm = T), 
  "ARIMA" = min(abs(ARIMA), na.rm = T), 
  "ETS" = min(abs(ETS), na.rm = T))
  index <- names(index[index == min(index, na.rm = T)])

  # Fitted and Predicted values
  AR <- ar(country)$resid + as.vector(country)
  MA <- ma(country, order = 2)
  ARIMA <- auto.arima(country, 
  stationary = F, seasonal = F, allowdrift = F)$resid 
  + as.vector(country)
  ETS <- ets(country)$fitted
  pred_vls <- data.frame("AR" = c(AR, predict(ar(country), n.ahead = 2)$pred),
  "MA" = c(MA, predict(ma(country, order = 2), h = 2)$mean),
  "ARIMA" = c(ARIMA, predict(auto.arima(country, 
  stationary = F, 
  seasonal = F, 
  allowdrift = F), 
  n.ahead = 2)$pred),
  "ETS" = c(ETS, 
  predict(ets(country)$fitted, h = 2)$mean),
  "Real data" = c(as.vector(country), NA, NA),
  "Year" = c(1994:2017))

  # Predicted future terrorist activity by countries
  pred_vls <- gather(pred_vls, method, value, - Year)
  pred_vls <- filter(pred_vls, method == index)
  fc2 <- data.frame("country" = i, 
  "X2016" = pred_vls$value[pred_vls$Year == 2016],
  "X2017" = pred_vls$value[pred_vls$Year == 2017],
  "method" = index)
  fc <- rbind(fc, fc2)
  rm(fc2)
}

# Real data by countries
rc <- data.frame("country" = character(), 
"2015" = integer())

for(i in levels(df1$country_txt)){
  country <- filter(df1, country_txt == i, iyear == 2015)
  country <- count(country, iyear)
  if(length(country$n) == 0){
    rc <- rbind(rc, data.frame("country" = i, "X2015" = 0))
  }
  else{
    rc <- rbind(rc, data.frame("country" = i, "X2015" = country$n))
  }
}

# Join Fitted-Predicted dataset and Real data
fc <- inner_join(fc, rc)
fc <- fc[c("country", "X2015", "X2016", "X2017", "method")]
fc$X2016 <- round(fc$X2016, digits = 0)
fc$X2017 <- round(fc$X2017, digits = 0)
fc <- arrange(fc, desc(X2017))
colnames(fc) <- c("Country", "2015", "2016", "2017", "Method")
fc$`2016`[fc$`2016` < 0] <- 0
fc$`2017`[fc$`2017` < 0] <- 0


### Top-10 countries by predicted terrorist attacks in 2017 year

#### Table "Top-10 countries by predicted terrorist attacks in 2017" ####
kable(fc[1:10,])

#### Plot "The most frequency predictive methods" ####
method <- data.frame(summary(fc$Method))
method$Method <- row.names(method)
method$Percents <- method$summary.fc.Method./sum(method$summary.fc.Method.)*100
method$Percents <- round(method$Percents, digits = 0)
method <- arrange(method, desc(Percents))

ggplot(method, aes(x = "", y = Percents, fill = reorder(Method, Percents)))+
geom_bar(width = 1, stat = "identity")+
coord_polar("y", start = 0)+
geom_text(aes(y = c(15, 40, 65, 90), label = paste0(Percents,"%")), 
size = 6)+
ggtitle("Frequency of predictive methods")+
labs(x = " ",
y = " ",
fill = " ")+
theme(legend.position = "right",
legend.text = element_text(size = 14),
axis.text.x = element_text(size = 14), 
axis.title.x = element_text(size = 14),
axis.text.y = element_text(size = 14), 
axis.title.y = element_text(size = 14),
title = element_text(size = 16))

###  The map of terrorist activity by countries in 2017 year
#### Map "Predicted terrorist activity in 2017" ####

GEO <- joinCountryData2Map(fc, joinCode = "NAME", nameJoinColumn = "Country")
mapCountryData(GEO, nameColumnToPlot = "2017", catMethod = "pretty",
missingCountryCol = "lightblue", nameColumnToHatch = T,
mapTitle = "Predicted terrorist activity in 2017 year")

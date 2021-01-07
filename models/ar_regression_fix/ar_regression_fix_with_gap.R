# Load libraries
library (zoo)
library(forecast)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(readr)
library("readxl")
library(dplyr)
library(tidyverse)
library(scales)
library(caret)
library(e1071)


# set the encoding, if not set then reading csv will cause errors
Sys.setlocale(category='LC_ALL', locale='C')

# making time index
full_index <- seq(from = as.POSIXct("2020-01-31 16:00"), to = as.POSIXct("2020-05-31 16:00"), by="hour")
train_index <- seq(from = as.POSIXct("2020-01-31 16:00"), to = as.POSIXct("2020-05-03 16:00"), by="hour")
valid_index <- seq(from = as.POSIXct("2020-05-03 17:00"), to = as.POSIXct("2020-05-31 16:00"), by="hour")
for_error_index <- seq(from = as.POSIXct("2020-02-07 17:00"), to = as.POSIXct("2020-05-31 16:00"), by="hour")

districts <- c("Neihu.xlsx", "Nangang.xlsx", "Songshan.xlsx", "Datong.xlsx",
               "Wenshan.xlsx", "Daan.xlsx", "Beitou.xlsx", "Zhongshan.xlsx",
               "Zhongzheng.xlsx", "Shilin.xlsx", "Xinyi.xlsx", "Wanhua.xlsx")

#### 12 districts AR (second layer)
for(district in districts) {
  district.name <- gsub(".xlsx", "", district)
  print(district.name)
  
  # read data into a dataframe
  district.df <- read_excel(district)
  
  # Add pos_time column
  district.df$POS_TIME <- as.POSIXct(district.df$POS_TIME, format = "%Y/%m/%d %H")
  
  # Define msts object
  nTotal <- length(district.df$rent_nums) # same in three time-series
  district.ts <- ts(district.df$rent_nums)
  dummy.ts <- ts(district.df$dummy)

  # setting for roll-forward
  fixed.nValid <- 24*35 
  dt_fixed.nTrain <- length(district.ts) - fixed.nValid
  stepsAhead <- 336 

  ### AR roll-forward
  print(district.name)
  n <- 1
  for(j in seq(from=dt_fixed.nTrain + 1, to=(length(district.ts)-168), by= 168)) {
    print(j)
    dt_train.ts <- window(district.ts, start=1, end= (j - 1))
    dt_valid.ts <- window(district.ts, start=j, end= (j - 1 + stepsAhead))
    dummy_train.ts <- window(dummy.ts, start=1, end= (j - 1))
    dummy_valid.ts <- window(dummy.ts, start=j, end= (j - 1 + stepsAhead))
    # linear regression
    train.lm <- tslm(dt_train.ts ~ trend + dummy_train.ts)
    # linear regression forecast
    train.lm.pred <- forecast(train.lm, newdata=dummy_valid.ts, h=stepsAhead)
    dummyE <- dt_train.ts-train.lm$fitted.values
    main_header <- paste(district.name, "ACF Chart (", n, "Roll )")
    Acf(dummyE, main= main_header)

    # AR forecast
    arima.res <- Arima(train.lm.pred$residuals, order = c(2, 0, 0))  
    # et+k = B1*et-1 + B2*et-2 + B0
    valid.res <- dt_valid.ts - train.lm.pred$mean  
    et1 <- c(train.lm.pred$residuals[j-1], valid.res[168 + 2:stepsAhead])
    et2 <- c(train.lm.pred$residuals[(j-2):(j-1)], valid.res[168 + 3:stepsAhead])
    etk = arima.res$coef[1]*(et1+arima.res$coef[3]) + arima.res$coef[2]*(et2+arima.res$coef[3]) + arima.res$coef[3]
    etk.ts <- window(etk[1:168], start = 1, end = 168)
    # Ft+k*  = Ft+k + et+k
    Ftk <- train.lm.pred$mean[169:336] + etk.ts
    # Plot setting
    dt_roll.ts <- subset(district.ts, start=j-672, end=j+335)
    dt_roll_time.df <- district.df %>% slice((j-672):(j+335))
    dt_roll_train_time.df <- district.df %>% slice((j-672):(j-1))
    dt_roll_gap_time.df <- district.df %>% slice(j:(j+167))
    dt_roll_valid_time.df <- district.df %>% slice((j+168):(j+335))
    # Plot AR
    main_header <- paste( "AR", district.name, "Time series chart (", n, "Roll )")
    plot(y=dt_roll.ts, x=dt_roll_time.df$POS_TIME, xlab="Time",ylab="Hourly Wemo Rentals", main=main_header, type = "l")
    lines(y=train.lm.pred$fitted[(j-672):(j-1)], x=dt_roll_train_time.df$POS_TIME ,lwd=2, lty=1, col="blue")
    color <- case_when( n == 1 ~ "cadetblue2", n == 2 ~ "blueviolet", n == 3 ~ "cadetblue4", n == 4 ~ "blue4")
    lines(y=Ftk, x=dt_roll_valid_time.df$POS_TIME, xlab="Time", lty=1, col= color)
    lines(y=dt_roll.ts[673:840], x=dt_roll_gap_time.df$POS_TIME, xlab="Time", lty=1,lwd=3, col="white")
    # Plot error
    gapE.seq <- rep(NA, 168)
    wholeE.seq <- c(train.lm.pred$residuals, gapE.seq, etk.ts)
    error_main_header <- paste("AR", district.name, "Error chart (", n, "Roll )")
    plot(wholeE.seq[(j-504):(j+335)], xlab="Time", ylab="Errors", main=error_main_header, ylim=c(-200, 200), x=full_index[1562:2401], type="l", col="red", lty=1, lwd=2)
    lines(y=etk.ts, x=valid_index[1:168], col="pink", lwd=2)
    abline(h=0, lty=2)
    # accuracy show
    accuracy.ret <- accuracy(Ftk, dt_valid.ts[169:336])
    print(paste(n, "roll accuracy"))
    print(accuracy.ret)
    # write csv
    gap.seq <- rep(NA, 168)
    forecast.vals <- c(train.lm.pred$fitted, ts(gap.seq, frequency=168), Ftk)
    real.vals <- district.df$rent_nums[1: (j+335)]
    output.df <- data.frame(forecast.vals, real.vals)
    output.df$error.vals <- output.df$real.vals - output.df$forecast.vals
    file.name <- paste("AR", district.name, n, "roll ret.csv")
    write.csv(output.df,file=file.name)
    
    n <- n + 1
} 

# charts won't come up too quick (you can choose how many seconds you want to take a rest)
#Sys.sleep(10)
}

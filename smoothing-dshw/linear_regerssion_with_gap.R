# Load libraries
library(forecast)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyverse)
library(tidyr)

# set the encoding, if not set then reading csv will cause errors
Sys.setlocale(category='LC_ALL', locale='C')


# making time index
full_index <- seq(from = as.POSIXct("2020-01-31 16:00"), to = as.POSIXct("2020-05-31 16:00"), by="hour")
train_index <- seq(from = as.POSIXct("2020-01-31 16:00"), to = as.POSIXct("2020-05-03 16:00"), by="hour")
valid_index <- seq(from = as.POSIXct("2020-05-03 17:00"), to = as.POSIXct("2020-05-31 16:00"), by="hour")
getwd()
districts <- c("Wanhua.csv")

for(district in districts) {
  district.name <- gsub(".csv", "", district)
  print(district.name)
  # read data into a dataframe
  district.df <- read.csv(district)
  # Add pos_time column
  district.df$POS_TIME <- as.POSIXct(district.df$POS_TIME)
  # Define ts object
  nTotal <- length(district.df$rent_nums) # same in three time-series
  district.ts <- ts(district.df$rent_nums)
  dummy.ts <- ts(district.df$dummy)
  # setting for roll-forward
  fixed.nValid <- 24*35 # 5 weeks with 1 week gap)
  dst_fixed.nTrain <- length(district.ts) - fixed.nValid
  stepsAhead <- 336 # 168(one week)*2
  ### district
  # roll-forward
  n <- 1
  for(j in seq(from=dst_fixed.nTrain + 1, to=(length(district.ts) - 168), by= 168)) {
    print(j)
    # run the model (you can change this section)
    dst_train.ts <- window(district.ts, start=1, end= (j - 1))
    dst_valid.ts <- window(district.ts, start=j, end= (j - 1 + stepsAhead))
    dummy_train.ts <- window(dummy.ts, start=1, end= (j - 1))
    dummy_valid.ts <- window(dummy.ts, start=j, end= (j - 1 + stepsAhead))
    # linear regression
    train.lm <- tslm(dst_train.ts ~ trend + dummy_train.ts)
    # linear regression forecast
    train.lm.pred <- forecast(train.lm, newdata=dummy_valid.ts, h=stepsAhead)
    # plotting
    dst_roll.ts <- subset(district.ts, start=j-672, end=j+335)
    dst_roll_time.df <- district.df %>% slice((j-672):(j+335))
    dst_roll_train_time.df <- district.df %>% slice((j-672):(j-1))
    dst_roll_gap_time.df <- district.df %>% slice(j:(j+167))
    dst_roll_valid_time.df <- district.df %>% slice((j+168):(j+335))
    main_header <- paste("Linear regression",district.name, "Time Series Chart (", n, "Roll )")
    plot(y=dst_roll.ts, x=dst_roll_time.df$POS_TIME, xlab="Time",ylab="Hourly Wemo Rentals", main=main_header, type = "l")
    lines(y=train.lm.pred$fitted[(j-672):(j-1)], x=dst_roll_train_time.df$POS_TIME ,lwd=2, lty=1, col="blue")
    lines(y=dst_roll.ts[673:840], x=dst_roll_gap_time.df$POS_TIME, xlab="Time", lty=1, col="white")
    color <- case_when(
      n == 1 ~ "cadetblue2",
      n == 2 ~ "blueviolet",
      n == 3 ~ "cadetblue4",
      n == 4 ~ "blue4"
    )
    lines(y=train.lm.pred$mean[169:336], x=dst_roll_valid_time.df$POS_TIME, lwd=2, lty=1, col=color)
  
    # error show
    dst_valid.ts <- window(district.ts, start=c(0, (j+168)), end= c(0, j+335))
    dst_validSmoothE <- dst_valid.ts - train.lm.pred$mean[169:336]
    dst_trainSmoothE <- train.lm.pred$residuals[(j-672):(j-1)]
    ## plot for the whole residuals in district
    trainSmoothE.df <- data.frame(dst_trainSmoothE)
    trainSmoothE.seq <- trainSmoothE.df$dst_trainSmoothE
  
    # gap seq with NAs
    empty_seq <- seq(1, 168, 1)
    empty_seq[empty_seq != 0] <- NA
  
    validSmoothE.df <- data.frame(dst_validSmoothE)
    validSmoothE.seq <- validSmoothE.df$dst_validSmoothE
  
    wholeSmoothE.seq <- c(trainSmoothE.seq, empty_seq, validSmoothE.seq)
    error_main_header <- paste("Linear regression", district.name, "Error Chart (", n, "Roll )")
    plot(wholeSmoothE.seq, xlab="Time", ylab="Errors", main=error_main_header, ylim=c(-250, 250), x=full_index[(j-672):(j+335)], type="l", col="red", lty=1, lwd=2)
    abline(h=0, lty=2)
    lines(y=dst_validSmoothE,x=full_index[(j+168):(j+335)], col="pink", lwd=2)
  
    # accuracy show
    accuracy.ret <- accuracy(train.lm.pred, dst_valid.ts)
    print(district.name)
    print(paste(n, "roll accuracy"))
    print(accuracy.ret)
  
    # write csv
    forecast.vals <- c(train.lm.pred$fitted, ts(empty_seq, frequency=168), train.lm.pred$mean[169:336])
    real.vals <- district.df$rent_nums[1: (j+335)]
    output.df <- data.frame(forecast.vals, real.vals)
    output.df$error.vals <- output.df$real.vals - output.df$forecast.vals
    file.name <- paste("Linear regression", district.name, n, "roll ret.csv")
    write.csv(output.df,file=file.name)

    n <- n + 1
  }
  # charts won't come up too quick (you can choose how many seconds you want to take a rest)
  Sys.sleep(30)
}

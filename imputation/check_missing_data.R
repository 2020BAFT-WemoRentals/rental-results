# Check strange data

districts <- c("Neihu.csv", "Nangang.csv", "Songshan.csv", "Datong.csv",
               "Wenshan.csv", "Daan.csv", "Beitou.csv", "Zhongshan.csv",
               "Zhongzheng.csv", "Shilin.csv", "Xinyi.csv", "Wanhua.csv")


for(district in districts) {
  district.name <- gsub(".csv", "", district)
  print(district.name)
  # read data into a dataframe
  district.df <- read.csv(district)
  index <- seq(from = as.POSIXct("2020-01-31 16:00"), to = as.POSIXct("2020-05-31 16:00"), by="hour")
  test.df <- data.frame(POS_TIME=index)
  data_with_missing_times.df <- full_join(test.df, district.df)
  write.csv(data_with_missing_times.df, "district_strange.csv")
  
  file.name <- paste(district.name, "imputation.csv")
  write.csv(output.df,file=file.name)
}
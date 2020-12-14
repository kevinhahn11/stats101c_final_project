#Preprocessing and other global functions
set.seed(1)

rm(list=ls())

libraries <- c("dplyr", "ggplot2", "tidyverse", "caret", "randomForest", "lubridate", "hrbrthemes", "viridis", "gbm")
lapply(libraries, require, character.only=T)

print("libaries loaded")

#Calculate the RMSE of testing set
calc_rmse <- function(predictions, dataset="testing"){
  if(dataset=="training"){
    rmse <- sqrt(mean((predictions - training$growth)^2))
  }else{rmse <- sqrt(mean((predictions - testing$growth)^2))}
  
  print(rmse)
}


preprocess <- function(data){
  #Change the Published Date column
  
  data <- cbind(data %>% select(-PublishedDate, -id), convert_time(data$PublishedDate))
  names(data)[which(names(data) == "growth_2_6")] = "growth"
  data <- factorize_channel(data)
  
  
  names(data) <- make.names(names(data), unique = TRUE)
  return(data)
}


#Function to convert published date column into the hour published and day published
convert_time <- function(x){
  
  #Extract the elements of the Published Date
  
  date <- as.Date(str_extract(x, "^.*/.*/...."), '%m/%d/%Y')
  month <- month(date)
  weekday <- wday(date)
  
  time <- str_remove(x, ".*/.*/....")
  
  hour <- as.numeric(str_remove(time, ":.*"))
  
  minutes <- str_extract(time, ":.*")
  minutes <- as.numeric(substring(minutes, 2, nchar(time)))
  
  return(data.frame(Month = month, Weekday=weekday, Hour = hour, Minute = minutes))
}


#Function to convert the low, medium, high variables
factorize_channel <- function(data){
  Subscribers <- rep(4, dim(data)[1])
  Subscribers[data$Num_Subscribers_Base_low == 1] = 1
  Subscribers[data$Num_Subscribers_Base_low_mid == 1] = 2
  Subscribers[data$Num_Subscribers_Base_mid_high == 1] = 3
  
  
  Channel_views <- rep(4, dim(data)[1])
  Channel_views[data$Num_Views_Base_low == 1] = 1
  Channel_views[data$Num_Views_Base_low_mid == 1] = 2
  Channel_views[data$Num_Views_Base_mid_high == 1] = 3
  
  Vid_count <- rep(4, dim(data)[1])
  Vid_count[data$count_vids_low == 1] = 1
  Vid_count[data$count_vids_low_mid == 1] = 2
  Vid_count[data$count_vids_mid_high == 1] = 3
  
  Average_growth <- rep(4, dim(data)[1])
  Average_growth[data$avg_growth_low == 1] = 1
  Average_growth[data$avg_growth_low_mid == 1] = 2
  Average_growth[data$avg_growth_mid_high == 1] = 3
  
  
  
  
  
  channel.info <- data.frame(Subscribers=as.factor(Subscribers),
                             Channel_views=as.factor(Channel_views),
                             Vid_count=as.factor(Vid_count),
                             Average_growth= as.factor(Average_growth))
  
  result <- cbind(data %>% select(-starts_with(c("Num_Subscribers", "Num_Views", "avg_growth", "count_vids"))), channel.info)
  
}


make_submission <- function(){
  #Apply the necessary preprocessing to the submission data
  #Don't need this for now
}



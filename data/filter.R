library(plyr)
library(dplyr)

dataset = read.csv('data/airlines_delay.csv')

#set labels
dataset$Class[dataset$Class == 1] <- 'Delayed'
dataset$Class[dataset$Class == 0] <- 'On Time'
dataset$DayOfWeek <- revalue(dataset$DayOfWeek, c('1' = 'Monday', '2' = 'Tuesday', '3' = 'Wednesday', '4' = 'Thursday', '5' = 'Friday', '6' = 'Saturday', '7' = 'Sunday'))
dataset <- (mutate(dataset, Length = ifelse(dataset$Length %in% 0:180, "Short Flight",
                                      ifelse(dataset$Length %in% 181:360, "Medium Flight", "Long Flight"))))

dataset$Length <- as.factor(dataset$Length)

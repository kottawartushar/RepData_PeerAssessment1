## Load the data
unzip("activity.zip")
activity_data <- read.csv("activity.csv")
head(activity_data)
summary(activity_data)

## Calculate total no. of steps

sum_steps <- tapply(activity_data$steps, activity_data$date, FUN = sum, na.rm = TRUE)
sum_steps

hist(sum_steps, xlab = "Total no. of steps", ylab = "Frequency", 
     main = "Histogram showing Total no. of steps taken each day", breaks = 30)

dev.copy(png, file = "plot1.png", width = 480, height = 480)

dev.off()

mean_steps = mean(sum_steps, na.rm = TRUE)
mean_steps
median_steps = median(sum_steps, na.rm = TRUE)
median_steps

## Average daily activity pattern calculation

average_steps <- aggregate(x = list(total_steps = activity_data$steps),
                           by = list(interval = activity_data$interval), 
                           FUN = mean, na.rm = TRUE)
average_steps

plot(average_steps$interval, average_steps$total_steps, xlab = "5-minute interval", 
     ylab = "Average no. of steps taken" , type = "l")

dev.copy(png, file = "plot2.png", width = 480, height = 480)

dev.off()

max_average <- average_steps[which.max(average_steps$total_steps),]
max_average

## Missing Values

missing_values <- sum(is.na(activity_data))
missing_values

library(imputeTS)
library(ggplot2)

replaced_values <- na_mean(activity_data)
head(replaced_values)
summary(replaced_values)

#missing_values_2 <- sum(is.na(replaced_values))
#missing_values_2

new_sum_steps <- tapply(replaced_values$steps, replaced_values$date, FUN = sum, na.rm = TRUE)
new_sum_steps

qplot(new_sum_steps, xlab = "Total no. of steps taken each day", ylab = "Frequency", 
      main = "Histogram with replaced missing values", binwidth = 1000)

dev.copy(png, file = "plot3.png", width = 480, height = 480)

dev.off()













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
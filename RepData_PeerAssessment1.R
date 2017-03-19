library(knitr)
opts_chunk$set(echo = TRUE)
require(ggplot2)
require(RColorBrewer)
require(dplyr)

loadData <- function(url="", csvFile="data.csv", zipFile="data.zip") {
  if (!file.exists(csvFile)) {
    if (!file.exists(zipFile)) {
      download.file(url, zipFile)
    }
    unzip(zipFile)
  }
  read.csv(csvFile)
}

data = loadData("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity.csv", "data.zip")

head(data)

data$weekday <- weekdays(as.Date(data$date))

head(data)

steps <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print

ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")


mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)

mean_steps
median_steps

interval <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))

ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "firebrick")

interval[which.max(interval$steps),]

sum(is.na(data$steps))

nas <- is.na(data$steps)
avg_interval <- tapply(data$steps, data$interval, mean, na.rm=TRUE, simplify=TRUE)
data$steps[nas] <- avg_interval[as.character(data$interval[nas])]

sum(is.na(data$steps))

data.steps <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print

ggplot(data.steps, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")

mean_steps <- mean(data.steps$steps, na.rm = TRUE)
median_steps <- median(data.steps$steps, na.rm = TRUE)

mean_steps
median_steps

data <- mutate(data, weektype = ifelse(data$weekday == "Saturday" | data$weekday == "Sunday", "weekend", "weekday"))
data$weektype <- as.factor(data$weektype)

head(data)

data.interval <- data %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
s <- ggplot(data.interval, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)




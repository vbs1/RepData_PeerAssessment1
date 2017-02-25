require(ggplot2)
require(lubridate)
require(RColorBrewer)
require(dplyr)
require(ggthemes)
library(scales)

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
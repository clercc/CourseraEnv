library(readr)
hw1_data <- read_csv("C:/Users/Christian/datasciencecoursera/hw1_data.csv")

names(hw1_data)

hw1_data[1:2,]

nrow(hw1_data)

n <- nrow(hw1_data)
hw1_data[(n-1):n,]

hw1_data$Ozone[47]

o <- is.na(hw1_data$Ozone)
summary(o)

h1 <- hw1_data$Ozone[!o]
mean(h1)

hw1_newdata <- hw1_data[hw1_data$Ozone > 31,]
hw1_newdata2 <- hw1_newdata[hw1_newdata$Temp > 90,]
good <- complete.cases(hw1_newdata2)
xbar_solarr <- mean(hw1_newdata2[good,]$Solar.R)
xbar_solarr

hw1_JuneData <- hw1_data[hw1_data$Month == 6,]
navalues <- is.na(hw1_JuneData$Temp) ##validating if any values are NA
a <- as.factor(navalues)
a ## only false -> no NA values
mean(hw1_JuneData$Temp)

hw1_MayData <- hw1_data[hw1_data$Month == 5,]
oz_naval <- is.na(hw1_MayData$Ozone) ##validating existance of NA values
b <- as.factor(oz_naval)
b ## both levels -> existance of NA values
newMayOzData <- hw1_MayData$Ozone[!oz_naval]
max(newMayOzData)

fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
fpath <- "~/GitHub/datasciencecoursera/data/household_power_consumption.zip"
download.file(fileURL, fpath)

unzip(fpath, "household_power_consumption.txt")

dest <- "~/GitHub/datasciencecoursera/data/household_power_consumption.txt"

data <- read.delim(dest, header = TRUE, sep = ";")

library(dplyr)

data2 <- data

data2$Date <- as.Date(data2$Date, format = "%d/%m/%Y")
data2$Global_active_power <- as.numeric(as.character(data2$Global_active_power))
data2$Global_reactive_power <- as.numeric(as.character(data2$Global_reactive_power))
data2$Voltage <- as.numeric(as.character(data2$Voltage))
data2$Global_intensity <- as.numeric(as.character(data2$Global_intensity))
data2$Sub_metering_1 <- as.numeric(as.character(data2$Sub_metering_1))
data2$Sub_metering_2 <- as.numeric(as.character(data2$Sub_metering_2))
data2$Sub_metering_3 <- as.numeric(as.character(data2$Sub_metering_3))
data2$DateTime <- as.POSIXct(paste(data2$Date, data2$Time), format = "%Y-%m-%d %H:%M:%S")
data2 <- data2 %>% filter(Date == "2007-02-01" | Date == "2007-02-02")

png(file = "Plot2.png", width = 480, height = 480, units = "px")
with(data2, plot(DateTime, Global_active_power, 
                 type = "l", 
                 xlab = "", 
                 ylab = "Global Active Power (kilowatt)"
                 )
     )
dev.off()



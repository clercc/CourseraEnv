fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileurl, "C:/Users/Christian/Github/datasciencecoursera/week1.csv")

setwd("C:/Users/Christian/Github/datasciencecoursera")
data <- read.csv("week1.csv")


value = subset(data$VAL, data$VAL == 24)
length(value)

fileurl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileurl2, "C:/Users/Christian/Github/datasciencecoursera/week1ex2.xlsx", mode = "wb")

dat <- read.xlsx("week1ex2.xlsx", sheetIndex = 1, colIndex = 7:15, rowIndex = 18:23)
sum(dat$Zip*dat$Ext,na.rm=T)

fileXML <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileXML, useInternal = TRUE)
rootnode <- xmlRoot(doc)
xmlName(rootnode)
rootnode[1]

zip <- xpathSApply(rootnode, "//zipcode",xmlValue)
zip <- zip[zip == "21231"]
length(zip)


fileurl3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileurl3, "C:/Users/Christian/Github/datasciencecoursera/week2ex3.csv")
data <- fread("week2ex3.csv")

system.time( DT[,mean(pwgtp15), by=SEX])
system.time(mean(DT[DT$SEX==1,]$pwgtp15))
system.time(mean(DT[DT$SEX==2,]$pwgtp15))
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(rowMeans(DT)[DT$SEX==1])
system.time(rowMeans(DT)[DT$SEX==2])
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(mean(DT$pwgtp15,by=DT$SEX))


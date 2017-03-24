fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
fpath <- "~/github/datasciencecoursera/acsid.csv"
download.file(fileURL, fpath)

data <- read.csv(fpath)

splitnames = strsplit(names(data),"wgtp")
splitnames[[123]]




fileURL2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fpath2 <- "~/github/datasciencecoursera/gdpw4.csv"
download.file(fileURL2, fpath2)

gdpo <- read.csv(fpath2)

gdp <- gsub(",", "", gdpo$X.3)
gdp <- as.numeric(as.character(gdp))
gdp <- gdp[!is.na(gdp)]
mean(gdp[1:190])




grep("^United",gdpo$X.2, value = TRUE)




fileURL3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
fpath3 <- "~/github/datasciencecoursera/eduw4.csv"
download.file(fileURL3, fpath3)

edu <- read.csv(fpath3)

gdp_edu <- inner_join(gdpo, edu, by = c("X" = "CountryCode"))
#grep("June 30", gdp_edu$Special.Notes, value = TRUE)
length(grep("June 30", gdp_edu$Special.Notes))




library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
y <- format(sampleTimes, "%a %Y")

y = c(length(grep("2012", sampleTimes)), length(grep("Mon 2012", y)))
y

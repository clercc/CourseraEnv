if(!file.exists("./data")){dir.create("./data")}
fileURL <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileURL,"./data/restaurants.csv", method = "curl")
restData <- read.csv("./data/restaurants.csv")

head(restData)
tail(restData)
str(restData)
summary(restData)

#distribution of councilDistricts
quantile(restData$councilDistrict, na.rm = TRUE)
quantile(restData$councilDistrict, probs = c(0.5,0.75,0.9))

#creates a table of zipcodes and creates an additional column for missing values
table(restData$zipCode, useNA = "ifany")
#creates a two-dimensional table - help gets an idea of any relationships between quantitative variables
table(restData$councilDistrict, restData$zipCode)

#checks for missing values
sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))

#checks if every value satisifies the condition
all(restData$zipCode > 0)

#takes a sum across all the columns
colSums(is.na(restData))
#verifies that all the column sums satisifes the condition
all(colSums(is.na(restData)) == 0)

#illustrates all the zipcodes in the number with a boolean return
table(restData$zipCode %in% c("21212"))

#can subset a dataset
head(restData[restData$zipCode %in% c("21212", "21213"),])


######
#       CROSS TABS      #

data("UCBAdmissions")
DF = as.data.frame(UCBAdmissions)
summary(DF)

#creating a cross tab of frequency breaking it down by gender and addmittance
xt <- xtabs(Freq ~ Gender + Admit, data = DF)
xt

#creates a cross tab quantifying breaks by replicate of tension and wool variables
warpbreaks$replicate <- rep(1:9, len = 54)
xt <- xtabs(breaks ~ ., data = warpbreaks)
xt

#A more readible format in a flat table of all the results in columns by replicates
ftable(xt)


#size of the dataset
fakedata = rnorm(1e5)
object.size(fakedata)
print(object.size(fakedata), units = "Mb")

restData <- read.csv("./data/restaurants.csv")

#creates a sequence index for the data
s1 <- seq(1,10, by=2); s1
s2 <- seq(1,10, length=3); s2
x <- c(1,3,8,25,100); seq(along = x)

#creating a variable in the dataset subsetting another variable with a boolean
restData$nearme = restData$neighborhood %in% c("Roland Park", "Homeland")
table(restData$nearme)

#creating binary variables
restData$zipwrong = ifelse(restData$zipCode <0 , TRUE, FALSE)
table(restData$zipwrong, restData$zipCode < 0)


## Creating Catagorical Variables

#create a catagorical variable based on another variable in the dataset - can be used to cluster
restData$zipGroups = cut(restData$zipCode, breaks = quantile(restData$zipCode))
table(restData$zipGroups)
table(restData$zipGroups, restData$zipCode)

#       an easier method for creating catagorical variables by specifiying by how many groups you want to split up the variable of interest

library(Hmisc)
restData$zipGroups = cut2(restData$zipCode, g = 4)
table(restData$zipGroups)


## Creating Factor Variables
restData$zcf <- factor(restData$zipCode)
restData$zcf[1:10]

class(restData$zcf)

yesno <- sample(c("yes","no"), size = 10, replace = TRUE)
yesnofac = factor(yesno, levels = c("yes","no"))
relevel(yesnofac, ref = "yes")

as.numeric(yesnofac)

library(plyr)

# creates a new table with the new zipgroups data
restData2 = mutate(restData, zipGroups = cut2(zipCode, g = 4))
table(restData2$zipGroups)


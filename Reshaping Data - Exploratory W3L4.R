###             Reshaping Data          ###

#Normalizing a table

#Melt allows you to group by ID and by measure varibles. 
#Measure variables will be under one column thus normalizing a table where multiple measure variables were on the same line.

library(reshape2)
head(mtcars)
mtcars$carname = rownames(mtcars)
carMelt <- melt(mtcars, id = c("carname", "gear", "cyl"), measure.vars = c("mpg", "hp"))
head(carMelt, n = 3)
tail(carMelt, n = 3)

# casting data frames, dcast displays how many measures exist for the called variables
cylData <- dcast(carMelt, cyl ~ variable)
cylData
        #       by adding an aggregation function at the end, the frequency of measures turns into the function which is called
cylData <- dcast(carMelt, cyl ~ variable, mean)
cylData


head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)

spIns = split(InsectSprays$count, InsectSprays$spray)
spIns
sprCount = lapply(spIns, sum)
sprCount
unlist(sprCount)

sapply(spIns, sum)

# Can do the same as above in 1 step with plyr package

ddply(InsectSprays,.(spray),plyr::summarize,sum = sum(count))

#Here we create the same dimensions as the original data set but instead, every spray value is the sum.
#The difference is instead of 6 spray types with each sum, we get a table of the same legth that
#we can now attach to the original dataset to do further analysis.
spraySums <- ddply(InsectSprays, .(spray), plyr::summarize, sum=ave(count,FUN=sum))
dim(spraySums)
dim(InsectSprays)

head(spraySums)
 
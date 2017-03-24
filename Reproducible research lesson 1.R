library(kernlab)
data(spam)
#Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]

names(trainSpam)
head(trainSpam)

table(trainSpam$type)

plot(trainSpam$capitalAve ~ trainSpam$type)
        # Here the data is very skewed.
        # With this kind of data, we should look at the log transform of the quantitative data.
        # We add 1 to the variable because of the amount of 0s in the dataset since taking the log(0) doesn't make sense.
                # This is ok because 1) we're doing exploratory analysis and making exploratory plots. Generally we shouldn't do this.
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

# Pairwise plot 
        # Used to look at the relationships between the variables. We can easily see the correlations.
plot(log10(trainSpam[, 1:4] +1))

# Hierarchal cluster analysis
        # Used to see what what words cluster together. Clustering can be sensative to any skewedness in the distribution of the
        # individual variables. Maybe useful to redo the cluster after the transformation of the predictor space.
hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)


hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1 ))))
plot(hClusterUpdated)


trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x, y) sum(x != (y > 0.5))
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
        lmFormula = reformulate(names(trainSpam)[i], response = "numType")
        glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
        cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]

## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)

## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])

## Classify as 'Spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

## Classification Table
table(predictedSpam, testSpam$type)

## Error Rate
(61 + 458)/(1346 + 458 + 61 + 449)

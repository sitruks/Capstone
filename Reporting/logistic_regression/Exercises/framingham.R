#setwd("C:/Users/v-kursan/Downloads/Capstone_DS/Reporting/logistic_regression/DataSets/")

library(caTools)

framingham = read.csv("C:/Users/v-kursan/Downloads/Capstone_DS/Reporting/logistic_regression/DataSets/framingham.csv")
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
#50-80% in the training set. when lots of data, can put less in train and more in test.
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
# '.' = all other variables
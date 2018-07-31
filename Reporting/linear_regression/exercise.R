setwd("C:/Users/v-kursan/Downloads/Capstone_DS/Reporting/linear_regression/")

##
## Exercise : least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

#Initial Run 1-3
states.data <- readRDS("dataSets/states.rds") 
states.nrg.metro <- subset(states.data, select = c("energy", "metro"))
summary(states.nrg.metro)
cor(states.nrg.metro)
plot(states.nrg.metro)

nrg.metro <- lm(energy ~ metro, data = states.data)
summary(nrg.metro)

#Best Fit Model
nrg.eco2 <- lm(energy ~ metro + toxic + green, data = states.data)
summary(nrg.eco2)

#-----------------------------------------------------------#

#Additional Predictors #1
nrg.metro.per <- lm(energy ~ metro + percent, data = states.data)
summary(nrg.metro.per)
### improves the model, but not very strong to begin with...

#Additional Predictors #2
nrg.all <- lm(energy ~ metro + percent + pop + area + density + miles + waste + toxic + green + expense + income, data = states.data)
summary(nrg.all)
### improves the model greatly, but not a lot of significant variables

#Additional Predictors #3
nrg.waste.eco.money <- lm(energy ~ metro + percent + waste + toxic + green + expense + income, data = states.data)
summary(nrg.waste.eco.money)
### dropping pop, area, and density = good

#Additional Predictors #4
nrg.waste.eco <- lm(energy ~ metro + percent + waste + toxic + green, data = states.data)
summary(nrg.waste.eco)
### adjusted r2 increase with income and expense removal

#Additional Predictors #5
nrg.eco <- lm(energy ~ metro + percent + toxic + green, data = states.data)
summary(nrg.eco)
### adjusted r2 increase with waste removal

#Additional Predictors #6
nrg.eco2 <- lm(energy ~ metro + toxic + green, data = states.data)
summary(nrg.eco2)
### adjusted r2 increase with percent removal
### best fit

#Additional Predictors #7
nrg.eco3 <- lm(energy ~ metro + toxic + green + income, data = states.data)
summary(nrg.eco3)
### adding income doesn't help/hurt much


#-----------------------------------------------------------#



## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

nrg.eco.by.percent <- lm(energy ~ metro*percent + toxic + green, data = states.data)
summary(nrg.eco.by.percent)

coef(summary(nrg.eco.by.percent))
#r2 increases while adjusted r2 decreses.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?


str(states.data$region)
states.data$region <- factor(states.data$region)

#states.data$region <-C(region, base=4)
## is there a difference??

nrg.eco.region <- lm(energy ~ region + metro*percent + toxic + green,
                data=states.data)
summary(nrg.eco.region)

##also having trouble getting 'west' to appear in summary(nrg.eco.region)
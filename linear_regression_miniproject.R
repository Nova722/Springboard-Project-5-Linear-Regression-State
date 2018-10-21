#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory

setwd("~/")

##   You might also start by listing the files in your working directory

getwd() # where am I?


# read the states data
states.data <- readRDS("states.rds") 

#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## Examine the data before fitting models
##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
states.energy <- subset(states.data, select = c("metro", "energy"))
summary(states.energy)
plot(states.energy)
cor(states.energy, use="pairwise")

##   2. Print and interpret the model `summary'
mod.energy <- lm(energy ~ metro, data = states.energy)
summary(mod.energy)

##   3. `plot' the model to look for deviations from modeling assumptions

plot(mod.energy)

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?
states.energy2 <- subset(states.data, select = c("metro", "energy", "pop", "density", "income"))
summary(states.energy2)
plot(states.energy2)
mod.energy2 <- lm(energy ~ metro + pop + density + income, data = states.energy2)
summary(mod.energy2) #this model has an increase in multiple R2 from 0.1154 to  0.1576 which is no surprise
#and adjusted R2 from 0.097 to 0.08274, R2 increased but adjusted R2 decreased slightly
#none of the varables were listed as significant by a . *, **, ***

cor(states.energy2, use = "pairwise") #there may be some MLC in regards to population and density
#will run the model again with only population since we are talking about energy per capita and remove density

states.energy2no.dens <- subset(states.data, select = c("metro", "energy", "pop", "income"))
summary(states.energy2no.dens)
plot(states.energy2no.dens)
mod.energy2no.dens <- lm(energy ~ metro + pop + income, data = states.energy2)
summary(mod.energy2no.dens) #multiple R squared decreased from 0.1576 to .1202
#adjusted R2 decreased from 0.8274 to 0.06279
#this time metro is listed as 0.1 significant
cor(states.energy2no.dens, use = "pairwise")

anova(mod.energy, mod.energy2, mod.energy2no.dens)

#In this case the original model with metro only was the best fit since it had the highest adjusted R2


## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

mod.energy<- lm(energy ~ metro * waste, data = states.data)
summary(mod.energy)


##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

mod.region <- lm(energy ~ metro * waste + region, data = states.data)
anova(mod.region)
summary(mod.region)

#region.N.East is significant compared to the original model, the adjusted R2 value is also much higher in
#this model than the last (model w/o region = 0.07, this model is at .12)

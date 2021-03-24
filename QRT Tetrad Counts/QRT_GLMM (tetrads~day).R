### Data analysis for tetrad pollen count

## Load dataset

# Read dataset for QRT
setwd("./QRT")
data1 <- read.csv("QRT_specific.csv")

# Comments: Y_var is normalized tetrad counts. 
#           Y_var = [ (tetrads on day x for plant y) / (tetrads produced by plant y across all days) ] * [ (positive flowers on day) * (total flowers on day x) +1 ]
#           flower_day 1 is the day when we observed the first open flower in each plant (i.e. flower_day is a normalized version of day during the experimental period) 

# Set variables as categorical 
data1$plant_ID <- as.factor(data1$plant_ID)

# Check for NAs in data
sum(is.na(data1))
#No NAs found

### Exploratory data analysis

## Checking correlations

# Linear relationships 
library(ggplot2)

ggplot(aes(x = flower_proportion, y = total_flowers), data = data1) +
  geom_point() + 
  stat_smooth(method = "lm")

ggplot(aes(x = flower_proportion, y = tetrads), data = data1) +
  geom_point() + 
  stat_smooth(method = "lm")

ggplot(aes(x = total_flowers, y = tetrads), data = data1) +
  geom_point() + 
  stat_smooth(method = "lm")

# Normal distribution
library("ggpubr")
ggqqplot(data1$flower_proportion)
ggqqplot(data1$total_flowers)
ggqqplot(data1$tetrads)

## Outliers

# Outlier search with Cleveland plot
library(lattice)

Z <- cbind(data1$positive_flowers, data1$total_flowers, data1$tetrads, data1$Y_var)
colnames(Z) <- c("positive_flowers", "total_flowers", "tetrads", "Y_var")

plot1 <- dotplot(as.matrix(Z), groups = FALSE,
                 strip = strip.custom(bg = 'white',
                                      par.strip.text = list(cex = 0.8)),
                 scales = list(x = list(relation = "free"),
                               y = list(relation = "free"),
                               draw = FALSE),
                 col = 1, cex  = 0.5, pch = 16,
                 xlab = "Value of the variable",
                 ylab = "Order of the data from csv file")

tiff("QRT_cleveland_plot.tiff", units="in", width=15, height=10, res=300)
plot1
dev.off()

# Outlier search with boxplot
library(ggplot2)

data1$flower_day <- as.factor(data1$flower_day)

ggplot(aes(y = Y_var, x = flower_day), data = data1) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter() 

# Outlier search with Cook's distance
data1$flower_day <- as.numeric(data1$flower_day)
m1 <- lm(Y_var ~ flower_day, data = data1)
par(mfrow=c(2,2))
plot(m1)

#Comments: Four observations (108, 101, 16 and 178) is outlying according to Cook's distance.
#          These observations are removed as it represent an extremely rare occurrence in which tetrad production spiked;
#          Keeping this observations obstructs our goal of modeling the main trend.

# Mapping outliers
data1 <- read.csv("QRT_specific.csv")
data1$flower_day <- as.numeric(data1$flower_day)
data1$plant_ID <- as.factor(data1$plant_ID)

to_remove <- c(108, 101, 16 and 178)

ggplot(aes(y = Y_var, x = flower_day), data = data1[-to_remove,]) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter() +
  geom_jitter(aes(y = Y_var, x = as.numeric(flower_day)),
              data = data1[to_remove,],
              colour="red") +
  xlab("Day") + 
  ylab("Normalized tetrad counts")

## Homoscedasticity check
library(ggplot2)
ggplot(aes(x = flower_day, y = Y_var), data = data1) + geom_boxplot()

#Comments: Data is obvious heterogenous due to it being count data
#          Resolve with GLM Poisson/ Negative binomial

## Normality check
library(ggplot2)
par(mfrow=c(1,1))
hist(data1$Y_var)

#Comments: Very apparent that the data is right-skewed (many zeroes, so mean is close to zero but many 
#          observations are found on far right). The solution is to transform the data or resolve with  
#          GLM Poisson/ Negative binomial

## Check for zero-inflation in Y
library(DHARMa)
par(mfrow = c(1,2))
plot(y = data1$Y_var, x = data1$flower_day, xlab = "Day (normalized)", ylab = "Response")
hist(data1$Y_var, xlab = "Response", main = "")
apply( data1 , 2 , function(x) sum ( x == 0 ) )

zero_count <- colSums(data1==0)/nrow(data1)*100
zero_count

#Comments: Many zeroes, use zero-inflation models

## Collinearity check 
# Comments: Irrelevant because there is only 1 explanatory variable;

## Relationship between X and Y
library(ggplot2)

ggplot(aes(x = flower_day, y = Y_var), data = data1) + 
  geom_point() + 
  facet_wrap(~plant_ID) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0,14,by = 2))

#Comments: There are some influential points that determine the slope of the linear model
#          In retrospect, random slope models did not fit. Resort to random intercept models.

# Pairplot 
library(psych)
pairs.panels(data1, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             )

#Comments: Correlation between Y_var and flower_day is only 0.20 

## Check for interactions
# Comments: Unnecessary because there is only one X variable

## Check if observations of response variable is independent
# Comments:No, same plant is observed over the days. Treat plant_ID variable as a random effect.

## Conclusions
# Comments: One outlier removed
#           Data is obviously heteroscedastic as Y is count data; resolve with GLM Poisson/ Negative binomial.
#           Many zeroes, use zero-inflation model.
#           Plant is a random effect.

### Linear Modeling

## Biological question: Which flower_day would give the highest Y_var?

# Plot to visualize data
library(ggplot2)
data1 <- read.csv("QRT_specific.csv")
data1$flower_day <- as.numeric(data1$flower_day)
data1$plant_ID <- as.factor(data1$plant_ID)
data1 <- data1[-c(108, 101, 16 and 178),]

# Change Y_var into integer for Poisson/ Negative binomial models
data1$Y_var_int <- round(data1$Y_var * 1000)

plot <- ggplot(aes(x = flower_day,y = Y_var), data = data1) + 
  geom_smooth(method='lm') + 
  geom_point() + 
  xlab("Days after flowering") +
  ylab("Normalized tetrad count")

pdf("QRT_XY_relationship.pdf")
print(plot)
dev.off()

# Load finalized data
data1 <- read.csv("QRT_specific.csv")
#data1$flower_day <- as.factor(data1$flower_day) # Doesn't work well; data within a day doesn't explain tetrad count well
data1$flower_day <- as.numeric(data1$flower_day)
data1$plant_ID <- as.factor(data1$plant_ID)
data1 <- data1[-c(108, 101, 16, 178),]
data1$Y_var_int <- round(data1$Y_var * 1000)

library(glmmTMB)
m1 <- glmmTMB(Y_var_int ~ flower_day + (1|plant_ID),
              data=data1,
              ziformula=~1,
              family = poisson)
m2 <- glmmTMB(Y_var_int ~ flower_day + (1|plant_ID),
              data=data1,
              ziformula=~1,
              family=nbinom1)
m3 <- glmmTMB(Y_var_int ~ flower_day + (1|plant_ID),
              data=data1,
              ziformula=~1,
              family=nbinom2)

# Note: Random intercept + random slope model has convergence problem.
#       Use random intercept model.

summary(m1)
summary(m2)
summary(m3)
# Comments: All OK

# Model selection by AIC
AIC(m1,m2,m3)
# Comments: model2 is the best

### Model validation

## Check heteroscedasticity

library(DHARMa)
simres <- simulateResiduals(m2)
plot(simres)
#OK

# Check residuals per random effect group
simulationOutput <- simulateResiduals(fittedModel = m2, n = 250)
simulationOutput <- recalculateResiduals(simulationOutput, group = data1$plant_ID)
plot(simulationOutput)
#OK

#check residues as a function of explanatory variables
par(mfrow = c(1,1))
plot(residuals(m3, type = "response")~ data1$flower_day) 
# Comments: Appears to have heterogeneity; residuals increase as flower_day increases BUT
#           cannot use scaled residuals ("pearson residuals are not implemented for models with zero-inflation or variable dispersion")

#check residues as a function of explanatory variables with scaled residuals
plot(simres$scaledResiduals ~ data1$flower_day, xlab = "Days after flowering", ylab = "Scaled residuals")
# Comments: By using simulated scaled residuals, we can observe homogeneity.
#           OK

# Plot residuals against time and space 
# Comments: Space is irrelevant; this is an experiment conducted in the lab.
#           Time is accounted as X variable.

### Final model
m2 <- glmmTMB(Y_var_int ~ flower_day + (1|plant_ID),
              data=data1,
              ziformula=~1,
              family=nbinom1)
summary(m2)

### Simulate model
library(tidyverse)
library(caret)
library(plyr) 

## K-fold validation

#parameters
set.seed(421)
k <- 10           #number of folds
percent_test <- 0.1    #proportion of data used for testing
data <- data1
df <- data.frame(R2=character(),
                 RMSE=character(), 
                 MAE=character()) 

#remember to edit the model within loop


pbar <- create_progress_bar('text')
pbar$init(k)

## Assign samples to K folds initially
index <- sample(letters[seq_len(k)], nrow(data), replace=TRUE)
for(i in seq_len(k)) {
  
  ## Make all samples assigned current letter the test set
  test_index <- index == letters[[i]]
  test.data <- data[test_index, ]
  
  ## All other samples are assigned to the training set
  train.data <- data[!test_index, ]
  
  ## train model
  model <- glmmTMB(Y_var_int ~ flower_day + (1|plant_ID),
                   data=train.data,
                   ziformula=~1,
                   family=nbinom1)
  
  predictions <- model %>% predict(test.data)
  d <- data.frame( R2 = R2(predictions, test.data$Y_var_int),
                   RMSE = RMSE(predictions, test.data$Y_var_int),
                   MAE = MAE(predictions, test.data$Y_var_int))
  print("Results for this round:")
  print(d)
  df <- rbind(df, d)
  
  pbar$step()
}

#get average of each column 
colMeans(df)

## End of k-fold validation

# R2      RMSE       MAE 
# 0.09807134 36.80082123 18.77878880 

#Explanations of k-fold validation metrics:
# R-squared (R2), representing the squared correlation between the observed outcome 
# values and the predicted values by the model. The higher the adjusted R2, the better the model.
# 
# Root Mean Squared Error (RMSE), which measures the average prediction error made by the model 
# in predicting the outcome for an observation. That is, the average difference between the 
# observed known outcome values and the values predicted by the model. The lower the RMSE, 
# the better the model.
# 
# Mean Absolute Error (MAE), an alternative to the RMSE that is less sensitive to outliers. 
# It corresponds to the average absolute difference between observed and predicted outcomes. 
# The lower the MAE, the better the model



### Model summary
m2 <- glmmTMB(Y_var_int ~ flower_day + (1|plant_ID),
              data=data1,
              ziformula=~1,
              family=nbinom1)
summary(m2)

# Check if random effects are large enough to be seen on plot
ranef(m2)
# Comments: All random effects near 0

### Visualize model
#Source: https://stackoverflow.com/questions/32400410/plotting-glmm-estimate-line-with-categorical-and-interaction-variables
#Create a new data frame that has the means of the variables we want to hold constant, along with a range of values for variables that we want to vary. 
#Use expand.grid to create a dataframe with all combinations of the values listed below
library(ggplot2)

pred.data = expand.grid(flower_day = seq(1, 13, 1),       # X variable
                        plant_ID = unique(data1$plant_ID) # Dependent structure
                        #, Precip=mean(cuckoo$Precip)     # Variables to keep constant is set as mean
                        
)

pred.data$pred = predict(m2, newdata=pred.data)

##Explained: The dataframe has all the variables the model use to predict, including categorical data for 
#            dependent structure. X variable (plotted on X axis) we give as sequence of numbers.
#            pred: calculates a predicted values; output value takes into account random intercept.
#            newdata: An optional data frame in which to look for variables with which to predict. 
#            If omitted, the fitted values are used.

plot1 <- ggplot() + geom_point(data = data1, aes(flower_day, log(Y_var_int))) + 
  #log Y because the predicted values are log due to negative binomial
  geom_line(data = pred.data, aes(flower_day, pred)) + facet_wrap(~plant_ID) +
  xlab("Days after flowering") + ylab("Normalized tetrad counts")

tiff("QRT_visualize_GLMM.tiff", units="in", width=15, height=10, res=300)
plot1
dev.off()

# Information on link functions
?family_glmmTMB


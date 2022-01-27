### Rescue of semi-sterility of msh4 mutant lines: 
### We studied the variable 'well-developed seeds per silique' as a proxy for viable seeds 
### produced by msh4 mutants in Col-0 and Ler in non-treated controls and after treatment 
### with TRV-RECQ4, TRV-FIGL1 and TRV-GUS.

## Load data
setwd("./msh4")
data1 <- read.csv('msh4.csv')

# Filter data to retain only observations with "viable seeds" and "msh4 mutants"
data1 <- data1[data1$viability == "Viable", ]
data1 <- data1[data1$mutant == "msh4",]

# Set variables as categorical 
data1$accession <- as.factor(data1$accession)
data1$treatment <- as.factor(data1$treatment)
data1$plant_ID <- as.factor(data1$plant_ID)

# Check for NAs in data
sum(is.na(data1))
#No NAs found

# Finding means and sd for description in paper
data2 <- data1[data1$accession == "Col-0",]

mean(data2[data2$treatment == "Control",]$viable_seeds_per_silique)
sd(data2[data2$treatment == "Control",]$viable_seeds_per_silique)

mean(data2[data2$treatment == "TRV2-GUS",]$viable_seeds_per_silique)
sd(data2[data2$treatment == "TRV2-GUS",]$viable_seeds_per_silique)

mean(data2[data2$treatment == "TRV2-FIGL",]$viable_seeds_per_silique)
sd(data2[data2$treatment == "TRV2-FIGL",]$viable_seeds_per_silique)

mean(data2[data2$treatment == "TRV2-RECQ4",]$viable_seeds_per_silique)
sd(data2[data2$treatment == "TRV2-RECQ4",]$viable_seeds_per_silique)

### Exploring Col-0 and Ler dataset separately because they differ greatly in viable seed counts

### Exploratory data analysis (Col-0)
data1 <- read.csv('msh4.csv')
data1 <- data1[data1$viability == "Viable", ]
data1 <- data1[data1$mutant == "msh4",]
data1 <- data1[data1$accession == "Col-0",]
data1$accession <- as.factor(data1$accession)
data1$treatment <- as.factor(data1$treatment)
data1$plant_ID <- as.factor(data1$plant_ID)

## Homoscedasticity check
library(ggplot2)
ggplot(aes(y = viable_seeds_per_silique, x = treatment, fill = accession), data = data1) + geom_boxplot()
# Comments: Heteroscedasticity for both datasets detected

# Transformations to achieve homoscedasticity
ggplot(aes(y = log(viable_seeds_per_silique), x = treatment, fill = accession), data = data1) + geom_boxplot()
# Comments: Variance is unequal.

ggplot(aes(y = sqrt(viable_seeds_per_silique), x = treatment, fill = accession), data = data1) + geom_boxplot()
# Comments: Variance is unequal.

# Solution: Model with Poisson or Negative Binomial distribution and check if residuals are scattered randomly.

## Normality check
library(ggplot2)
hist(data1$viable_seeds_per_silique)
# Comments: Not normal, as expected from count response
# Solution: Model with Poisson or Negative Binomial distribution and check if residuals are scattered randomly.

## Outliers (Col-0)

# Outlier search with Cleveland plot
library(lattice)
Z <- cbind(data1$viable_seeds_per_silique)
colnames(Z) <- c("viable seeds per silique")

plot1 <- dotplot(as.matrix(Z), groups = FALSE,
                 strip = strip.custom(bg = 'white',
                                      par.strip.text = list(cex = 0.8)),
                 scales = list(x = list(relation = "free"),
                               y = list(relation = "free"),
                               draw = FALSE),
                 col = 1, cex  = 0.5, pch = 16,
                 xlab = "Value of the variable",
                 ylab = "Order of the data from csv file")

plot1

# Outlier search with boxplot
library(ggplot2)
ggplot(aes(y = viable_seeds_per_silique, x = treatment, 
           hue = accession, fill = accession), data = data1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter()

# Outliers: Col-0 FIGL: row 953
#           Col-0 RECQ4: row 869

# Outlier search with linear model
m1 <- lm(viable_seeds_per_silique ~ treatment, data = data1)
par(mfrow=c(2,2))
plot(m1)
#Influential points: Col-0 FIGL: row 953
#                    Col-0 RECQ4: row 869

# Mapping outliers
# Comments: Outliers will be mapped after analyzing outliers for Ler dataset too; see below's "Mapping outliers"

## Check for zero-inflation in Y
library(DHARMa)
par(mfrow = c(1,2))
plot(y = data1$viable_seeds_per_silique, x = data1$treatment)
hist(data1$viable_seeds_per_silique, xlab = "Response", main = "")
apply( data1 , 2 , function(x) sum ( x == 0 ) )

zero_count <- colSums(data1==0)/nrow(data1)*100
zero_count
#Comments: Only 8.7% of Y is zero. No need for zero-inflated model.


## Multi-collinearity 
# Comment: Irrelevant because there is only 1 explanatory variable.

## X-Y relationship

# Pairplot 
library(psych)
pairs.panels(data1, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

## Interactions between variables
# Comment: Irrelevant because there is only 1 explanatory variable.

## Independent observations
# Comment: Not independent, siliques obtained from same plant; set plant_ID as dependency structure.

### Exploratory data analysis (Ler)
data1 <- read.csv('msh4.csv')
data1 <- data1[data1$viability == "Viable", ]
data1 <- data1[data1$mutant == "msh4",]
data1 <- data1[data1$accession == "Ler",]
data1$accession <- as.factor(data1$accession)
data1$treatment <- as.factor(data1$treatment)
data1$plant_ID <- as.factor(data1$plant_ID)

## Homoscedasticity check
# Comments: Heteroscedasticity for both datasets detected; 
#           see above's "Homoscedasticity check" for both Col-0 and Ler dataset

## Normality check
library(ggplot2)
hist(data1$viable_seeds_per_silique)
hist(sqrt(data1$viable_seeds_per_silique))
hist(log(data1$viable_seeds_per_silique))
# Comments: Not normal; bimodal despite transformation
# Solution: Model with Poisson or Negative Binomial distribution and check if residuals are scattered randomly.

## Outliers

# Outlier search with Cleveland plot
library(lattice)
Z <- cbind(data1$viable_seeds_per_silique)
colnames(Z) <- c("viable seeds per silique")

plot1 <- dotplot(as.matrix(Z), groups = FALSE,
                 strip = strip.custom(bg = 'white',
                                      par.strip.text = list(cex = 0.8)),
                 scales = list(x = list(relation = "free"),
                               y = list(relation = "free"),
                               draw = FALSE),
                 col = 1, cex  = 0.5, pch = 16,
                 xlab = "Value of the variable",
                 ylab = "Order of the data from csv file")
plot1


# Outlier search with boxplot
library(ggplot2)
ggplot(aes(y = viable_seeds_per_silique, x = treatment, hue = accession, 
           fill = accession), data = data1) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter()
# Comments: No outliers

# Outlier search with linear model
m1 <- lm(viable_seeds_per_silique ~ treatment, data = data1)
par(mfrow=c(2,2))
plot(m1)
# Comments: No outliers

# Mapping outliers (both Col-0 and Ler dataset combined)
to_remove <- c(869, 953)

data2 <- read.csv('msh4.csv')
data_to_remove <- data2[to_remove,]

data1 <- read.csv('msh4.csv')
data1 <- data1[-to_remove,]
data1 <- data1[data1$viability == "Viable", ]
data1 <- data1[data1$mutant == "msh4",]
#data1 <- data1[data1$accession == "Col-0",]
data1$accession <- as.factor(data1$accession)
data1$treatment <- as.factor(data1$treatment)
data1$plant_ID <- as.factor(data1$plant_ID)
data_to_keep <- data1


ggplot(aes(y = viable_seeds_per_silique, x = treatment, fill = accession), 
       data = data_to_keep) +
  geom_boxplot(outlier.shape = NA, position=position_dodge(width = 1)) +
  geom_jitter(pch = 21, alpha = 0.7, position = position_jitterdodge(jitter.width = 1, 
                                                                    dodge.width = 1, 
                                                                    jitter.height = 0), 
             size = 1.5, 
             data = data_to_keep) +
  #geom_jitter(aes(y = viable_seeds_per_silique, x = treatment, fill = accession, position = "dodge"), data = data_to_keep) + 
  geom_point(aes(y = viable_seeds_per_silique, x = treatment), position = position_jitterdodge(jitter.width = 0.2),
              data = data_to_remove, col = "red") +
  xlab("Treatment") + 
  ylab("Number of seeds per silique")


## Check for zero-inflation in Y
library(DHARMa)
par(mfrow = c(1,2))
plot(y = data1$viable_seeds_per_silique, x = data1$treatment)
hist(data1$viable_seeds_per_silique, xlab = "Response", main = "")
apply( data1 , 2 , function(x) sum ( x == 0 ) )

zero_count <- colSums(data1==0)/nrow(data1)*100
zero_count
# Comments: No zero-inflation

## Multi-collinearity 
# Comments: Irrelevant because there is only 1 explanatory variable.

## X-Y relationship

# Pairplot 
library(psych)
pairs.panels(data1, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

### Linear Modeling

## Biological question: Does TRV-RECQ4/FIGL1 treatment rescue msh4 mutant's inviable seeds?

# Load finalized data
data1 <- read.csv('msh4.csv')
data1 <- data1[-c(869, 953),]
data1 <- data1[data1$viability == "Viable", ]
data1 <- data1[data1$mutant == "msh4",]
data1$accession <- as.factor(data1$accession)
data1$treatment <- as.factor(data1$treatment)
data1$plant_ID <- as.factor(data1$plant_ID)

library(glmmTMB)

m1 <- glmmTMB(viable_seeds_per_silique ~ treatment + accession + (1|plant_ID), 
              family=poisson, 
              ziformula=~0,
              data = data1)

m2 <- glmmTMB(viable_seeds_per_silique ~ treatment + accession + (1|plant_ID), 
              family=nbinom1, 
              ziformula=~0,
              data = data1)

m3 <- glmmTMB(viable_seeds_per_silique ~ treatment + accession + (1|plant_ID), 
              family=nbinom2, 
              ziformula=~0,
              data = data1)

# Model selection by AIC
AIC(m1,m2,m3)
# Comments: AIC for m2 is the lowest. Use m2. 
summary(m2)

### Model validation

## Check heteroscedasticity
library(DHARMa)
simres <- simulateResiduals(m2)
plot(simres)
# Comments: OK

# Check residuals per random effect group
simulationOutput <- simulateResiduals(fittedModel = m2, n = 250)
simulationOutput <- recalculateResiduals(simulationOutput, group = data1$plant_ID)
plot(simulationOutput)
# Comments: Outlier test is significant but "outliers" in this dataset reflects the haphazard 
#           production of viable seeds per siliques. We cannot change the dataset and will accept
#           these outliers as a source of true natural variation.

#check residues as a function of explanatory variables with scaled residuals
plot(simres$scaledResiduals ~ data1$treatment, xlab = "Scaled residuals", ylab = "Treatment")
# Comments: OK

# Plot residuals against time and space 
# Comments: Space and time is irrelevant; this is an experiment conducted in controlled conditions within the lab, not in the field.

### Final model
library(glmmTMB)
m2 <- glmmTMB(viable_seeds_per_silique ~ treatment + accession + (1|plant_ID), 
              family=nbinom1, 
              ziformula=~0,
              data = data1)
summary(m2)

### Simulate model
library(tidyverse)
library(caret)
library(plyr) 

## K-fold validation

#parameters
set.seed(420)
k <- 10           #number of folds
percent_test <- 0.1    #proportion of data used for testing
data <- data1
df <- data.frame(R2=character(),
                 RMSE=character(), 
                 MAE=character()) 

#remember to edit the model within loop
#remember to edit Y within loop


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
  model <- glmmTMB(viable_seeds_per_silique ~ treatment + accession + (1|plant_ID), 
                   family=nbinom1, 
                   ziformula=~0,
                   data = data1)
  
  predictions <- model %>% predict(test.data)
  d <- data.frame( R2 = R2(predictions, test.data$viable_seeds_per_silique),
                   RMSE = RMSE(predictions, test.data$viable_seeds_per_silique),
                   MAE = MAE(predictions, test.data$viable_seeds_per_silique))
  print("Results for this round:")
  print(d)
  df <- rbind(df, d)
  
  pbar$step()
}

#get average of each column 
colMeans(df)

## End of k-fold validation

#Results:
#R2      RMSE       MAE 
#0.677977 17.344181 11.324229 

### Visualize model

#check if random effects are large enough to be seen on plot
ranef(m2)

### Visualize model
#Source: https://stackoverflow.com/questions/32400410/plotting-glmm-estimate-line-with-categorical-and-interaction-variables
#Create a new data frame that has the means of the variables we want to hold constant, along with a range of values for variables that we want to vary. 
#Use expand.grid to create a dataframe with all combinations of the values listed below

library(ggplot2)
pred.data = expand.grid(treatment = unique(data1$treatment),       # X variable
                        accession = unique(data1$accession),
                        plant_ID = unique(data1$plant_ID)          # Dependent structure
                        #, Precip=mean(cuckoo$Precip)     
                        #variables to keep constant is set as mean
)

#remember to account for link function (e.g. if link is log, use exponential on pred)
pred.data$pred = exp(predict(m2, newdata=pred.data))

#prepare dataset for only Col-0
data1_Col0 <- data1[data1$accession == "Col-0",]
pred.data_Col0 <- pred.data[pred.data$accession == "Col-0",]

#prepare dataset for only Ler
data1_Ler <- data1[data1$accession == "Ler",]
pred.data_Ler <- pred.data[pred.data$accession == "Ler",]

#plot for Col-0
library(ggplot2)
plot1 <- ggplot() + geom_point(data = data1_Col0, aes(x = treatment, 
                                                 y = viable_seeds_per_silique), 
                               position = "jitter", alpha = 0.5) + 
  geom_point(data = pred.data_Col0, aes(x = treatment, y = pred), 
             shape = 18, color = "red", size = 4) + 
  theme(panel.spacing.x = unit(1, "cm"), axis.text.x=element_text(angle = 90, hjust = 0)) +
  facet_wrap(~plant_ID, ncol = 3) + xlab("Treatment") + ylab("Number of seeds per silique")

tiff("msh4_combined_Col-0_visualize_GLMM.tiff", units="in", width=15, height=10, res=300)
plot1
dev.off()

#plot for Ler
plot1 <- ggplot() + geom_point(data = data1_Ler, aes(x = treatment, 
                                                      y = viable_seeds_per_silique), 
                               position = "jitter", alpha = 0.5) + 
  geom_point(data = pred.data_Ler, aes(x = treatment, y = pred), 
             shape = 18, color = "red", size = 4) + 
  theme(panel.spacing.x = unit(0.5, "cm"), axis.text.x=element_text(angle = 90, hjust = 0)) +
  facet_wrap(~plant_ID, ncol = 3) + xlab("Treatment") + ylab("Viable seeds per silique")

tiff("msh4_combined_Ler_visualize_GLMM.tiff", units="in", width=15, height=10, res=300)
plot1
dev.off()


## Changing reference levels in categorical variables (codes for reference)
data1 <- read.csv('msh4.csv')
data1 <- data1[-c(869, 953),]
data1 <- data1[data1$viability == "Viable", ]
data1 <- data1[data1$mutant == "msh4",]
data1$accession <- as.factor(data1$accession)
data1$treatment <- as.factor(data1$treatment)
data1$plant_ID <- as.factor(data1$plant_ID)
data1$treatment <- relevel(data1$treatment, ref = "TRV2-FIGL")
m2 <- glmmTMB(viable_seeds_per_silique ~ treatment + accession + (1|plant_ID), 
              family=nbinom1, 
              ziformula=~0,
              data = data1)
summary(m2)

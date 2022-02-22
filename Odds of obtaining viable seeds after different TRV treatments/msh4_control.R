### Data analysis for odds of obtaining viable seeds in Col-0 
### plants treated with TRV-FIGL1, TRV-RECQ4 and TRV-GUS

## Load data
setwd("./Odds of obtaining viable seeds after different TRV treatments")
data1 <- read.csv('Col-0_controls.csv')
data1$odds <- data1$viable_seeds_per_silique/data1$aborted_seeds_per_silique

# Comments: X = treatment (4 levels: treatment with VIGS RECQ4, FIGL1, GUS or nothing (control))
#           Y = odds (odds of viable seeds)

# Set variables as categorical 
data1$treatment <- as.factor(data1$treatment)
data1$plant_ID <- as.factor(data1$plant_ID)

# Check for NAs in data
sum(is.na(data1))
#No NAs found

### Exploratory data analysis

## Checking correlations

# X-Y relationship
library(ggplot2)
ggplot(aes(x = treatment, y = odds), 
       data = data1) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter() +
  ylab("Odds") +
  xlab("Treatment")

# Comments: Similar median and variance, except for RECQ4 (higher median, lower variance).
#           Many observations had zero dead seeds (thereby odds = viable/dead = viable/0 = Inf)
#           We need GLM with binomial family (Logit) to counteract this.

## Outliers

library(ggplot2)
ggplot(aes(x = treatment, y = viable_seeds_per_silique), 
       data = data1) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter() +
  facet_wrap(~plant_ID) +
  ylab("Viable seeds per silique") +
  xlab("treatment")

# Outliers: row 1, 67, 166, 14, 30, 42

ggplot(aes(x = treatment, y = aborted_seeds_per_silique), 
       data = data1) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter() +
  facet_wrap(~plant_ID) +
  ylab("Aborted seeds per silique") +
  xlab("treatment")

# Outliers: row 67, 166, 86, 30

# Remove outliers
to_remove <- c(1, 67, 166, 14, 30, 42, 86, 30)

data2 <- read.csv('Col-0_controls.csv')
data_to_remove <- data2[to_remove,]

data1 <- read.csv('Col-0_controls.csv')
data_to_keep <- data1[-to_remove,]
data1$treatment <- as.factor(data1$treatment)
data1$plant_ID <- as.factor(data1$plant_ID)
data1 <- data_to_keep

# Mapping outlier for viable seeds
ggplot(aes(y = viable_seeds_per_silique, x = treatment), 
       data = data_to_keep) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~plant_ID) + 
  geom_jitter(width = 0.2, data = data_to_keep) +
  #geom_jitter(aes(y = viable_seeds_per_silique, x = treatment, fill = accession, position = "dodge"), data = data_to_keep) + 
  geom_jitter(aes(y = viable_seeds_per_silique, x = treatment),
              data = data_to_remove, col = "red") +
  xlab("Treatment") + 
  ylab("Viable seeds per silique")

# Mapping outlier for viable seeds
ggplot(aes(y = aborted_seeds_per_silique, x = treatment), 
       data = data_to_keep) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~plant_ID) + 
  geom_jitter(width = 0.2, data = data_to_keep) +
  #geom_jitter(aes(y = viable_seeds_per_silique, x = treatment, fill = accession, position = "dodge"), data = data_to_keep) + 
  geom_jitter(aes(y = aborted_seeds_per_silique, x = treatment),
              data = data_to_remove, col = "red") +
  xlab("Treatment") + 
  ylab("Aborted seeds per silique")

## Assumptions for logistic regression
#  Does not require linearity, normality, homoscedasticity therefore their checks are unnecessary
#  We are only using one X variable, therefore no multicollinearity check

### Linear Modeling

## Biological question: Is there a difference in aborted seeds between treatment levels for WT Col-0?
data1 <- read.csv('Col-0_controls.csv')
to_remove <- c(1, 67, 166, 14, 30, 42, 86, 30)
data_to_keep <- data1[-to_remove,]
data1$treatment <- as.factor(data1$treatment)
data1$plant_ID <- as.factor(data1$plant_ID)
data1 <- data_to_keep

library(lme4)
m1 <- glmer(cbind(viable_seeds_per_silique, aborted_seeds_per_silique) ~ 
              treatment + (1|plant_ID), 
            data = data1,
            family = "binomial")
summary(m1)

# Pseudo-R squared
library(rcompanion)
null_model <- glmer(cbind(viable_seeds_per_silique, aborted_seeds_per_silique) ~ 
                      1 + (1|plant_ID), 
                    data = data1,
                    family = "binomial")
nagelkerke(m1, null = null_model)

# Visualizing model
library(effects)
plot(allEffects(m1), ylab = "Odds of viable seeds over aborted seeds", xlab = "Treatment",
     main = NULL)

# Comments: No significant difference between the treatments
  

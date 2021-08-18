#----------------------------------------------------------#
#
#
#                 CATEX BABE experiment
#
#            Explanatory Variables - Predators
#   
#                  Katerina Sam   2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 1. Load libraries and functions -----
#----------------------------------------------------------#

# delete existing workspace to start clean
rm(list = ls())
library(corrplot)

#----------------------------------------------------------#
# 6. Import data -----
#----------------------------------------------------------#

# 6.1 load predation data and predator data-----
dataset_predators <-  
  readxl::read_xlsx("data/input/CATEX_predators.xlsx")
str(dataset_predators)
summary(dataset_predators)

# first you have to standardize the data and load the version with standardized and correct data, this can be done ony once we have all values
dataset_predators$Bird_pred<-scale(dataset_predators$Bird_pred)
dataset_predators$Insectivore_biomass<-scale(dataset_predators$Insectivore_biomass)
dataset_predators$Bird_biomass<-scale(dataset_predators$Bird_biomass)
dataset_predators$Count_insectivore<-scale(dataset_predators$Count_insectivore)
dataset_predators$Count_birds<-scale(dataset_predators$Count_birds)
summary(dataset_predators)

## bird predation vs. biomass of insectivorous birds ## 
cor(dataset_predators$Bird_pred, dataset_predators$Insectivore_biomass, method="pearson")
cor.test(dataset_predators$Bird_pred, dataset_predators$Insectivore_biomass, method="pearson")

## bird predation vs. number of insectivorous birds ##
cor(dataset_predators$Bird_pred, dataset_predators$Bird_biomass, method="pearson")
cor.test(dataset_predators$Bird_pred, dataset_predators$Bird_biomass, method="pearson")

## bird predation vs. biomass of all birds ##
cor(dataset_predators$Bird_pred, dataset_predators$Count_insectivore, method="pearson")
cor.test(dataset_predators$Bird_pred, dataset_predators$Count_insectivore, method="pearson")

# bird predation vs. count of all birds ##
cor(dataset_predators$Bird_pred, dataset_predators$Count_birds, method="pearson")
cor.test(dataset_predators$Bird_pred, dataset_predators$Count_birds, method="pearson")

### another approach with plot of all correlations -----
dataset_predators_birds <- dataset_predators[c(4, 6:9)]
summary(dataset_predators_birds)

bird<-cor(dataset_predators_birds)
corrplot(bird, method="number", type="upper")

testRes = cor.mtest(dataset_predators_birds, conf.level = 0.95)

corrplot(bird, type="upper", method="color", p.mat = testRes, 
         sig.level = 0.05,  insig = 'p-value',   
         tl.col="black", tl.srt=45,
         diag=FALSE)
## addCoef.col = "black" ## adds coefficient as number to the square

library("PerformanceAnalytics")
chart.Correlation(dataset_predators_birds, histogram=TRUE, pch=19)

library("ggpubr")
ggscatter(dataset_predators, x = "Bird_pred", y = "Insectivore_biomass", 
          add = "reg.line", conf.int = TRUE, label = "Site", 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Predation by birds", ylab = "Biomass of insectivorous birds")


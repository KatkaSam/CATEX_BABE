#----------------------------------------------------------#
#
#
#                 CATEX BABE experiment
#
#                       Config file
#   
#                  Katerina Sam   2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 1. Load libraries and functions -----
#----------------------------------------------------------#

# delete existing workspace to start clean
rm(list = ls())

# Package version control
library(renv)
# renv::init()
# renv::snapshot(lockfile = "data/lock/revn.lock")
renv::restore(lockfile = "data/lock/revn.lock")

# libraries
library(lme4)
library(bbmle)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(MuMIn)
library(emmeans)
library(dplyr)
library(see)
library(qqplotr)
library(glmmTMB)
library(merTools)
library(readr)

#----------------------------------------------------------#
# 1. Import data -----
#----------------------------------------------------------#

# 1.1 caterpillar predation data -----
dataset_catex <-  
  readxl::read_xlsx("data/input/CatexBABE_Complete.xlsx")

Sites <- data.frame(Site = c("TOM", "LAK", "BUB", "KAK", "DRO", "EUC"),
                    Lat = c(42.68, 51.2, 21.6, -5.13, -16.1, -33.62))

dataset_catex$Lat <- Sites$Lat[match(dataset_catex$Site, Sites$Site)]
summary(dataset_catex)
dataset_catex$Branch <- factor(dataset_catex$Branch)
dataset_catex$Site <- factor(dataset_catex$Site)
dataset_catex$Strata <- factor(dataset_catex$Strata)
dataset_catex$Species <- factor(dataset_catex$Species)
summary(dataset_catex)

#----------------------------------------------------------#
# 2. graphical properties definition  -----
#----------------------------------------------------------#

theme_set(theme_classic())
text_size <-  18

PDF_width <-  6
PDF_height <-  8

# get the flat violin geom
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


library(dplyr)
df<-dataset_catex %>%
  group_by(Site, Strata) %>% 
  summarise_each(funs(mean))
colnames(df)
df[c("Site","Strata","BirdProp", "BirdProp", "Bird72")]

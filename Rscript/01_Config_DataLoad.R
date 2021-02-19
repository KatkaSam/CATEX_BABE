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
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(MuMIn)
library(emmeans)
library(performance)
library(glmmTMB)
library(ggplot2)
library(dplyr)
library(see)
library(qqplotr)
library (randomForest)

#----------------------------------------------------------#
# 1. Import data -----
#----------------------------------------------------------#

# 1.1 caterpillar predation data -----
dataset_catex <-  
  readxl::read_xlsx("data/input/CatexBABE_Complete.xlsx")

dataset_catex$Sp2 <- paste(dataset_catex$Site, dataset_catex$Species, sep = "_")

Sites <- data.frame(Site = c("TOM", "LAK", "BUB", "KAK", "DRO", "EUC"),
                    Lat = c(42.68, 51.2, 29.26, -5.13, -16.1, -33.62))

dataset_catex$Lat <- Sites$Lat[match(dataset_catex$Site, Sites$Site)]

#----------------------------------------------------------#
# 2. graphical properties definition  -----
#----------------------------------------------------------#

theme_set(theme_classic())
text_size <-  22

PDF_width <-  10
PDF_height <-  6

# get the flat violin geom
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")






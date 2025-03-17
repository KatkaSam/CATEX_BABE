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

theme_set(theme_classic())
text_size <-  18


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

#----------------------------------------------------------#
# 7. Analyze the correlations between birds and bird predation  -----
#----------------------------------------------------------#

## bird predation vs. biomass of insectivorous birds ## 
cor(dataset_predators$Bird_pred, dataset_predators$Insectivore_biomass, method="pearson")
cor.test(dataset_predators$Bird_pred, dataset_predators$Insectivore_biomass, method="pearson")

## bird predation vs. number of insectivorous birds ##
cor(dataset_predators$Bird_pred, dataset_predators$Count_insectivore, method="pearson")
cor.test(dataset_predators$Bird_pred, dataset_predators$Count_insectivore, method="pearson")

## bird predation vs. biomass of all birds ##
cor(dataset_predators$Bird_pred, dataset_predators$Bird_biomass, method="pearson")
cor.test(dataset_predators$Bird_pred, dataset_predators$Bird_biomass, method="pearson")

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
chart.Correlation(dataset_predators_birds, histogram=TRUE, pch=19, method = "pearson")

library("ggpubr")
ggscatter(dataset_predators, x = "Bird_pred", y = "Insectivore_biomass", 
          add = "reg.line", conf.int = TRUE, label = "Site", 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Predation by birds", ylab = "Biomass of insectivorous birds")

#----------------------------------------------------------#
# 8. Figures for supplementary data   -----
#----------------------------------------------------------#
dataset_predators <-  
  readxl::read_xlsx("data/input/CATEX_predators.xlsx")
str(dataset_predators)
summary(dataset_predators)

Fig1<-dataset_predators %>%
  ggplot(aes(x=Lat, y=Insectivore_biomass/1000, group=Strata, color=Strata)) +
  geom_point(
    data = dataset_predators,
    aes(y = Insectivore_biomass/1000),
    size = 5,shape = 19) +
  coord_flip()  +
  labs(
    x = "Latitude",
    y = expression(paste("Biomass of insectivores (kg)")) )+
  scale_fill_manual(values = c("#42adc7", "#ffb902"))+
  scale_color_manual(values = c("#42adc7", "#ffb902"))+
  theme(
    text = element_text(size = text_size),
    legend.position = "none") +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid")) +
  geom_smooth(size = 2, se = FALSE)
Fig1

Fig2<-dataset_predators %>%
  ggplot( aes(x=Lat, y=Count_insectivore, group=Strata, color=Strata)) +
  geom_point(
    data = dataset_predators,
    aes(y = Count_insectivore),
    size = 5,shape = 19) +
    coord_flip()  +
  labs(
    x = "Latitude",
    y = expression(paste("Abundance of insectivores")) )+
  scale_fill_manual(values = c("#42adc7", "#ffb902"))+
  scale_color_manual(values = c("#42adc7", "#ffb902"))+
  theme(
    text = element_text(size = text_size),
    legend.position = "none") +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid")) +
  geom_smooth(size = 2, se = FALSE)
Fig2

Fig3<-dataset_predators %>%
  ggplot( aes(x=Lat, y=Count_birds, group=Strata, color=Strata)) +
  geom_point(
    data = dataset_predators,
    aes(y = Count_birds),
    size = 5,shape = 19) +
  coord_flip()  +
  labs(
    x = "Latitude",
    y = expression(paste("Abundance of all birds")) )+
  scale_fill_manual(values = c("#42adc7", "#ffb902"))+
  scale_color_manual(values = c("#42adc7", "#ffb902"))+
  theme(
    text = element_text(size = text_size),
    legend.position = "none") +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid")) +
  geom_smooth(size = 2, se = FALSE)
Fig3

Fig4<-dataset_predators %>%
  ggplot( aes(x=Lat, y=Bird_biomass/1000, group=Strata, color=Strata)) +
  geom_point(
    data = dataset_predators,
    aes(y = Bird_biomass/1000),
    size = 5,shape = 19) +
  coord_flip()  +
  labs(
    x = "Latitude",
    y = expression(paste("Biomass of all birds (kg)")) )+
  scale_fill_manual(values = c("#42adc7", "#ffb902"))+
  scale_color_manual(values = c("#42adc7", "#ffb902"))+
  theme(
    text = element_text(size = text_size), legend.position = "none") +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid")) +
  geom_smooth(size = 2, se = FALSE)
Fig4

library(cowplot)
birdplot<-plot_grid(Fig3, Fig4,  Fig2, Fig1,
          labels=c("A)", "B)",'C)','D)'), 
          ncol = 2, nrow = 2, align = 'v', hjust=-2)

ggsave(
  "figures/Predators_birds.pdf",
  birdplot,
  width = PDF_width,
  height = PDF_height,
  units = "in")

#----------------------------------------------------------#
# 9. Analyze the correlations between ants and ant predation  -----
#----------------------------------------------------------#

## bird predation vs. biomass of insectivorous birds ## 
cor(dataset_predators$Arth_pred, dataset_predators$Ants_abundance, method="pearson")
cor.test(dataset_predators$Arth_pred, dataset_predators$Ants_abundance, method="pearson")

Fig1Ant<-dataset_predators %>%
  ggplot( aes(x=Lat, y=Ants_abundance, group=Strata, color=Strata)) +
  coord_flip()  +
  geom_point(
    data = dataset_predators,
    aes(y = Ants_abundance),
    size = 5,shape = 19) +
  labs(
    x = "Latitude",
    y = expression(paste("Abundance of ants")) )+
  scale_fill_manual(values = c("#42adc7", "#ffb902"))+
  scale_color_manual(values = c("#42adc7", "#ffb902"))+
  theme(
    text = element_text(size = text_size),
    legend.position = "none") +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid")) +
  geom_smooth(size = 2, se = FALSE)
Fig1Ant

#----------------------------------------------------------#
# 9. Analyze the correlations between herbivores and bird predation  -----
#----------------------------------------------------------#
dataset_predators$Mean_herb_abundance<-scale(dataset_predators$Mean_herb_abundance)
dataset_predators$Mean_herb_abundance<-as.numeric(na.omit(dataset_predators$Mean_herb_abundance))
summary(dataset_predators)

## bird predation vs. biomass of insectivorous birds ## 
cor(dataset_predators$Arth_pred, dataset_predators$Mean_herb_abundance, method="pearson")
cor.test(dataset_predators$Arth_pred, dataset_predators$Mean_herb_abundance, method="pearson")

cor(dataset_predators$Bird_pred, dataset_predators$Mean_herb_abundance, method="pearson")
cor.test(dataset_predators$Bird_pred, dataset_predators$Mean_herb_abundance, method="pearson")

Fig1Arth<-dataset_predators %>%
  ggplot( aes(x=Lat, y=Mean_herb_abundance, group=Strata, color=Strata)) +
  coord_flip()  +
  geom_point(
    data = dataset_predators,
    aes(y = Mean_herb_abundance),
    size = 5,shape = 19) +
  labs(
    x = "Latitude",
    y = expression(paste("Abundance of arthropod herbivores")) )+
  scale_fill_manual(values = c("#42adc7", "#ffb902"))+
  scale_color_manual(values = c("#42adc7", "#ffb902"))+
  theme(
    text = element_text(size = text_size),
    legend.position = "none") +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid")) +
  geom_smooth(size = 2, se = FALSE)
Fig1Arth

allplot<-plot_grid(Fig3, Fig4,  Fig1Ant, Fig2, Fig1, Fig1Arth,
                    labels=c("A)", "B)",'C)','D)', "E)", "F)"), 
                    ncol = 3, nrow = 2, align = 'v', hjust=-2)
allplot

ggsave(
  "figures/Predators_all_Figure2.pdf",
  allplot,
  width = 15,
  height = 10,
  units = "in")

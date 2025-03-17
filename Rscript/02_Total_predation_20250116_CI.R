#----------------------------------------------------------#
#
#
#                 CATEX BABE experiment
#
#               Total predation analyses
#   
#                  Katerina Sam   2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 3. Exploratory graphs  -----
#----------------------------------------------------------#

# see data
summary(dataset_catex)

# prepare proportional data for graphs
dataset_catex$PropTotPred<-dataset_catex$TotalPred72H/dataset_catex$NonLost72H
summary(dataset_catex)
is.numeric(dataset_catex$PropTotPred) 

(expl_plot1<-
  dataset_catex%>% 
  ggplot(
         aes(
           x = Site,
          y = PropTotPred)) +
  
  geom_flat_violin(
    col = "gray30",
    alpha = 1/2,
    trim = TRUE,
    position = position_nudge(
      x = 0.2,
      y = 0)) +
  
  geom_boxplot(
      width=0.2,
      outlier.shape = NA,
      col = "gray30",
      alpha = 0.5) +
  
  geom_point(
    position = position_jitter(width = 0.15),
    alpha = 1,
    size = 1) +
  
  labs(
    x = "Site", 
    y = expression(paste("Total proportion of predated caterpillars"))) +
  theme(
    text = element_text(size = text_size),
    legend.position = "none"))

ggsave(
  "figures/explor_plot_01_sites.pdf",
  expl_plot1,
  width = PDF_width,
  height = PDF_height,
  units = "in")

(expl_plot2<-
    dataset_catex%>% 
    ggplot(
      aes(
        x = Site,
        y = PropTotPred,
        col=Strata,
        fill=Strata)) +
    
    geom_flat_violin(
      col = "gray30",
      alpha = 1/2,
      trim = TRUE,
      position = position_nudge(
        x = 0.2,
        y = 0)) +
    
    geom_boxplot(
      width=0.2,
      outlier.shape = NA,
      col = "gray30",
      alpha = 0.5) +
    
    geom_point(
      position = position_jitter(width = 0.15),
      alpha = 1,
      size = 1) +
    
    labs(
      x = "Site", 
      y = expression(paste("Total proportion of predated caterpillar"))) +
    theme(
      text = element_text(size = text_size),
      legend.position = "top"))

ggsave(
  "figures/explor_plot_02_sites_Strata.pdf",
  expl_plot2,
  width = PDF_width,
  height = PDF_height,
  units = "in")

### Models TOTAL PREDATION
### The full model which considers treatment in interaction with plant species, distance of the neighbouring 
###trees from the central tree and the directions + it takes the cluster of the trees as random effect
### cbind(TotalPred, OK) is used in the binomial glmers - generally we need to use the number of predated caterpillars and number of those that survived, 
### column OK = exposed - (predated by birds + predated by arthropods + lost)

library(psych)
describeBy(
  dataset_catex,
  dataset_catex$Strata # grouping variable
)

#----------------------------------------------------------#
# 3.1 Model build -----
#----------------------------------------------------------#

glmm_total_predation_full <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2)*Strata + (1|Species),
                  data = dataset_catex, family = "binomial")
glmm_total_predation_module <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),2)*Strata + (1|Species),
                                   data = dataset_catex, family = "binomial")
glmm_total_predation_noStrata <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2) + (1|Species),
                                   data = dataset_catex, family = "binomial")                             
glmm_total_predation_linear <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),1)*Strata + (1|Species),
                                     data = dataset_catex, family = "binomial")
glmm_total_predation_full_add <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2)+Strata + (1|Species),
                                   data = dataset_catex, family = "binomial")
glmm_total_predation_linear_add <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),1)+Strata + (1|Species),
                                     data = dataset_catex, family = "binomial")
glmm_total_predation_Strata <- glmer(cbind(TotalPred72H, Survived72H)~Strata + (1|Species),
                                         data = dataset_catex, family = "binomial")
glmm_total_predation_null <- glmer(cbind(TotalPred72H, Survived72H)~1 + (1|Species),
                                   data = dataset_catex, family = "binomial")
AICctab(glmm_total_predation_full, glmm_total_predation_module, glmm_total_predation_noStrata, glmm_total_predation_linear,
        glmm_total_predation_full_add, glmm_total_predation_linear_add, glmm_total_predation_Strata, glmm_total_predation_null)

# build the best model
glm_predation_select<-glmm_total_predation_full
summary(glmm_total_predation_full)
anova(glmm_total_predation_full)

## Predict the values
newData <- data.frame(Lat = rep(seq(from = -40, to = 55, length.out = 500),2),
                      Strata = rep(c("U", "C"), each = 500))

newData$Predation <- predict(glm_predation_select, newdata = newData, re.form = NA, type = "response")

model_plot_01 <-plot(dataset_catex$TotalPred72H/(dataset_catex$TotalPred72H + dataset_catex$Survived72H) ~ 
       jitter(dataset_catex$Lat), col = c("deepskyblue3", "goldenrod3")[as.numeric(as.factor(dataset_catex$Strata))])
lines(newData$Lat[newData$Strata == "U"], 
      newData$Predation[newData$Strata == "U"], col = "goldenrod3")
lines(newData$Lat[newData$Strata == "C"], 
      newData$Predation[newData$Strata == "C"], col = "deepskyblue3")

newData %>% 
  as_tibble() %>% 
  write_csv("data/output/OK_prediction_total_predation.csv")


library(merTools)
#Generate the fitted lines for the model
NewDataPredTot <- data.frame(Lat = rep(seq(from = -40, to = 55, length.out = 500),2),
                             Strata = rep(c("U", "C"), each = 500),
                             Species = factor("Acacia_parramattensis", levels = levels(model.frame(glm_predation_select)$Species)))

Lat_poly <- poly(NewDataPredTot$Lat, 2, coefs = attr(model.frame(glm_predation_select)$`poly(Lat, 2)`, "coefs"))
NewDataPredTot <- cbind(NewDataPredTot, Lat_poly)

NewDataPredTot$TotPredation <- predict(glm_predation_select, newdata = NewDataPredTot, re.form = NA, type = "response")
TotPredInterval <- predictInterval(glm_predation_select, 
                                   newdata = NewDataPredTot, 
                                   which = "fixed", level = 0.95, stat = "median",
                                   n.sims = 20000, type = "probability")
# Add the prediction intervals to the dataset
NewDataPredTot$TotPredationLwr <- TotPredInterval$lwr
NewDataPredTot$TotPredationUpr <- TotPredInterval$upr

NewDataPredTot$Strata<-as.factor(NewDataPredTot$Strata)
summary(NewDataPredTot)
str(NewDataPredTot)
NewDataPredTot %>% 
  as_tibble() %>% 
  write_csv("data/output/Predictions_total_predation_CI_20250120.csv")

#----------------------------------------------------------#
# 3.2 Figure from model draw -----
#----------------------------------------------------------#
colnames(NewDataPredTot)
model_plot_01 <- ggplot() +
    # Add jittered points
  geom_point(
    data = dataset_catex,
    aes(
      x = Lat,
      y = PropTotPred,
      col = Strata,
      fill = Strata),
    size = 3, alpha = 0.5, 
    position = position_jitterdodge(
      dodge.width = 2,
      jitter.width = 2)) +
  
  # Add the confidence interval ribbon
  geom_ribbon(
    data = NewDataPredTot,
    aes(
      x = Lat,
      ymin = TotPredationLwr,
      ymax = TotPredationUpr,
      fill = Strata),
    alpha = 0.2) +
  
  # Add the fitted line
  geom_line(
    data = NewDataPredTot,
    aes(
      x = Lat,
      y = TotPredation,
      col = Strata),
    size = 2) +
  
  # Flip the coordinates
  coord_flip() +
  
  # Add labels and style
  labs(
    x = "Latitude",
    y = expression(paste("Total proportion of caterpillars attacked"))) +
  scale_fill_manual(values = c("#42adc7", "#ffb902")) +
  scale_color_manual(values = c("#42adc7", "#ffb902")) +
  theme(
    text = element_text(size = text_size),
    legend.position = "right",
    axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
    axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"))
model_plot_01

# save pdf
ggsave(
  "figures/Figure_1_total_predation_CI_20250116.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")






#----------------------------------------------------------#
# 3.2 ENFORCE REMOVEL OF THE STRATA  -----
#----------------------------------------------------------#

glm_predation_select<-glmm_total_predation_noStrata 

## Predict the values
newData <- data.frame(Lat = rep(seq(from = -40, to = 55, length.out = 500),2),
                      Strata = NA)

newData$Predation <- predict(glm_predation_select, newdata = newData, re.form = NA, type = "response")

model_plot_01 <-plot(dataset_catex$TotalPred72H/(dataset_catex$TotalPred72H + dataset_catex$Survived72H) ~ 
                       jitter(dataset_catex$Lat), col = c("deepskyblue3", "goldenrod3")[as.numeric(as.factor(dataset_catex$Strata))])
lines(newData$Lat[newData$Strata == "U"], 
      newData$Predation[newData$Strata == "U"], col = "goldenrod3")
lines(newData$Lat[newData$Strata == "C"], 
      newData$Predation[newData$Strata == "C"], col = "deepskyblue3")

library(merTools)
#Generate the fitted lines for the model
NewDataPredTot <- data.frame(Lat = rep(seq(from = -40, to = 55, length.out = 500),2),
                             Strata = NA,
                             Species = factor("Acacia_parramattensis", levels = levels(model.frame(glm_predation_select)$Species)))

Lat_poly <- poly(NewDataPredTot$Lat, 2, coefs = attr(model.frame(glm_predation_select)$`poly(Lat, 2)`, "coefs"))
NewDataPredTot <- cbind(NewDataPredTot, Lat_poly)

NewDataPredTot$TotPredation <- predict(glm_predation_select, newdata = NewDataPredTot, re.form = NA, type = "response")
TotPredInterval <- predictInterval(glm_predation_select, 
                                   newdata = NewDataPredTot, 
                                   which = "fixed", level = 0.65, stat = "median",
                                   n.sims = 20000, type = "probability")
# Add the prediction intervals to the dataset
NewDataPredTot$TotPredationLwr <- TotPredInterval$lwr
NewDataPredTot$TotPredationUpr <- TotPredInterval$upr


summary(NewDataPredTot)
str(NewDataPredTot)

NewDataPredTot %>% 
  as_tibble() %>% 
  write_csv("data/output/Predictions_NOSTRATA_total_predation_CI_20250120.csv")

#----------------------------------------------------------#
# 3.2 Figure from model draw -----
#----------------------------------------------------------#
model_plot_02a <- ggplot() +
  # Add jittered points
  geom_point(
    data = dataset_catex,
    aes(
      x = Lat,
      y = PropTotPred,
      col = Strata,
      fill = Strata),
    size = 3, alpha = 0.5, 
    position = position_jitterdodge(
      dodge.width = 0.8,
      jitter.width = 2,
      jitter.height = 0.01)) +
    
        # Add the confidence interval ribbon
    geom_ribbon(
      data = NewDataPredTot,
      aes(
        x = Lat,
        ymin = TotPredationLwr,
        ymax = TotPredationUpr,
        fill = Strata),
      alpha = 0.2) +
    
    # Add the fitted line
    geom_line(
      data = NewDataPredTot,
      aes(
        x = Lat,
        y = TotPredation,
        col = Strata),
      size = 2, linetype = "dashed") +
  
   coord_flip()  +
    
    labs(
      x = "Latitude",
      y = expression(paste("Total proportion of caterpillars attacked")) )+
    scale_fill_manual(values = c("black", "grey"))+
    scale_color_manual(values = c("black", "grey"))+
    theme(
      text = element_text(size = text_size),
      legend.position = "right") +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"))
model_plot_02a


# save pdf
ggsave(
  "figures/Figure_1_total_predation_NoStrata_CI_20250116.pdf",
  model_plot_02a,
  width = PDF_width,
  height = PDF_height,
  units = "in")

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
summary(dataset_catexB)

# prepare proportional data for graphs
dataset_catexB$PropTotPred<-dataset_catexB$TotalPred72H/dataset_catexB$NonLost72H
summary(dataset_catexB)
is.numeric(dataset_catexB$PropTotPred) 

(expl_plot1<-
  dataset_catexB%>% 
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
  "figures/explor_plot_01_sites_ToBIRDS.pdf",
  expl_plot1,
  width = PDF_width,
  height = PDF_height,
  units = "in")

(expl_plot2<-
    dataset_catexB%>% 
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
  "figures/explor_plot_02_sites_Strata_ToBIRD.pdf",
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
  dataset_catexB,
  dataset_catexB$Strata # grouping variable
)

#----------------------------------------------------------#
# 3.1 Model build -----
#----------------------------------------------------------#

glmm_total_predation_full <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2)*Strata + (1|Species),
                  data = dataset_catexB, family = "binomial")
glmm_total_predation_module <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),2)*Strata + (1|Species),
                                   data = dataset_catexB, family = "binomial")
glmm_total_predation_noStrata <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2) + (1|Species),
                                   data = dataset_catexB, family = "binomial")                             
glmm_total_predation_linear <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),1)*Strata + (1|Species),
                                     data = dataset_catexB, family = "binomial")
glmm_total_predation_full_add <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2)+Strata + (1|Species),
                                   data = dataset_catexB, family = "binomial")
glmm_total_predation_linear_add <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),1)+Strata + (1|Species),
                                     data = dataset_catexB, family = "binomial")
glmm_total_predation_Strata <- glmer(cbind(TotalPred72H, Survived72H)~Strata + (1|Species),
                                         data = dataset_catexB, family = "binomial")
glmm_total_predation_null <- glmer(cbind(TotalPred72H, Survived72H)~1 + (1|Species),
                                   data = dataset_catexB, family = "binomial")
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

model_plot_01 <-plot(dataset_catexB$TotalPred72H/(dataset_catexB$TotalPred72H + dataset_catexB$Survived72H) ~ 
       jitter(dataset_catexB$Lat), col = c("deepskyblue3", "goldenrod3")[as.numeric(as.factor(dataset_catexB$Strata))])
lines(newData$Lat[newData$Strata == "U"], 
      newData$Predation[newData$Strata == "U"], col = "goldenrod3")
lines(newData$Lat[newData$Strata == "C"], 
      newData$Predation[newData$Strata == "C"], col = "deepskyblue3")

newData %>% 
  as_tibble() %>% 
  write_csv("data/output/OK_prediction_total_predation_ToBIRDS.csv")

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
  write_csv("data/output/LostToBird_predictions_total_predation_CI_20250121.csv")


#----------------------------------------------------------#
# 3.2 Figure from model draw -----
#----------------------------------------------------------#

TotalFull <- data.frame(Site = c("TOM", "TOM","LAK", "LAK","BUB", "BUB","KAK", "KAK","DRO", "DRO","EUC","EUC"),
                    Strata = c("U", "C", "U", "C","U", "C","U", "C","U", "C","U", "C"),
                    Lat = c(42.68, 42.68, 51.2, 51.2, 21.6, 21.6, -5.13, -5.13, -16.1, -16.1, -33.62, -33.62),
                    pred<-c(0.130998903, 0.319239941, 0.097649634, 0.52461993, 
                            0.219163419, 0.115320626, 0.29623157, 0.119884521, 
                            0.304426333, 0.193738634, 0.285544793, 0.532831398))


(model_plot_01 <- ggplot(dataset_catexB,
  aes(
    x=Lat,
    y = PropTotPred,
    col = Strata,
    fill=Strata,
    size = 3)) +
   
   geom_point(
     data = dataset_catexB,
     aes(y = PropTotPred),
     size = 3, alpha = 0.5, 
     position = position_jitterdodge(
       dodge.width = 2,
       jitter.width = 2)) +
   
  geom_line(data = newData, aes(y = Predation), size = 2) +
    
    coord_flip()  +
   
   labs(
     x = "Latitude",
     y = expression(paste("Total proportion of caterpillars attacked")) )+
   scale_fill_manual(values = c("#42adc7", "#ffb902"))+
   scale_color_manual(values = c("#42adc7", "#ffb902"))+
   theme(
     text = element_text(size = text_size),
     legend.position = "right")) +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"))

ggsave(
  "figures/OK_model_plot_01_TotalPredation_ToBIRDS.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")


#----------------------------------------------------------#
# 3.3 Model and figure from absolute latitude FUN TEST  -----
#----------------------------------------------------------#

dataset_catexB2 <-  
  readxl::read_xlsx("data/input/CatexBABE_Complete.xlsx")

Sites <- data.frame(Site = c("TOM", "LAK", "BUB", "KAK", "DRO", "EUC"),
                    Lat = c(42.68, 51.2, 21.6, 5.13, 16.1, 33.62))

dataset_catexB2$Lat <- Sites$Lat[match(dataset_catexB2$Site, Sites$Site)]


glmm_total_predation_full <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2)*Strata + (1|Species),
                                   data = dataset_catexB2, family = "binomial")
glmm_total_predation_module <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),2)*Strata + (1|Species),
                                     data = dataset_catexB, family = "binomial")
glmm_total_predation_noStrata <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2) + (1|Species),
                                       data = dataset_catexB2, family = "binomial")                             
glmm_total_predation_linear <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),1)*Strata + (1|Species),
                                     data = dataset_catexB2, family = "binomial")
glmm_total_predation_full_add <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2)+Strata + (1|Species),
                                       data = dataset_catexB2, family = "binomial")
glmm_total_predation_linear_add <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),1)+Strata + (1|Species),
                                         data = dataset_catexB2, family = "binomial")
glmm_total_predation_Strata <- glmer(cbind(TotalPred72H, Survived72H)~Strata + (1|Species),
                                     data = dataset_catexB2, family = "binomial")
glmm_total_predation_null <- glmer(cbind(TotalPred72H, Survived72H)~1 + (1|Species),
                                   data = dataset_catexB2, family = "binomial")
AICctab(glmm_total_predation_full, glmm_total_predation_module, glmm_total_predation_noStrata, glmm_total_predation_linear,
        glmm_total_predation_full_add, glmm_total_predation_linear_add, glmm_total_predation_Strata, glmm_total_predation_null)

# build the best model
glm_predation_select2<-glmm_total_predation_full


## Predict the values
newData2 <- data.frame(Lat = rep(seq(from = 0, to = 55, length.out = 500),2),
                      Strata = rep(c("U", "C"), each = 500))

newData2$Predation <- predict(glm_predation_select, newdata = newData2, re.form = NA, type = "response")

model_plot_02absol <-plot(dataset_catexB$TotalPred72H/(dataset_catexB$TotalPred72H + dataset_catexB$Survived72H) ~ 
                       jitter(dataset_catexB2$Lat), col = c("deepskyblue3", "goldenrod3")[as.numeric(as.factor(dataset_catexB$Strata))])
lines(newData2$Lat[newData2$Strata == "U"], 
      newData2$Predation[newData2$Strata == "U"], col = "goldenrod3")
lines(newData2$Lat[newData2$Strata == "C"], 
      newData2$Predation[newData2$Strata == "C"], col = "deepskyblue3")


newData2 %>% 
  as_tibble() %>% 
  write_csv("data/output/xx_prediction_latitude_absolute.csv")

#----------------------------------------------------------#
# 3.4 Model build for DISCRETE SITES NOT USED NYMORE  -----
#----------------------------------------------------------#

# compute all posible combinations
glm_total_predation_full <- glm(cbind(TotalPred72H, Survived72H)~Site*Strata,
                                data = dataset_catexB, family = "binomial", 
                                na.action = "na.fail")

# compute all posible combinations
glm_total_predation_dd <- 
  MuMIn::dredge(
    glm_total_predation_full,
    trace = T)

# save result table
glm_total_predation_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/xx_total_predation_model_result_discrete.csv")

# observe the best model
glm_total_predation_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()

# build the best model
glm_predation_select<-glmm_total_predation_full

summary(glm_predation_select)
check_model(glm_predation_select, binwidth = 10)
model_performance(glm_predation_select)
check_heteroscedasticity(glm_predation_select)
qplot(residuals(glm_predation_select))

# calculate emmeans
glm_predation_emmeans <-
  emmeans(
    glm_predation_select,
    pairwise ~ Strata*Site,
    type = "response")

plot(glm_predation_emmeans)

#----------------------------------------------------------#
# 3.2 Figure from DISCRETE MODEL -----
#----------------------------------------------------------#

(model_plot_01 <- 
   glm_predation_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x=Site,
        y = prob,
        col = Strata,
        fill=Strata)) + 
  
  scale_x_discrete(limits=c("LAK", "TOM", "BUB", "DRO", "KAK", "EUC")) +
   
  geom_point(
      data = dataset_catexB,
      aes(y = PropTotPred),
      alpha = 0.5,
      size = 2,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  asymp.LCL,
        ymax = asymp.UCL),
      width=0.3,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 2)+
    
    geom_point(
      shape = 0,
      position = position_dodge(width = 0.5),
      size = 4) +
    
    labs(
      x = "Site",
      y = expression(paste("Total proportion of attacked caterpillars")) )+
   scale_fill_manual(values = c("deepskyblue3", "goldenrod3"))+
   scale_color_manual(values = c("deepskyblue3", "goldenrod3"))+
    theme(
      text = element_text(size = text_size),
      legend.position = "right")) +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"))

# to turn and rescale the figure 
model_plot_01<-model_plot_01 + coord_flip() +
  scale_x_discrete(limits=c("EUC", "DRO", "KAK",  "BUB", "LAK","TOM")) 

# save pdf
ggsave(
  "figures/xx_model_plot_DISCRETE_total_predation.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# save the pairwise test 
glm_predation_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/xx_total_predation_pairwise_contrast.csv")

glm_predation_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/xx_total_predation_pairwise_emmeans.csv")


#----------------------------------------------------------#
#
#
#                 CATEX BABE experiment
#
#               Arthropod predation analyses
#   
#                  Katerina Sam   2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 5. Exploratory graphs for arthropod predation -----
#----------------------------------------------------------#

# see data
summary(dataset_catex)

(expl_plot5<-
  dataset_catex%>% 
  ggplot(
         aes(
           x = Site,
          y = ArthProp)) +
  
  scale_x_discrete(limits=c("TOM", "LAK", "BUB", "DRO", "KAK", "EUC")) +
  
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
    y = expression(paste("Proportion of caterpillars attacked by arthropods"))) +
  theme(
    text = element_text(size = text_size),
    legend.position = "none"))

ggsave(
  "figures/explor_plot_05_Arthropod_sites.pdf",
  expl_plot5,
  width = PDF_width,
  height = PDF_height,
  units = "in")

(expl_plot6<-
    dataset_catex%>% 
    ggplot(
      aes(
        x = Site,
        y = ArthProp,
        col=Strata,
        fill=Strata)) +
    
    scale_x_discrete(limits=c("TOM", "LAK", "BUB", "DRO", "KAK", "EUC")) +
    
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
      y = expression(paste("Proportion of caterpillars attacked by arthropods"))) +
    theme(
      text = element_text(size = text_size),
      legend.position = "top"))

ggsave(
  "figures/explor_plot_06_Arthropod_sites_Strata.pdf",
  expl_plot6,
  width = PDF_width,
  height = PDF_height,
  units = "in")

### Models arthopod predation
### cbind(dead, alive) is used in the binomial models
### generally we need to use the number of predated caterpillars and number of those that survived 


#----------------------------------------------------------#
# 5.1 Model build for arthropod predation  -----
#----------------------------------------------------------#

glmm_arthropod_predation_full <- glmer(cbind(Arth72, Survived72H)~poly(Lat,2)*Strata + (1|Species),
                                   data = dataset_catex, family = "binomial")
glmm_arthropod_predation_module <- glmer(cbind(Arth72, Survived72H)~poly(abs(Lat),2)*Strata + (1|Species),
                                     data = dataset_catex, family = "binomial")
glmm_arthropod_predation_noStrata <- glmer(cbind(Arth72, Survived72H)~poly(Lat,2) + (1|Species),
                                       data = dataset_catex, family = "binomial")
glmm_arthropod_predation_linear <- glmer(cbind(Arth72, Survived72H)~poly(abs(Lat),1)*Strata + (1|Species),
                                     data = dataset_catex, family = "binomial")
glmm_arthropod_predation_full_add <- glmer(cbind(Arth72, Survived72H)~poly(Lat,2)+Strata + (1|Species),
                                       data = dataset_catex, family = "binomial")
glmm_arthropod_predation_linear_add <- glmer(cbind(Arth72, Survived72H)~poly(abs(Lat),1)+Strata + (1|Species),
                                         data = dataset_catex, family = "binomial")
glmm_arthropod_predation_Strata <- glmer(cbind(Arth72, Survived72H)~Strata + (1|Species),
                                     data = dataset_catex, family = "binomial")
glmm_arthropod_predation_null <- glmer(cbind(Arth72, Survived72H)~1 + (1|Species),
                                   data = dataset_catex, family = "binomial")
AICctab(glmm_arthropod_predation_full, glmm_arthropod_predation_module, glmm_arthropod_predation_noStrata, glmm_arthropod_predation_linear,
        glmm_arthropod_predation_full_add, glmm_arthropod_predation_linear_add, glmm_arthropod_predation_Strata, glmm_arthropod_predation_null)

glm_arthropod_predation_select <- glmm_arthropod_predation_full
summary(glmm_arthropod_predation_full)
anova(glmm_arthropod_predation_full)

## Predict the values - THIS IS TO GENERATE A FITTED LINE WITHOUT CI INTERVALS
newDataArth <- data.frame(Lat = rep(seq(from = -40, to = 55, length.out = 500),2),
                          Strata = rep(c("U", "C"), each = 500))
newDataArth$ArthPredation <- predict(glm_arthropod_predation_select , newdata = newDataArth, re.form = NA, type = "response")

newDataArth %>% 
  as_tibble() %>% 
  write_csv("data/output/OK_prediction_Arth_predation_NOLAK.csv")



library(merTools)
#Generate the fitted lines for the model
NewDataPredArth <- data.frame(Lat = rep(seq(from = -40, to = 55, length.out = 500),2),
                          Strata = rep(c("U", "C"), each = 500),
                          Species = factor("Acacia_parramattensis", levels = levels(model.frame(glm_arthropod_predation_select)$Species)))

Lat_poly <- poly(NewDataPredArth$Lat, 2, coefs = attr(model.frame(glm_arthropod_predation_select)$`poly(Lat, 2)`, "coefs"))
NewDataPredArth <- cbind(NewDataPredArth, Lat_poly)

NewDataPredArth$ArthPredation <- predict(glm_arthropod_predation_select, newdata = NewDataPredArth, re.form = NA, type = "response")
NewDataPredArth$ArthPredationIntervals <- predictInterval(glm_arthropod_predation_select, 
                                                      newdata = NewDataPredArth, 
                                              which = "fixed", level = 0.95, stat = "median",
                                              n.sims = 20000, type = "probability")

NewDataPredArth %>% 
  as_tibble() %>% 
  write_csv("data/output/INTERVALS_prediction_Arth_predation_NOLAK.csv")


model_plot_01 <-plot(dataset_catex$ArthProp ~ 
                       jitter(dataset_catex$Lat), col = c("deepskyblue3", "goldenrod3")[as.numeric(as.factor(dataset_catex$Strata))])
lines(newDataArth$Lat[newDataArth$Strata == "U"], 
      newDataArth$ArthPredation[newDataArth$Strata == "U"], col = "goldenrod3")
lines(newDataArth$Lat[newDataArth$Strata == "C"], 
      newDataArth$ArthPredation[newDataArth$Strata == "C"], col = "deepskyblue3")

#----------------------------------------------------------#
# 5.2 Figure from model draw -----
#----------------------------------------------------------#

summary(NewDataPredArth)
NewDataPredArth$ArthPredationIntervals.lwr <- NewDataPredArth$ArthPredationIntervals$lwr
NewDataPredArth$ArthPredationIntervals.upr <- NewDataPredArth$ArthPredationIntervals$upr

(model_plot_03 <- ggplot(dataset_catex,
                         aes(
                           x=Lat,
                           y = ArthProp,
                           col = Strata,
                           fill=Strata,
                           size = 3)) +
  
   scale_y_reverse() +
   coord_flip()+
   
   
   geom_point(
     data = dataset_catex,
     aes(y = ArthProp),
     size = 3,
     position = position_jitterdodge(
       dodge.width = 2,
       jitter.width = 2)) +
   
   geom_line(data = NewDataPredArth, aes(y = ArthPredation), size = 2) +
   geom_line(data = NewDataPredArth, aes(y = ArthPredationIntervals.lwr), alpha = 0.4, size = 1, linetype="dashed") +
   geom_line(data = NewDataPredArth, aes(y = ArthPredationIntervals.upr), alpha = 0.4, size = 1, linetype="dashed") +
    
   labs(
     x = "Latitude",
     y = expression(paste("Proportion attacked by arthropods")) )+
   scale_fill_manual(values = c("#42adc7", "#ffb902"))+
   scale_color_manual(values = c("#42adc7", "#ffb902"))+
   theme(
     text = element_text(size = text_size),
     legend.position = "none")) +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"))

ggsave(
  "figures/CI_model_plot_03_ArthropodPredations_NoLAK.pdf",
  model_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# scale_x_reverse()
# scale_y_reverse() +
# scale_y_continuous(position = "right") +


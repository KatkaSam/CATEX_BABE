#----------------------------------------------------------#
#
#
#                 CATEX BABE experiment
#
#               Bird predation analyses
#   
#                  Katerina Sam   2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 4. Exploratory graphs for bird predation -----
#----------------------------------------------------------#

# see data
summary(dataset_catex)

# run some exploratory graphs
(expl_plot3<-
  dataset_catex%>% 
  ggplot(
         aes(
           x = Site,
          y = BirdProp)) +
  
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
    y = expression(paste("Proportion of caterpillars attacked by birds"))) +
  theme(
    text = element_text(size = text_size),
    legend.position = "none"))

ggsave(
  "figures/explor_plot_03_BIRD_sites.pdf",
  expl_plot3,
  width = PDF_width,
  height = PDF_height,
  units = "in")

(expl_plot4<-
    dataset_catex%>% 
    ggplot(
      aes(
        x = Site,
        y = BirdProp,
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
      y = expression(paste("Proportion of caterpillars attacked by birds"))) +
    theme(
      text = element_text(size = text_size),
      legend.position = "top"))

ggsave(
  "figures/explor_plot_04_BIRD_sites_Strata.pdf",
  expl_plot4,
  width = PDF_width,
  height = PDF_height,
  units = "in")


#----------------------------------------------------------#
# 4.1 Model build for bird predation  -----
#----------------------------------------------------------#

glmm_bird_predation_full <- glmer(cbind(Bird72, Survived72H)~poly(Lat,2)*Strata + (1|Species),
                                   data = dataset_catex, family = "binomial")
glmm_bird_predation_module <- glmer(cbind(Bird72, Survived72H)~poly(abs(Lat),2)*Strata + (1|Species),
                                     data = dataset_catex, family = "binomial")
glmm_bird_predation_noStrata <- glmer(cbind(Bird72, Survived72H)~poly(Lat,2) + (1|Species),
                                       data = dataset_catex, family = "binomial")
glmm_bird_predation_linear <- glmer(cbind(Bird72, Survived72H)~poly(abs(Lat),1)*Strata + (1|Species),
                                     data = dataset_catex, family = "binomial")
glmm_bird_predation_full_add <- glmer(cbind(Bird72, Survived72H)~poly(Lat,2)+Strata + (1|Species),
                                       data = dataset_catex, family = "binomial")
glmm_bird_predation_linear_add <- glmer(cbind(Bird72, Survived72H)~poly(abs(Lat),1)+Strata + (1|Species),
                                         data = dataset_catex, family = "binomial")
glmm_bird_predation_Strata <- glmer(cbind(Bird72, Survived72H)~Strata + (1|Species),
                                     data = dataset_catex, family = "binomial")
glmm_bird_predation_null <- glmer(cbind(Bird72, Survived72H)~1 + (1|Species),
                                   data = dataset_catex, family = "binomial")
AICctab(glmm_bird_predation_full, glmm_bird_predation_module, glmm_bird_predation_noStrata, glmm_bird_predation_linear,
        glmm_bird_predation_full_add, glmm_bird_predation_linear_add, glmm_bird_predation_Strata, glmm_bird_predation_null)


# build the best model
glm_bird_predation_select<-glmm_bird_predation_full


## Predict the values
newDataBird <- data.frame(Lat = rep(seq(from = -40, to = 55, length.out = 500),2),
                      Strata = rep(c("U", "C"), each = 500))

newDataBird$BirdPredation <- predict(glm_bird_predation_select, newdata = newDataBird, re.form = NA, type = "response")

model_plot_01 <-plot(dataset_catex$Bird72/(dataset_catex$Bird72 + dataset_catex$Survived72H) ~ 
                       jitter(dataset_catex$Lat), col = c("deepskyblue3", "goldenrod3")[as.numeric(as.factor(dataset_catex$Strata))])
lines(newDataBird$Lat[newDataBird$Strata == "U"], 
      newDataBird$BirdPredation[newDataBird$Strata == "U"], col = "goldenrod3")
lines(newDataBird$Lat[newDataBird$Strata == "C"], 
      newDataBird$BirdPredation[newDataBird$Strata == "C"], col = "deepskyblue3")

newDataBird %>% 
  as_tibble() %>% 
  write_csv("data/output/OK_prediction_latfull_bird.csv")

#----------------------------------------------------------#
# 4.2 Figure from model draw -----
#----------------------------------------------------------#

(model_plot_02 <- ggplot(dataset_catex,
                         aes(
                           x=Lat,
                           y = BirdProp,
                           col = Strata,
                           fill=Strata,
                           size = 3)) +
   
   geom_point(
     data = dataset_catex,
     aes(y = BirdProp),
     size = 3,
     position = position_jitterdodge(
       dodge.width = 2,
       jitter.width = 2)) +
   
   geom_line(data = newDataBird, aes(y = BirdPredation), size = 2) +
   
   coord_flip()  +
   
   labs(
     x = "Latitude",
     y = expression(paste("Proportion attacked by birds")) )+
   scale_fill_manual(values = c("#42adc7", "#ffb902"))+
   scale_color_manual(values = c("#42adc7", "#ffb902"))+
   theme(
     text = element_text(size = text_size),
     legend.position = "right")) +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"))

ggsave(
  "figures/OK_model_plot_02_BirdPredations.pdf",
  model_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")



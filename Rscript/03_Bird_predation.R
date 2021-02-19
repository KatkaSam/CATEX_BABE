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

source("RScript/01_Config_DataLoad.R")

#----------------------------------------------------------#
# 4. Exploratory graphs for bird predation -----
#----------------------------------------------------------#

# see data
summary(dataset_catex)

# prepare proportional data for graphs
dataset_catex$PropBirdPred<-dataset_catex$Bird72/(dataset_catex$NonLost72H - dataset_catex$Arth72 - dataset_catex$Mam72)
is.numeric(dataset_catex$PropBirdPred) 

# NAs were generated in lines where all caterpillars were eaten by arthropods, thus the NAs were re-coded as 0
dataset_catex <- dataset_catex %>%
  mutate(PropBirdPred = coalesce(PropBirdPred, 0))
summary(dataset_catex)

(expl_plot3<-
  dataset_catex%>% 
  ggplot(
         aes(
           x = Site,
          y = PropBirdPred)) +
  
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
        y = PropBirdPred,
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


glmm_bird_predation_full <- glmer(cbind(Bird72, Survived72H)~poly(Lat,2)*Strata + (1|Sp2),
                                   data = dataset_catex, family = "binomial", 
                                   na.action = "na.fail")
glmm_bird_predation_module <- glmer(cbind(Bird72, Survived72H)~poly(abs(Lat),2)*Strata + (1|Sp2),
                                     data = dataset_catex, family = "binomial", 
                                     na.action = "na.fail")
glmm_bird_predation_noStrata <- glmer(cbind(Bird72, Survived72H)~poly(Lat,2) + (1|Sp2),
                                       data = dataset_catex, family = "binomial", 
                                       na.action = "na.fail")
glmm_bird_predation_linear <- glmer(cbind(Bird72, Survived72H)~poly(abs(Lat),1)*Strata + (1|Sp2),
                                     data = dataset_catex, family = "binomial", 
                                     na.action = "na.fail")
glmm_bird_predation_full_add <- glmer(cbind(Bird72, Survived72H)~poly(Lat,2)+Strata + (1|Sp2),
                                       data = dataset_catex, family = "binomial", 
                                       na.action = "na.fail")
glmm_bird_predation_linear_add <- glmer(cbind(Bird72, Survived72H)~poly(abs(Lat),1)+Strata + (1|Sp2),
                                         data = dataset_catex, family = "binomial", 
                                         na.action = "na.fail")
glmm_bird_predation_Strata <- glmer(cbind(Bird72, Survived72H)~Strata + (1|Sp2),
                                     data = dataset_catex, family = "binomial", 
                                     na.action = "na.fail")
glmm_bird_predation_null <- glmer(cbind(Bird72, Survived72H)~1 + (1|Sp2),
                                   data = dataset_catex, family = "binomial", 
                                   na.action = "na.fail")
AICctab(glmm_bird_predation_full, glmm_bird_predation_module, glmm_bird_predation_noStrata, glmm_bird_predation_linear,
        glmm_bird_predation_full_add, glmm_bird_predation_linear_add, glmm_bird_predation_Strata, glmm_bird_predation_null)

# compute all posible combinations
glm_bird_predation_dd <- 
  MuMIn::dredge(
    glm_bird_predation_full,
    trace = T)

# save result table
glm_bird_predation_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/bird_predation_model_result.csv")

# observe the best model
glm_bird_predation_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()

# build the best model
glm_bird_predation_select<-glm(cbind(Bird72, Survived72H)~Site+Strata+Site:Strata,
                          data = dataset_catex, family = "binomial", 
                          na.action = "na.fail")

summary(glm_bird_predation_select)
check_model(glm_bird_predation_select)
model_performance(glm_bird_predation_select)
check_heteroscedasticity(glm_bird_predation_select)
qplot(residuals(glm_bird_predation_select))

# calculate emmeans
glm_bird_predation_emmeans <-
  emmeans(
    glm_bird_predation_select,
    pairwise ~ Strata*Site,
    type = "response")
plot(glm_bird_predation_emmeans)
#NOTE - as there was no predation by bird in the LAK understory, it is doing weird things there

#----------------------------------------------------------#
# 4.2 Figure from model for bird predation draw -----
#----------------------------------------------------------#

(model_plot_02 <- 
   glm_bird_predation_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x=Site,
        y = prob,
        col = Strata,
        fill=Strata)) + 
  
  scale_x_discrete(limits=c("TOM", "LAK", "BUB", "DRO", "KAK", "EUC")) +
   
  geom_point(
      data = dataset_catex,
      aes(y = PropBirdPred),
      alpha = 0.5,
      size = 2,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  asymp.LCL,
        ymax = asymp.UCL),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 2)+
    
    geom_point(
      shape = 0,
      position = position_dodge(width = 0.5),
      size = 3) +
    
   labs(
     x = "Site",
     y = expression(paste("Proportion of caterpillars attacked by birds")) )+
   scale_fill_manual(values = c("deepskyblue3", "goldenrod3"))+
   scale_color_manual(values = c("deepskyblue3", "goldenrod3"))+
   theme(
     text = element_text(size = text_size),
     legend.position = "right")) +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"))
#NOTE - as there was no predation by bird in the LAK understory, it is doing weird things there

# to turn and rescale the figure 
model_plot_02<-model_plot_02 + coord_flip() +
  scale_x_discrete(limits=c("EUC", "DRO", "KAK",  "BUB", "LAK","TOM"))
#NOTE not flipping anymore because: Coordinate system already present. Adding new coordinate system, which will replace the existing one.

# save pdf
ggsave(
  "figures/model_plot_02_Bird_Predation.pdf",
  model_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# save the pairwise test 
glm_bird_predation_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/bird_predation_pairwise_contrast.csv")

glm_bird_predation_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/bird_predation_pairwise_emmeans.csv")



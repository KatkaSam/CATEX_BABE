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

source("RScript/01_Config_DataLoad.R")

#----------------------------------------------------------#
# 5. Exploratory graphs for arthropod predation -----
#----------------------------------------------------------#

# see data
summary(dataset_catex)

# prepare proportional data for graphs
dataset_catex$PropArthPred<-dataset_catex$Arth72/(dataset_catex$NonLost72H - dataset_catex$Bird72 - dataset_catex$Mam72)
is.numeric(dataset_catex$PropArthPred) 
# NAs were not generated so all is ok

(expl_plot5<-
  dataset_catex%>% 
  ggplot(
         aes(
           x = Site,
          y = PropArthPred)) +
  
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
        y = PropArthPred,
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

glm_arthropod_predation_full <- glm(cbind(Arth72, Survived72H)~Site*Strata,
                  data = dataset_catex, family = "binomial", 
                  na.action = "na.fail")

# compute all posible combinations
glm_arthropod_predation_dd <- 
  MuMIn::dredge(
    glm_arthropod_predation_full,
    trace = T)

# save result table
glm_arthropod_predation_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/arthropod_predation_model_result.csv")

# observe the best model
glm_arthropod_predation_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()

# build the best model
glm_arthropod_predation_select<-glm(cbind(Arth72, Survived72H)~Site+Strata+Site:Strata,
                          data = dataset_catex, family = "binomial", 
                          na.action = "na.fail")

summary(glm_arthropod_predation_select)
check_model(glm_arthropod_predation_select, binwidth=10)
model_performance(glm_arthropod_predation_select)
check_heteroscedasticity(glm_arthropod_predation_select)
qplot(residuals(glm_arthropod_predation_select))

# calculate emmeans
glm_arthropod_predation_emmeans <-
  emmeans(
    glm_arthropod_predation_select,
    pairwise ~ Strata*Site,
    type = "response")
plot(glm_arthropod_predation_emmeans)

#----------------------------------------------------------#
# 5.2 Figure from model for arthropod predation draw -----
#----------------------------------------------------------#

(model_plot_03 <- 
   glm_arthropod_predation_emmeans$emmeans %>% 
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
      aes(y = PropArthPred),
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
     y = expression(paste("Proportion of caterpillars attacked by arthropods")) )+
   scale_fill_manual(values = c("deepskyblue3", "goldenrod3"))+
   scale_color_manual(values = c("deepskyblue3", "goldenrod3"))+
   theme(
     text = element_text(size = text_size),
     legend.position = "right")) +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"))

model_plot_03<-model_plot_03 + coord_flip() +
  scale_x_discrete(limits=c("EUC", "DRO", "KAK",  "BUB", "LAK","TOM")) 


# save pdf
ggsave(
  "figures/model_plot_03_Arthropod_predation.pdf",
  model_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# save the pairwise test 
glm_arthropod_predation_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/arthropod_predation_pairwise_contrast.csv")

glm_arthropod_predation_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/arthropod_predation_pairwise_emmeans.csv")



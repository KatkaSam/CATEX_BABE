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

source("RScript/01_Config_DataLoad.R")

#----------------------------------------------------------#
# 3. Exploratory graphs  -----
#----------------------------------------------------------#

# see data
summary(dataset_catex)

# prepare proportional data fro graphs
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
    y = expression(paste("Total Predation/Exposed"))) +
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
      y = expression(paste("Total Predation/Exposed"))) +
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

#----------------------------------------------------------#
# 3.1 Model build -----
#----------------------------------------------------------#


glm_total_predation_full <- glm(cbind(TotalPred72H, Survived72H)~Site*Strata,
                  data = dataset_catex, family = "binomial", 
                  na.action = "na.fail")

# compute all posible combinations
glm_total_predation_dd <- 
  MuMIn::dredge(
    glm_total_predation_full,
    trace = T)

# save result table
glm_total_predation_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/total_predation_model_result.csv")

# observe the best model
glm_total_predation_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()

# build the best model
glm_predation_select<-glm(cbind(TotalPred72H, Survived72H)~Site+Strata+Site:Strata,
                          data = dataset_catex, family = "binomial", 
                          na.action = "na.fail")

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
#NOTE: A nesting structure was detected in the fitted model: Strata %in% Site
plot(glm_predation_emmeans)

#----------------------------------------------------------#
# 3.2 Figure from model draw -----
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
  
  scale_x_discrete(limits=c("TOM", "LAK", "BUB", "DRO", "KAK", "EUC")) +
   
  geom_point(
      data = dataset_catex,
      aes(y = PropTotPred),
      alpha = 0.5,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  asymp.LCL,
        ymax = asymp.UCL),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      position = position_dodge(width = 0.5),
      size = 3) +
    
    labs(
      x = "Site",
      y = expression(paste("Total predation (proportion)")) )+
   scale_fill_manual(values = pallete_1)+
   scale_color_manual(values = pallete_1)+
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

# save pdf
ggsave(
  "figures/model_plot_01_Total_predation.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# save the pairwise test 
glm_predation_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/total_predation_pairwise_contrast.csv")

glm_predation_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/total_predation_pairwise_emmeans.csv")
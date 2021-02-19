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

#----------------------------------------------------------#
# 3.1 Model build -----
#----------------------------------------------------------#


glmm_total_predation_full <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2)*Strata + (1|Sp2),
                  data = dataset_catex, family = "binomial", 
                  na.action = "na.fail")
glmm_total_predation_module <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),2)*Strata + (1|Sp2),
                                   data = dataset_catex, family = "binomial", 
                                   na.action = "na.fail")
glmm_total_predation_noStrata <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2) + (1|Sp2),
                                   data = dataset_catex, family = "binomial", 
                                   na.action = "na.fail")
glmm_total_predation_linear <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),1)*Strata + (1|Sp2),
                                     data = dataset_catex, family = "binomial", 
                                     na.action = "na.fail")
glmm_total_predation_full_add <- glmer(cbind(TotalPred72H, Survived72H)~poly(Lat,2)+Strata + (1|Sp2),
                                   data = dataset_catex, family = "binomial", 
                                   na.action = "na.fail")
glmm_total_predation_linear_add <- glmer(cbind(TotalPred72H, Survived72H)~poly(abs(Lat),1)+Strata + (1|Sp2),
                                     data = dataset_catex, family = "binomial", 
                                     na.action = "na.fail")
glmm_total_predation_Strata <- glmer(cbind(TotalPred72H, Survived72H)~Strata + (1|Sp2),
                                         data = dataset_catex, family = "binomial", 
                                         na.action = "na.fail")
glmm_total_predation_null <- glmer(cbind(TotalPred72H, Survived72H)~1 + (1|Sp2),
                                   data = dataset_catex, family = "binomial", 
                                   na.action = "na.fail")
AICctab(glmm_total_predation_full, glmm_total_predation_module, glmm_total_predation_noStrata, glmm_total_predation_linear,
        glmm_total_predation_full_add, glmm_total_predation_linear_add, glmm_total_predation_Strata, glmm_total_predation_null)

# compute all posible combinations
glmm_total_predation_dd <- 
  MuMIn::dredge(
    glmm_total_predation_full,
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
    pairwise ~ Lat*Site,
    type = "response")

plot(glm_predation_emmeans)

## Predict
newData <- data.frame(Lat = rep(seq(from = -90, to = 90, length.out = 500),2),
                      Strata = rep(c("U", "C"), each = 500))

newData$Predation <- predict(glm_predation_select, newdata = newData, re.form = NA, type = "response")

plot(dataset_catex$TotalPred72H/(dataset_catex$TotalPred72H + dataset_catex$Survived72H) ~ 
       jitter(dataset_catex$Lat), col = c("deepskyblue3", "goldenrod3")[as.numeric(as.factor(dataset_catex$Strata))])
lines(newData$Lat[newData$Strata == "U"], newData$Predation[newData$Strata == "U"], col = "goldenrod3")
lines(newData$Lat[newData$Strata == "C"], newData$Predation[newData$Strata == "C"], col = "deepskyblue3")
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

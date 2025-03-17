#----------------------------------------------------------#
# 1. Import data -----
#----------------------------------------------------------#

# 1.1 LAI data import -----
SitesLAI <-  
  readxl::read_xlsx("data/input/CATEX_LAI.xlsx")
summary(SitesLAI)

#----------------------------------------------------------#
# 3.1 Model build -----
#----------------------------------------------------------#

LAI_predation_full <- lm(Corrected_LAI_HSA~poly(Latitude,2)*Strata,
                                   data = SitesLAI)
LAI_predation_module <- lm(Corrected_LAI_HSA~poly(abs(Latitude),2)*Strata,
                                  data = SitesLAI)
LAI_predation_noStrata <- lm(Corrected_LAI_HSA~poly(Latitude,2),
                                       data = SitesLAI)                             
LAI_predation_linear <- lm(Corrected_LAI_HSA~poly(abs(Latitude),1)*Strata,
                                     data = SitesLAI)
LAI_predation_full_add <- lm(Corrected_LAI_HSA~poly(abs(Latitude),2)+Strata,
                                       data = SitesLAI)
LAI_predation_linear_add <- lm(Corrected_LAI_HSA~poly(abs(Latitude),1)+Strata,
                                         data = SitesLAI)
LAI_predation_Strata <- lm(Corrected_LAI_HSA~Strata,
                                     data = SitesLAI)
LAI_predation_null <- lm(Corrected_LAI_HSA~1,
                                   data = SitesLAI)
AICctab(LAI_predation_full, LAI_predation_module, LAI_predation_noStrata, LAI_predation_linear,
        LAI_predation_full_add, LAI_predation_linear_add, LAI_predation_Strata, LAI_predation_null)

# build the best model
glm_predation_select<-LAI_predation_full


## Predict the values
newData <- data.frame(Latitude = rep(seq(from = -40, to = 55, length.out = 500),2),
                      Strata = rep(c("Overstory", "Understory"), each = 500))

newData$LAI <- predict(glm_predation_select, newdata = newData, re.form = NA, type = "response")

model_plot_01 <-plot(SitesLAI$Corrected_LAI_HSA ~ 
                       jitter(SitesLAI$Latitude), col = c("goldenrod3", "deepskyblue3")[as.numeric(as.factor(SitesLAI$Strata))])
lines(newData$Lat[newData$Strata == "Understory"], 
      newData$LAI[newData$Strata == "Understory"], col = "goldenrod3")
lines(newData$Lat[newData$Strata == "Overstory"], 
      newData$LAI[newData$Strata == "Overstory"], col = "deepskyblue3")

newData %>% 
  as_tibble() %>% 
  write_csv("data/output/prediction_LAI.csv")

#----------------------------------------------------------#
# 3.2 Figure from model draw -----
#----------------------------------------------------------#
(model_plot_01 <- ggplot(SitesLAI,
                         aes(
                           x=Latitude,
                           y = Corrected_LAI_HSA,
                           col = Strata,
                           fill=Strata,
                           size = 3)) +
    
    geom_point(
      data = SitesLAI,
      aes(y = Corrected_LAI_HSA),
      size = 3, alpha = 0.5, 
      position = position_jitterdodge(
        dodge.width = 2,
        jitter.width = 2)) +
    
    geom_line(data = newData, aes(y = LAI), size = 2) +
    
       coord_flip()  +
    
    labs(
      x = "Latitude",
      y = expression(paste("Corrected LAI")) )+
    scale_fill_manual(values = c("#42adc7", "#ffb902"))+
    scale_color_manual(values = c("#42adc7", "#ffb902"))+
    theme(
      text = element_text(size = text_size),
      legend.position = "right")) +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"))

ggsave(
  "figures/OK_model_plot_01_LAI.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")


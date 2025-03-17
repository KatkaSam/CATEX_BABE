data<-read.delim2("clipboard") # read the data from the predicted values of the models for total predation, for bird predation or from the arthropod predation
summary(data)
data$Strata<-as.factor(data$Strata)
data$Predation<-as.numeric(data$Predation)

tapply(data$Predation, data$Strata, summary)  
sd(data$Predation) # standard deviation

library(dplyr)
df <- data %>% 
  group_by(Strata) %>% 
  summarise(mean = mean(Predation),
            std = sd(Predation))
df

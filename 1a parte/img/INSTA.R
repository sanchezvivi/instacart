# bibli -------------------------------------------------------------------

library(tidyverse)
library(plyr)
library(skimr)
library(inspectdf)
library(ggplot2)
library(ggpubr)
library(plotly)

# data --------------------------------------------------------------------
setwd("C:/Users/leona/Documents/GitHub/instacart/data")
data <- read.csv(file = 'orders.csv')

mean_interval <- data %>% 
  filter(!is.na(days_since_prior_order)) %>% 
  filter(days_since_prior_order<30) %>% 
  mutate(freq = n()) %>% 
  group_by(user_id) %>% 
  summarise(n = n(),mean = mean(days_since_prior_order)) 

mean_interval$freq_mes <- round(30/mean_interval$mean,0)

mean_interval <- mean_interval[mean_interval$freq_mes<=15,]

c4 <- c(5,6)
mean_interval$freq_mes[mean_interval$freq_mes %in% c4] <-  4
c8 <- c(7,9)
mean_interval$freq_mes[mean_interval$freq_mes %in% c8] <-  8
c12 <- seq(10,15)
mean_interval$freq_mes[mean_interval$freq_mes %in% c12] <-  12

freq <- mean_interval %>% 
  group_by(freq_mes) %>% 
  summarise(freq = n()/nrow(mean_interval))

  
  
  
freq %>% 
  ggplot(aes(x=as.factor(freq_mes),y=freq))+
  labs(x=" ",title ="Frequência mensal")+
  geom_col()


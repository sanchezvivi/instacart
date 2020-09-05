# conf. --------------
rm(list=ls())
#setwd("C:/Users/leona/OneDrive/Insper/AI/instacart")
options(warn=-1)

set.seed(1)

# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(formattable)
library(data.table)
library(ggplot2)
library(plotly)
library(ggiraphExtra)
library(patchwork)
library(knitr)
library(skimr)

library(extrafont)

#font_import()

theme_set(theme_minimal())
theme_update(text = element_text(family = "Brandon Text", size = 20),
             plot.title = element_text(face = "bold"),
             strip.text.x = element_text(size = 15))

source('../instacart_palette.R')

# Datas -------------------------------------------------------------------

# data <- read.csv("df_join_S2.csv")
# data$X <- NULL

## Sérgio -------
data_S2 <- read.csv("data_S2.csv")
data_S2$X <- NULL

data_S2 <- data_S2 %>% 
  rename("mD"="t_mean") %>% 
  rename("mP"="mean_prod_cart") %>% 
  rename("mREC"="mean_rec_fat") %>% 
  rename("mPC"="mean_peso_cart")
  
 
## Leo ---------

data_L <- read.csv("data_L.csv")
data_L$X <- NULL
data_L$x <- NULL
data_L$y <- NULL
data_L$coord <- NULL



# funções -----------
##  função out_not_inter(data,cl)----------------

out_not_inter <- function(data,clB,clR,corBOM,corRUIM,LouS,ls){
  mean_data <- data %>% 
    select(-user_id) %>% 
    group_by(cluster) %>% 
    summarise_if(is.numeric, mean)
  mean_data <- round(mean_data,digits=2)
  mean_data$cluster <- as.factor(mean_data$cluster)
  

########### radaar 
  nc <- corBOM
  c <- corRUIM
  menos <- color_tile(nc,c)
  mais <- color_tile(c,nc)
  
  cores <- c()
  n <- 1
  for( i in unique(mean_data$cluster)){
    if(as.numeric(i) != clR && as.numeric(i) != clB){
      cores[n] <- "grey"
    }
    else if (as.numeric(i) == clR){
      cores[n] <- c
    }
    else if (as.numeric(i) == clB){
      cores[n] <- nc
    }
    n=n+1
  }
  
  
  for (i in 2:5){
    mean_data[i] <- round(mean_data[i],digits = 0)
  }
  
  ggR <- ggRadar(data=mean_data
          ,aes(facet=cluster,
               color=cluster)
          ,size = 1.5
          ,legend.position = "none")+
    scale_colour_manual(values=cores)+
    scale_fill_manual(values=cores)+
    theme(axis.text.x = element_text(size = ls))
  
  print(ggR)

  ########tabela
  ####################### Sérgio -----------
  if (LouS == "S"){
 
  out <- formattable(mean_data,list(
    "n_compras" = mais,
    "mD"= menos,
    "mP" = mais,
    "mPC"= mais,
    "mREC"=mais))  
  }
  
  ################ Leo
  if (LouS == "L"){

  out <- formattable(mean_data,list(
    "alpha_P"= mais,
    "inter_P"= mais,
    "alpha_D"= menos,
    "inter_D"= menos)) 
  }
  
  out
}
  

############  função data_inter(dataL,dataS,clL,clS)--------------------------

data_inter<- function(data_L,data_S,cl_L,cl_S)
{
  uL <- data_L[data_L$cluster==cl_L,]
  uS <- data_S[data_S$cluster == cl_S,]
  uLS <- intersect(uL$user_id,uS$user_id)
  
  data_Lint <- data_L[data_L$user_id %in% uLS,]
  data_Sint <- data_S[data_S$user_id %in% uLS,]
  
  data_int <- merge(data_Lint,data_Sint,by="user_id")
  
  data_int$cluster.x <- NULL
  data_int$cluster.y <- NULL
  data_int$user_id <- paste(cl_L,"_",cl_S)
  data_int <- data_int %>% 
    rename("cluster"="user_id") 
  return(data_int)

}



data_bom_L3S5 <- data_inter(data_L,data_S2,3,5)
data_ruim_L4S7 <- data_inter(data_L,data_S2,4,7)

data_L3S5_L4S7 <- rbind(data_bom_L3S5,data_ruim_L4S7)

mean_3547 <- data_L3S5_L4S7 %>% 
  group_by(cluster) %>% 
  summarise_if(is.numeric, mean)

for(i in 2:9){
  mean_3547 [i] <- round(mean_3547 [i],digits = 0)
}
mean_3547 <-mean_3547[c(
  "cluster",
  "n_compras",
  "mPC" ,
  "mREC",
  "inter_P",
  "alpha_P" ,
  "mP",
  "inter_D",
  "alpha_D" ,
  "mD")]





 
################ função boxplot_graph ------------
boxplot_graph <- function(data,vars){

if (vars != "S"  && vars !=0) {
data <- pivot_longer(data ,
                                 cols=ends_with(vars),
                                 values_to = "values",
                                 names_to="names")
}
  
  if (vars == "S"){
    data <- data[c("cluster","n_compras","mPC","mREC" )]
    data <- pivot_longer(data ,
                         cols=n_compras:mREC,
                         values_to = "values",
                         names_to="names")
  }
 
  if (vars==0){
    data <- pivot_longer(data ,
                         cols=alpha_P:mREC,
                         values_to = "values",
                         names_to="names")
  } 
bp <- data %>% 
  ggplot( aes(x=names, y=values
              , fill=cluster
              ))+
  geom_boxplot(outlier.shape = NA)+
  scale_fill_manual(values=c("orange", "green"))+
  scale_y_continuous(trans = scales::pseudo_log_trans())

print(bp)

}


### Funçao tabela final-------------
tabela_comp <- function(data,vars,digits){
  
  nc <- "green"
  c <- "orange"
  menos <- color_tile(nc,c)
  mais <- color_tile(c,nc)
  
  if (vars=="P"){
    data <- data[c("cluster","inter_P","mP","alpha_P")]
  }
  
  if (vars=="D"){
    data <- data[c("cluster","inter_D","mD","alpha_D")]
  }
  
  if (vars=="S"){
    data <- data[c("cluster","n_compras","mPC","mREC" )]
  }
  
  if(vars==0){
    data <- data
  }

  mean_data <- data %>%
    group_by(cluster) %>%
    summarise_if(is.numeric, mean)
  
  

  
  if (vars=="P"){
    for (i in 2:ncol(mean_data)){
      mean_data[i] <- round(mean_data[i],digits=digits)
    }
    mean_data <- formattable(mean_data,
                             list(
                               "inter_P" =mais
                               ,"mP"=mais
                               ,"alpha_P"=mais
                             )
                             )
  }
  
  if (vars=="D"){
    for (i in 2:ncol(mean_data)){
      mean_data[i] <- round(mean_data[i],digits=digits)
    }
    mean_data <- formattable(mean_data,
                             list(
                               "inter_D" =menos
                               ,"mD"=menos
                               ,"alpha_D"=menos
                             )
    )
  }
  
  if(vars=="S"){
    for (i in 2:(ncol(mean_data)-1)){
      mean_data[i] <- round(mean_data[i],digits=digits)
    }
    mean_data <- formattable(mean_data,
                             list(
                               "n_compras" =mais
                               ,"mPC" =mais
                               ,"mREC"  =mais
                             )
    )
    mean_data$mREC <- round(mean_data$mREC,digits = 2)
  }

  if(vars==0){
    for (i in 2:(ncol(mean_data)-1)){
      mean_data[i] <- round(mean_data[i],digits=digits)
    }
    mean_data <- formattable(mean_data,
                             list(
                               "inter_P" =mais
                               ,"mP"=mais
                               ,"alpha_P"=mais
                               ,"inter_D" =menos
                               ,"mD"=menos
                               ,"alpha_D"=menos
                               ,"n_compras" =mais
                               ,"mPC" =mais
                               ,"mREC"  =mais
                             )
    )
    mean_data$mREC <- round(mean_data$mREC,digits = 2)
  } 
  
mean_data
}


# FINALLLLLLLLLLLLLLLLLLLLLL ----------------------------------------------

# SEPARADOS
out_not_inter(data_L,4,3,"green","orange","L",9) ##OK
out_not_inter(data_S2,7,5,"green","orange","S",7) ##OK

# INTERSECÇÃO 

### Todas variáveis
boxplot_graph(data_L3S5_L4S7,0)  ######ok
tabela_comp(data_L3S5_L4S7,0,0)  ######ok

## Produtos
boxplot_graph(data_L3S5_L4S7,"P") ######ok
tabela_comp(data_L3S5_L4S7,"P",0) ######ok

## Tempo
boxplot_graph(data_L3S5_L4S7,"D") ######ok
tabela_comp(data_L3S5_L4S7,"D",0) ######ok

## Sérgio
boxplot_graph(data_L3S5_L4S7,"S") ######ok
tabela_comp(data_L3S5_L4S7,"S",2) ######ok



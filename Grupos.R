# configurações gerais ----------------------------------------------------

rm(list=ls())
setwd("C:/Users/leona/OneDrive/Insper/AI/instacart")

# bibliotecas ---------------------------------------------------------------

library(tidyverse)

library(ggplot2)
library(plotly)
library('patchwork')

require(dplyr)
library(skimr)

library(nlme)

suppressWarnings(expr)
# data --------------------------------------------------------------------

nr <- 10^6
data <- read.csv("df_join.csv",nrows = nr)


data[c("add_to_cart_order",
           "reordered","eval_set","aisle_id","X")] <- NULL

names(data)[which(names(data)=="days_since_prior_order")] <- 
  "days"

data <- na.omit(data, cols=days)

remove(nr)


# Filtra do por número de compras----------------------------------------------------------------
x <- data %>% 
  group_by(user_id,order_id) %>% 
  summarise(n=n())

x <- data.frame(table(x$user_id))
x <- x[x$Freq>3,] ### Pontuar

data_freq <- data[data$user_id %in% x$Var1,]

remove(x)
# criando grupos  ---------------------------------------------------------
gb_days_n<- data_freq %>%
  group_by(user_id,order_id) %>%
  summarise(days=mean(days),produtos=n()) 


# days --------------------------------------------------------------------
inter <- c()
alpha <- c()
n=1
for (user in unique(gb_days_n$user_id)){
  a <- as.vector(gb_days_n[gb_days_n$user_id==as.integer(user),"days"])
  l <- seq(1,nrow(a))
  y <- data.frame(l,a)
  r <- lm(days~l, data=y)
  inter[n] <-  summary(r)$coefficients[1]
  alpha[n] <-  summary(r)$coefficients[2]
  n <- n+1
}

days_aplha_inter <- data.frame(unique(gb_days_n$user_id),alpha,inter)

days_aplha_inter <- days_aplha_inter%>% 
  rename(alpha_D = alpha,
         inter_D = inter)

remove(a,l,y,r,n,user,inter,alpha)

# quantidade de produtos --------------------------------------------------

inter <- c()
alpha <- c()
n=1
for (user in unique(gb_days_n$user_id)){
  a <- as.vector(gb_days_n[gb_days_n$user_id==as.integer(user),"produtos"])
  l <- seq(1,nrow(a))
  y <- data.frame(l,a)
  r <- lm(produtos~l, data=y)
  inter[n] <-  summary(r)$coefficients[1]
  alpha[n] <-  summary(r)$coefficients[2]
  n <- n+1
}

produtos_aplha_inter <- data.frame(unique(gb_days_n$user_id),
                                   alpha,inter)

produtos_aplha_inter <- produtos_aplha_inter %>% 
  rename(alpha_P=alpha,inter_P=inter)

remove(a,l,y,r,n,user,inter,alpha)

# join --------------------------------------------------------------------

data_join<-inner_join(produtos_aplha_inter,
                      days_aplha_inter,by="unique.gb_days_n.user_id.")

# kmenas error ------------------------------------------------------------

wss <- factoextra :: fviz_nbclust(scale(data_join) , 
                                  kmeans, 
                                  method = "wss")



res_silhouette <- silhouette(kmeans_final$cluster, euc_dist)
sil <- factoextra :: fviz_nbclust(scale(data_join),
                                  kmeans,
                                  method = "silhouette")

wss+sil
remove(wss,sil)

# plot kmeans -------------------------------------------------------------
kmean_plot <- function(df,ng){
  df <- column_to_rownames(df,"unique.gb_days_n.user_id.")
  df <- scale(df)
  km <-  kmeans(df, ng)
  clu <- factoextra ::fviz_cluster(km,df,
               repel=TRUE,
               geom="poimt",
               xlab=" ",
               ylab=" ",
               main=NULL,
               show.clust.cent = TRUE)
  return(clu)
}

cl2 <- kmean_plot(data_join,2)
cl3 <- kmean_plot(data_join,3)
cl4 <- kmean_plot(data_join,4)
cl5 <- kmean_plot(data_join,5)
cl6 <- kmean_plot(data_join,6)
cl7 <- kmean_plot(data_join,7)

cl2+cl3+cl4+cl5+cl6+cl7


# características clusters ------------------------------------------------
data_join <- data_join %>% 
  rename(name="unique.gb_days_n.user_id.")

## Função transformação base
cluter_data <- function (data_join,cl,name){
  cl <- cl$data
  df <- merge(cl, data_join, by="name")
  df <-  pivot_longer(df,alpha_P:inter_D,names_to = "names",
                      values_to="values")
  df <- df[5:7]
 
if (name != 0){ 
 df <- df %>% 
    filter(str_detect(names, name))
}
 return(df)
  }

## função gráfico
graph_cluster  <-function(data_join,cl,name,yI,yS){
  df <- cluter_data(data_join,cl,name)
  df %>% 
  ggplot(aes(x=names, y=values, fill=cluster)) +
  geom_boxplot(outlier.shape = NA)+
  ylim(yI,yS)
}

## gráficos (boxplot)
graph_cluster(data_join,cl2,"alpha",-5,5)
graph_cluster(data_join,cl3,"alpha",-5,5)
graph_cluster(data_join,cl4,"alpha",-5,5)
graph_cluster(data_join,cl5,"alpha",-5,5)
graph_cluster(data_join,cl6,"alpha",-5,5)
graph_cluster(data_join,cl7,"alpha",-5,5)


# médias, mediana, desvpad ---------------------------------------------------------
cluster_chars <- function(data_join,cl){
  df <- cluter_data(data_join,cl,0) 
  
  x <- df %>% 
    group_by(cluster,names) %>% 
    summarise(median=median(values),
              mean=mean(values),
              sd=sd(values),
              "cv(%)"=(sd(values)/mean(values)))
  x <- as.data.frame(x)
  x <-   format(x, digits = 3)
  return(x)
}

y <- cluster_chars(data_join,cl4)

view(y)

y <- pivot_longer(y,median:sd,names_to =" names",
                  values_to = "values")

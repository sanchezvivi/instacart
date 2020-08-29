# configurações gerais ----------------------------------------------------

rm(list=ls())
setwd("C:/Users/leona/OneDrive/Insper/AI/instacart")

# bibliotecas ---------------------------------------------------------------

library(tidyverse)
library(data.table)
library(formattable)

library(ggplot2)
library(plotly)
library('patchwork')
library(fmsb)
library(viridis)

require(dplyr)
library(skimr)

library(nlme)

suppressWarnings(expr)
# data --------------------------------------------------------------------

# data_raw <- read.csv("df_join.csv")
# nr_data <- nrow(data_raw)
# rows <- sample(1:nr_data,nr,replace=FALSE)
# data <- data_raw[rows,]
# set.seed(1)

nr <- 10^6
data<- read.csv("df_join.csv",nrow=nr)



data[c("add_to_cart_order",
           "reordered","eval_set","aisle_id","X")] <- NULL

names(data)[which(names(data)=="days_since_prior_order")] <- 
  "days"

data <- na.omit(data, cols=days) ## tirar prirmeira compra


# Filtra do por número de compras----------------------------------------------------------------
x <- data %>% 
  group_by(user_id,order_id) %>% 
  summarise(n=n())

x <- data.frame(table(x$user_id))
x <- x[x$Freq>4,] ### Pontuar

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

data_join <- data_join %>% 
  rename(name="unique.gb_days_n.user_id.")

data_join  <- data.frame(data_join , row.names = 1)
# kmenas error ------------------------------------------------------------

# funciona até 10^6 linhas
if (nr <= 10^6){
  wss <- factoextra :: fviz_nbclust(scale(data_join) ,
                        kmeans,
                        method = "wss")
  
  
  
  
  sil <- factoextra :: fviz_nbclust(scale(data_join),
                      kmeans,
                      method = "silhouette")
  
  wss+sil
}
remove(wss,sil)

if (nr > 10^6){
nk <- 6
totwithinss <- c()
betweenss <- c()

for (i in 1:nk){
  x<- kmeans(scale(data_join),i+1)
  totwithinss[i]  <-x$tot.withinss
  betweenss[i]  <- x$betweenss
}

kmeans_error <- data.frame(clusters=seq(2,nk+1),totwithinss,betweenss)

kmeans_error <- pivot_longer(kmeans_error,colnames(kmeans_error)[2:3],
                             names_to ="error" ,values_to = "value")

ggplot(kmeans_error, aes(x = clusters, y = value)) +
  geom_smooth(aes(color = error, fill = error),size=1)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,nk+1))
}

# plot kmeans -------------------------------------------------------------
kmean_plot <- function(df,ng){
  df <- scale(df)
  km <-  kmeans(df, ng)
  clu <- factoextra ::fviz_cluster(km,df,
               repel=TRUE,
               geom="point",
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


## Função transformação base
cluster_data <- function (data_join,cl,name){
  data_join$name <- rownames(data_join)
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
  df <- cluster_data(data_join,cl,name)
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
  df <- cluster_data(data_join,cl,0) 
  
  x <- df %>% 
    group_by(cluster,names) %>% 
    summarise(median=median(values)
              ,mean=mean(values)
              ,sd=sd(values)
              #,"cv(%)"=100*(sd(values)/mean(values))
              )
  x <- as.data.frame(x)
  x <-   format(x, digits = 3)
  return(x)
}

# Funções r -------------------------------------------------------------------

## data_radar --------------------------------------------------------------

## var: mean, median, sd
data_radar <- function(data_join,cl,var){
  
  ### preparação da base
  y <- cluster_chars(data_join,cl)
  
  y <- y[,c("cluster","names",var)]

  
  data_radar <- pivot_wider(y,names_from = names,
                            values_from =var )
  
  data_radar <- column_to_rownames(data_radar,
                                   var="cluster")
  
  for (i in 1:4){
    data_radar[i] <- as.numeric(unlist(data_radar[i]))
  }
  
  data_radar <- scale(data_radar)
  
  data_radar <- rbind(rep(max(data_radar),ncol(data_radar)) , 
                      rep(min(data_radar),ncol(data_radar)) , data_radar)
  
  ncl <- (nrow(data_radar)-2)
  r <- c("max","min",seq(1,ncl))
  row.names(data_radar) <- r
  
  data_radar <- data.frame(data_radar )
  
  return(data_radar)
}

## graph_radar -------------------------------------------------------------


graph_radar <- function(data_join,cl,var){
  data_radar <- data_radar(data_join,cl,var)
  ncl <- (nrow(data_radar)-2)
  x <- 3
  ####### cores
  colors_border <- c()
  for (i in 3:nrow(data_radar )){
    colors_border[i-2] <- alpha(i*x,0.75)
  }
  
  colors_in <- c()
  for (i in 3:nrow(data_radar)){
    colors_in[i-2] <- alpha(i*x,0.5)
  }
  
  # plot with default options:
  radarchart( data_radar
              ,axistype=4 ## onde números aparecem
              #custom polygon
              ,pcol=colors_border ## cor dos pontos
              ,pfcol=colors_in ## fill
              ,plwd=.1 #grossura linha
              ,plty=1 #tipo de linha do grupo
              #custom the grid
              ,cglcol="darkgrey"#color of the net
              , cglty=1 #net line type
              , axislabcol="black" #color of axis labels
              ,caxislabels=seq(-2,2,5) #vector of axis labels to display
              #custom labels
              ,vlcex=0.8 #group labels size
              ,title=paste("Radar chart (",var, "normalized) :\n "
                           ,ncl, " clusters"))
  
  legend(
    x=1.5, y=1
    ,legend = rownames(data_radar[-c(1,2),])
    ,bty = "n" #type of box ("o" ou "n")
    ,pch=20  #símbolo legenda
    ,col=colors_in 
    ,text.col = "black"
    ,pt.cex=2.5 #size point
  )
  }



## gráfico de colunas (data_radar) -----------------------------------------
graph_col  <- function(data_join,cl){
  data_radar <- cluster_chars(data_join,cl)
 
  for (i in 3:5){
    data_radar[i] <- as.numeric(unlist(data_radar[i]))
  }

  data_radar <- data_radar %>% 
    select(-median)
  
  data_radar <- pivot_longer(data_radar,
                     names(data_radar)[3:ncol(data_radar)],
                     names_to="name",
                    values_to = "values")
  
  
  data_radar%>%  
    ggplot(aes(x=as.factor(name)
               ,y=as.numeric(values)
               ,fill=as.factor(names)))+
    geom_col(position ="dodge")+
    facet_wrap(~cluster, ncol =7)

}

## tabela comparativa ------------------------------------------------------
cluster_table <- function(data_join,cl,var){

  data <- cluster_chars(data_join,cl)
  
  
  for (i in 3:5){
    data[i] <- round(as.numeric(unlist(data[i])),2)
  }
  
  data <- data[c("cluster","names",var)]
  
  if (var == "mean"){
  data<- data %>% 
    pivot_wider(names_from = names, 
                values_from = mean)
  }
  
  if (var == "median"){
    data<- data %>% 
      pivot_wider(names_from = names, 
                  values_from = median)
  }

  if (var == "sd"){
    data<- data %>% 
      pivot_wider(names_from = names, 
                  values_from = sd)
  }

    colnames(data)[1] <- paste(
      "cluster (",var,")")
    
  formattable(data, lapply(1:nrow(data), function(row) {
    area(row) ~ color_tile("white", "blue")}))
  
  c0D <- "green"
  cfD<- "orange"
  
  c0P <- "orange"
  cfP <- "green"
  formattable(data,list(
    alpha_D= color_tile(c0D, cfD),
    alpha_P= color_tile(c0P, cfP),
    inter_D= color_tile(c0D, cfD),
    inter_P= color_tile(c0P, cfP)
  )) 
  

}


# Gráficos de radar -------------------------------------------------------

graph_radar(data_join,cl2,"mean")
graph_radar(data_join,cl3,"mean")
graph_radar(data_join,cl4,"mean")
graph_radar(data_join,cl5,"mean")
graph_radar(data_join,cl6,"mean")
graph_radar(data_join,cl7,"mean")


# Gráficos de coluna ------------------------------------------------------

graph_col(data_join,cl2)
graph_col(data_join,cl3)
graph_col(data_join,cl4)
graph_col(data_join,cl5)
graph_col(data_join,cl6)
graph_col(data_join,cl7)







# tabelas comparativas (mean, median, sd) -----------------------------------------------------------------

cluster_table(data_join,cl2,"mean")
cluster_table(data_join,cl3,"mean")
cluster_table(data_join,cl4,"mean")
cluster_table(data_join,cl5,"mean")
cluster_table(data_join,cl6,"mean")
cluster_table(data_join,cl7,"mean")



# características cl4 -----------------------------------------------------

data_cl4 <- cl4$data    

data_cl4 <- data_cl4 %>% 
  rename(user_id=name) %>% 
  select(user_id,cluster)


data_freq$user_id <- as.factor(data_freq$user_id )
data_cl4<-full_join(data_cl4 ,
                      data_freq,
                      by="user_id")
print("Days")
data_cl4 %>% 
  group_by(cluster) %>% 
  summarise(mean=mean(days),
            median=median(days),
            sd=sd(days))

med <- c()
median <- c()
sd <- c()
for (i in 1:4){
x <- data_cl4[data_cl4$cluster==i,]

x <- x %>% 
  group_by(user_id,order_id) %>% 
  tally
x$order_id <- NULL
hist(x$n)
med[i] <- mean(x$n)
median[i] <- median(x$n)
sd[i] <- sd(x$n)
}
quant_cl4 <- tibble(mean=med,median=median,sd=sd)
print("Produtos")
quant_cl4 


#1: compra mesma quantidade, 
#mas o intervalo de tempo 
#está reduzindo

#2: COmpra pouco, mas com alta frequência,
#está praticamtne estável

#3:Compra muito frequentemente, o intervalo estável,
#mas quantidade de produtos está caindo

#4:Faz compras semanais de poucos produtos,
# tanto dias, como quantidade de produtos estão estáveis

#Grupo cuja receita está aumentando : 1
# Grupos estáveis: 2 e 4
# Grupos que está comprado menos: 3



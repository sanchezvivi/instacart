#gráficos:  freqAPP, freqF,comp_APP_FxAPP, freq_supermercado

# bibliotecas -------------------------------------------------------------

library(tidyverse)
library(plyr)
library(lubridate)
library(Hmisc)
library(skimr)
library(ggplot2)
library(ggpubr)
library(plotly)
library(readxl)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(textshape)
library(tm)
library(extrafont)

# visualização gráfica ----------------------------------------------------


#font_import()
theme_set(theme_minimal())
theme_update(text = element_text(family = "Brandon Text"),
             plot.title = element_text(face = "bold"))
source('instacart_palette.R')

# dados pesquisa ----------------------------------------------------------


data <- read_excel("Pesquisa de Compras Online(3).xlsx")
data <- mutate_if(data, is.character, tolower)
data<- data[,colSums(is.na(data))<nrow(data)]
data<- data[rowSums(is.na(data))<ncol(data),]


# dados Instacart----------------------------------------------------------
path <- "data/"
file <- 'orders.csv'
data_insta <- read.csv(paste(path,file,sep = ""))

# read_data ---------------------------------------------------------------

read_data <- function(data,column,filter){
  col <- which( colnames(data)==column )
  colnames(data)[col] <- "x"
  data <- data %>% drop_na(x)
  data <- data[data$x==filter,]
  colnames(data)[col] <- column
  data<- data[,colSums(is.na(data))<nrow(data)]
  data<- data[rowSums(is.na(data))<ncol(data),]
  
  return(data)
}

# Utiliza app (data_1: data_1F, data_1APP) -------------------------------------------------------------

data_1<- read_data(data,"Faz compras via app?" ,'sim')

data_1<- data_1%>% 
  drop_na("As compras de supermecado são feitas por app ou fisicamente?2")

## "As compras de supermecado são feitas por app ou fisicamente?2"
data_1F<- read_data(data_1,"As compras de supermecado são feitas por app ou fisicamente?2",'fisicamente')
data_1APP <- read_data(data_1,"As compras de supermecado são feitas por app ou fisicamente?2",'app')


# Funções de análise das palavras -----------------------------------------
## Frequência (dataframe): palavras(df,cols)

palavras <- function(df,cols){
  x <- subset(df,select=cols)
  x<- VCorpus(VectorSource(x))
  
  x <- x%>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeWords, iconv(stopwords("portuguese"))) %>% 
    tm_map(stemDocument, language = "portuguese")
  
  x<- TermDocumentMatrix(x)
  x<- as.matrix(x)
  x<- sort(rowSums(x),decreasing=TRUE)
  x <- data.frame(word = names(x),freq=x)
  
  if ("mercadolivr" %in% rownames(x)){
    c=which(rownames(x)=="mercadolivr")
    rownames(x)[c] <- "merc.livre"
    x$word[c] <- "merc.livre"
  }
  
  return(x)
}


## Frequência (gráfico): freq_palavras(df,cols,q)

freq_palavras <- function(df,cols,q){
  x <- palavras(df,cols)
  x <- na.omit(x, cols=cols)
  lim <- x$freq[q]
  x <- x %>% 
    filter(freq>= lim) %>% 
    ggplot(aes(x=reorder(word,freq),y=freq))+
    geom_col(fill= ic_cols('orange'))+
    labs(x="",title=deparse(substitute(df)))+
    coord_flip()
  
  return(x)
}


# "Quais apps costuma usar?" ----------------------------------------------

## Compra de supermecado por app

col="Quais apps costuma usar?"
freqAPP <- freq_palavras(data_1APP,col,6)
freqAPP 

## Compra de supermecado fisicamente

col="Quais apps costuma usar?"
freqF <- freq_palavras(data_1F,col,8)
freqF

## Comparação

pergunta <-"Quais apps costuma usar?"
n <- 9
dAPP <- palavras(data_1APP,pergunta)
colnames(dAPP)[2] <- "APP"
dF <- palavras(data_1F,pergunta)
colnames(dF)[2] <- "F"

x <- join(dAPP, dF,by="word",type ="full")
x[is.na(x)] <- 0
x$soma <- x$APP+x$F
x <- x[order(x$soma,decreasing = TRUE),]
x <- x[c(1:n),]
x$soma <- NULL  
x$APP<- round(x$APP/sum(x$APP),3)
x$F<- round(x$F/sum(x$F),3)  

xP <- pivot_longer(data=x,
                  cols=APP:F,
                  names_to="Compra",
                  values_to="Frequência")
fig <- xP %>% 
ggplot(aes(x=reorder(word,-Frequência),y=Frequência,fill=Compra)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  scale_fill_manual(values=c("#43b02a",
                             "#ff8200"))
comp_APP_FxAPP <- fig
comp_APP_FxAPP


# "Com que frequência você faz compras de mercado?" -----------------------

## PESQUISA
col <-  "Com que frequência você faz compras de mercado?"

freq_pes <- table(data_1F$`Com que frequência você faz compras de mercado?`)
freq_pes <- data.frame(freq_pes)
colnames(freq_pes) <- c("freq_mes","Pesquisa")
freq_pes$Pesquisa <- round(freq_pes$Pesquisa /sum(freq_pes$Pesquisa),3)

## INSTACART

mean_interval <- data_insta%>% 
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

freq_insta <- mean_interval %>% 
  group_by(freq_mes) %>% 
  summarise("Instacart" = n()/nrow(mean_interval))

freq_insta$Instacart <- round(freq_insta$Instacart,3)

## Join freq_insta e freq_pes

freq <- merge(x = freq_insta, y = freq_pes, by = "freq_mes", all = TRUE)


freq <- freq%>% pivot_longer(
  cols = Instacart:Pesquisa,
  names_to = "x",
  values_to = "freq")

## gráfico
freq_supermecado <- freq %>% 
  ggplot(aes(x=as.factor(freq_mes), y=freq, fill=x)) +
  geom_bar(stat="identity",position=position_dodge())+
  labs(x=" ",title ="Compras de supermcado por mês ")+
  theme_minimal()

freq_supermecado







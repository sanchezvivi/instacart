# Declarndo bibliotecas -

library(tidyverse)
library(stringr)
library(ggplot2)
library(skimr)


# Definir Work Directory -

getwd()
setwd('C:/Users/Rafael/Documents/INSPER - DATA SCIENCE/Instacart/')


# Ler aqruivos -

order_prod_raw <- read.csv("instacart_2017_05_01/order_products__prior.csv") %>% glimpse
order_products_train_raw <- read.csv("instacart_2017_05_01/order_products__train.csv") %>% glimpse
orders_raw <- read.csv("instacart_2017_05_01/orders.csv") %>% glimpse
products_raw <- read.csv("instacart_2017_05_01/products.csv") %>% glimpse
aisles_raw <- read.csv("instacart_2017_05_01/aisles.csv") %>% glimpse
departments_raw <- read.csv("instacart_2017_05_01/departments.csv") %>% glimpse


# Tips -
   
  install.packages("esquisse")
  library(esquisse)

        
# Mesclando as bases 'order_prior' e 'order_train' -
   order_total_raw <- union(order_prod_raw,order_products_train_raw)
   #order_total_raw %>%  skim
   order_total_raw %>%  head

         
# Clientes que compraram mais de 10 compras
   
   orders_count <- orders_raw %>% 
     group_by(user_id) %>% 
     summarise(qt_ordem = n_distinct(order_id)) %>%
     arrange(qt_ordem) %>%  
     ungroup() %>% 
     mutate(abaixo_10 = if_else(qt_ordem <= 10, 'Menos de 10 pedidos', 'Mais de 10 pedidos'))
   
   orders_count %>%  head
   
   orders2 <- orders_raw  %>% 
     left_join(orders_count) 

      
# Fazendo um left join da base de 'base_prod' com a base de base_aisles e base_dept, para trazer os nomes dos corredores e departamentos - 

   base_total <- order_total_raw  %>% 
     left_join(products_raw) %>% 
     left_join(aisles_raw) %>% 
     left_join(departments_raw) %>% 
     left_join(orders2)
   
   base_total %>%  head
   
   base_total <- base_total[,c(1:5,8:17)]   
   base_total %>%  head   


      

# Utilizando Gráficos do GGPLOT para entender a base -   Mais de 10 pedidos - Qtd Produtos x Departamento

   grafico1 <- base_total %>% 
     group_by(department, abaixo_10) %>% 
     summarise(unique_products = n_distinct(product_id)) %>% 
     arrange(desc(unique_products)) %>%  
     ungroup() 
   
   #%>% 
     #select(department, unique_products, abaixo_10)
     #group_by(abaixo_10) %>%
     #summarise(ranking = row()) %>% 
     #arrange(ranking) %>%  
     #ungroup()
      
   
   grafico1 %>%  head
   
   grafico1[1:12,] %>%
     ggplot(aes(x = reorder(department,-unique_products), fill = abaixo_10, weight = unique_products)) +
     geom_bar() +
     scale_fill_brewer(palette = "YlGn") +
     #labs(x = "Departamentos", y = "Qtd de Produtos")+
     scale_x_discrete(name = "Departamentos", expand = c(0, 0)) +
     scale_y_continuous(name = "Qtd de Produtos", expand = c(0, 0))+
     theme_minimal() +
     theme(panel.grid.minor.y =element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           legend.position = "none",
           panel.spacing = unit(1, "lines"),
           strip.text=element_text(hjust=0.8, vjust = -1.2))+
     facet_wrap(vars(abaixo_10))
   
   ## Em geral a distribuição nos 5 maiores deprtamentos apresenta uma curva muito similar.
   ## No entanto, observa-se que os clientes com mais compras possuem uma pequena maior variabilidade de produtos, possivelmente em função da recorrencia nas compras.
  
   
   
   
# Não consegui ajuar #
   
# Utilizando Graficos do GGPLOT para entender a base - explorar principais produtos dentro do maior departamento
   
   grafico2 <- base_total %>% group_by(aisle, abaixo_10) %>% 
     #filter(department %in% 'personal care')%>% 
     summarise(unique_product = n_distinct(product_id)) %>% 
     arrange(desc(unique_product)) %>%
     ungroup()
   
   grafico2 %>%  head
   
   grafico2[1:12,] %>%
     ggplot(aes(x = reorder(aisle,-unique_product), fill = abaixo_10, weight = unique_product)) +
     geom_bar() +
     scale_fill_brewer(palette = "YlGn") +
     #labs(x = "aisle", y = "Qtd de Produtos")+
     scale_x_discrete(name = "Corredor do Mercado", expand = c(0, 0)) +
     scale_y_continuous(name = "Qtd de Produtos", expand = c(0, 0))+
     theme_minimal() +
     theme(panel.grid.minor.y =element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           legend.position = "none",
           panel.spacing = unit(1, "lines"),
           strip.text=element_text(hjust=0.8, vjust = -1.2))+
     facet_wrap(vars(abaixo_10))   
   
   
   
   
   
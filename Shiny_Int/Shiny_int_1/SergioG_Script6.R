
# Bibliotecas -------------------------------------------------------------

# biblios <- c('tidyverse','dplyr', 'ggplot2', 'lubridate', 'stringr', 
#              'inspectdf', 'skimr', 'naniar', 'visdat', 'tidymodels', 
#              'klaR', 'corrplot', 'NetCluster', 'factoextra', 'maptree', 'treemap', 'DT','patchwork')
# library(ggiraph)
# library(tidymodels)
# library(tidyverse)
# library(cluster)

biblios <- c('stringr', 'inspectdf', 'dplyr', 'skimr', 
             'factoextra', 'treemap', 'd3treeR', 'ggiraphExtra')


for (i in biblios){
  if(!require(i, character.only = TRUE)){install.packages(paste0(i)); library(i, character.only = TRUE)}
}



# install.packages("remotes")
#remotes::install_github("timelyportfolio/d3treeR")
# 
# install.packages('treemap')

library(treemap)
 
library(d3treeR)
# 
# library(ggiraphExtra)


# Importando os dados em .csv, usando o read.csv --------------------------

# path <- "data\\"
# # path <- "..\\..\\data\\"
# file_aisles <- "aisles.csv"
# base_aisles <- read.csv(paste(path,file_aisles,sep = ""))
# 
# file_dept <- "departments.csv"
# base_dept <- read.csv(paste(path,file_dept,sep = ""))
# 
# file_ord_prior <- "order_products__prior.csv"
# base_ord_prior <- read.csv(paste(path,file_ord_prior,sep = "")) %>% glimpse()
# 
# file_ord_train <- "order_products__train.csv"
# base_ord_train <- read.csv(paste(path,file_ord_train,sep = "")) %>% glimpse()
# 
# file_orders <- "orders.csv"
# base_orders <- read.csv(paste(path,file_orders,sep = "")) %>% glimpse()
# 
# file_products <- "products.csv"
# base_products <- read.csv(paste(path,file_products,sep = "")) %>% glimpse()
# 


# Funções -----------------------------------------------------------------

x4 <- function(x) x^4

x_4<- function(x) sqrt(sqrt(x))

x2 <- function(x) x^2

x_2<- function(x) sqrt(x)


# Inicio Código -----------------------------------------------------------

# Removendo os registros da tabela `orders` que estão categorizados como 'test', uma vez que essas 'order_id' não possuem dados correspondentes nas bases de product_order
# base_orders_cl <- base_orders %>% filter(eval_set != 'test')
# 
# # Criando um sample de 10% dos usuários
# usuarios <- base_orders_cl %>% group_by(user_id) %>% summarise(cnt = n())
# set.seed(123)
# usuarios_10 <- usuarios %>% filter(cnt > 4) %>% sample_frac(size = 0.1)
# 
# # write.csv(usuarios_10, "data\\user.csv")
# 
# # Filtrar por um número mínimo de 5 compras ###########################################
# 
# 
# # Mesclando as bases 'order_prior' e 'order_train'
# base_ord_geral <- dplyr::union(base_ord_prior,base_ord_train)
# 
# # Usando somente os 10%
# base_orders_cl <- base_orders_cl %>% right_join(usuarios_10 %>% select(user_id))
# 
# base_ord_geral <- base_ord_geral %>% right_join(base_orders_cl %>% select(order_id)) 
# 
# # Fazendo um left join da base de 'base_prod' com a base de base_aisles e base_dept, para trazer os nomes dos corredores e departamentos
# base_products_names <- base_products %>% left_join(base_aisles) %>% left_join(base_dept)
# base_products_names <- base_products_names[,c(1:2,5:6)]
# 
# 
# # Fazendo um left join da base de order_geral com a base_products_names, para trazer dados dos produtos comprados (nome_produto, corredor e departamento)
# base_ord_geral_prod <- base_ord_geral %>% left_join(base_products_names)
# 
# 
# # Nova Clusterização - 2020-08-25 -----------------------------------------
# 
# # definindo os principais produtos em termos de recorrência
# prod_top_100 <- base_ord_geral_prod %>% 
#                   group_by(product_name) %>% 
#                   summarise(n_reordered = sum(reordered)) %>% 
#                   arrange(desc(n_reordered))
# 
# 
# 
# base_ord_geral_all %>% select(user_id,order_number,order_id,add_to_cart_order,product_name) %>% filter(user_id == 22) %>% arrange(user_id, order_number) %>% View()
# 
# 
# 
# 
# # definindo o tempo médio entre compras de um determinado produto
# base_ord_geral_all <- base_ord_geral_prod %>% 
#   left_join(base_orders_cl %>% 
#               select(order_id, 
#                      user_id, 
#                      order_number, 
#                      days_since_prior_order)) %>% 
#   filter(!is.na(days_since_prior_order)) %>% 
#   arrange(user_id,desc(order_number))

# base_ord_geral_all %>% write.csv("data\\base_ord_geral_all.csv")

base_ord_geral_all <- read.csv("data\\base_ord_geral_all.csv")
base_ord_geral_all <- base_ord_geral_all[2:ncol(base_ord_geral_all)]

# prod_mean_time <- base_ord_geral_all %>% 
#   group_by(user_id,product_name) %>% 
#   summarise(tempo_medio = (sum(days_since_prior_order)/(sum(reordered)+1))) %>% 
#   # filter(tempo_medio > 0) %>% 
#   group_by(product_name) %>% 
#   summarise(tempo_medio = mean(tempo_medio)) %>% 
#   right_join(prod_top_100) %>% 
#   arrange(tempo_medio, desc(n_reordered))
# 
# 
# 
# prod_mean_time_norm <- prod_mean_time[,c(2:3)] %>% scale(center = F) %>% as_tibble()
# 
# # base com os produtos ordenados por peso (total de recorrencias dividido pelo tempo médio entre compras desse produto)
# prod_fator <- bind_cols(prod_mean_time[,1],prod_mean_time_norm) %>% 
#                           mutate(fator = ifelse((tempo_medio == 0 | is.na(tempo_medio)), 0, n_reordered/tempo_medio)) %>% 
#                           arrange(desc(fator))
# 
# 
# # clusterizar por:
# # Numerico
# #   Número de Compras
# #   Tempo Médio entre compras
# #   Número médio de produtos no carrinho
# #   Peso médio do carrinho
# #   Fator de recorrência de produtos (numero de recorrencias dividido pelo total de produtos comprados)
# 
# # Criando a tabela a ser utilizada para K-means
# base_k_user_ord <- base_ord_geral_all %>% select(user_id, order_id, order_number, days_since_prior_order, add_to_cart_order, reordered, product_name) %>%
#                                           filter(!is.na(days_since_prior_order))%>% 
#                                           left_join(prod_fator %>% select(product_name, fator)) %>% 
#                                           group_by(user_id, order_id) %>% 
#                                           summarise(order_number = mean(order_number),
#                                                     days_since_prior_order = mean(days_since_prior_order),
#                                                     n_prod_cart = max(add_to_cart_order),
#                                                     peso_cart = mean(fator),
#                                                     rec_fat = mean(reordered))
# 
# # 
# # base_ord_geral_all %>% filter(order_id == 144358)
# 
# 
# # base para K-Means com os campos descritos acima
# base_k_user <- base_k_user_ord %>% 
#                 group_by(user_id) %>% 
#                 summarise(n_compras = max(order_number),
#                           t_mean = mean(days_since_prior_order),
#                           mean_prod_cart = mean(n_prod_cart),
#                           mean_peso_cart = mean(peso_cart),
#                           mean_rec_fat = mean(rec_fat))

# base_k_user %>% write.csv("data\\base_k_user.csv")

base_k_user <- read.csv("data\\base_k_user.csv")
base_k_user <- base_k_user[2:ncol(base_k_user)]

# Verificando um consumidor como exemplo
# base_ord_geral_all %>% left_join(prod_fator %>% select(product_name, fator)) %>% filter(user_id == 4) %>% view()


# Buscando melhor k para clusterização
# fator_vet <- tibble(k = numeric(), fator = numeric(), variacao = numeric(), media = numeric(), singulares = numeric())
# fator_atual <- 1
# for (i in 2:20){
#   print(i)
#   k_mean <- base_k_user[,c(2:ncol(base_k_user))] %>% scale() %>% hkmeans(k = i)
#   fator_ant = fator_atual
#   fator_atual = (k_mean$tot.withinss/k_mean$betweenss)
#   sing <- nrow(as_tibble(k_mean$cluster) %>% group_by(value) %>% count() %>% filter(n == 1))
#   if (i > 11){
#     med <- mean(fator_vet$variacao[i-10:i])
#     fator_vet <- fator_vet %>% bind_rows(c(k = i, fator = fator_atual, variacao = ((fator_atual - fator_ant)/fator_ant), media = med, singulares = sing))
#   }
#   else{
#     fator_vet <- fator_vet %>% bind_rows(c(k = i, fator = fator_atual, variacao = ((fator_atual - fator_ant)/fator_ant), media = NA, singulares = sing))
#   }
# }
# 
# # Encontrando o primeiro 'k' que possui a média da variação dos últimos 10 valores > -0.1. Ao encontrar esse 'k', podemos escolher valores até dez 'k's anteriores. 
# k_max <- fator_vet$k[min(which((fator_vet$media > -0.1) == TRUE))]
# k_min <- k_max - 9
# 
# # Gráficos k
# fator_vet %>% ggplot(aes(x = k)) +
#   geom_line(aes(y = fator), color = "darkgreen") + 
#   labs(title = "Gráfico de Fator (dist_Intra_Cluster / dist_Inter_Cluster) pelo número de clusters") +
#   theme_minimal()+
#   geom_vline(xintercept = c(7, 9), show.legend = TRUE, linetype = "dashed", color = 'darkorange', size = 1)+
#   geom_text(aes(x = 7.2, y = 1, label = '7'),hjust = 0, color = 'darkorange')+
#   geom_text(aes(x = 9.2, y = 1, label = '9'),hjust = 0, color = 'darkorange')+
#   theme(title = element_text(size = 7),axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
# 
# fator_vet %>% ggplot(aes(x = k)) +
#   geom_line(aes(y = variacao), color = "blue") + 
#   geom_hline(yintercept = -0.10, linetype = "dotted") +
#   geom_hline(yintercept = 0.0) +
#   geom_rect(aes(xmin = k_min, xmax = k_max, ymin = -0.1, ymax = 0), alpha = 1/500, color = "red", fill = "green") +
#   geom_vline(xintercept = c(k_min, k_max), show.legend = TRUE, linetype = "dashed") +
#   geom_line(aes(y = singulares/40), color = "red") + 
#   scale_y_continuous(
#     name = "Variação Fator intra/inter Cluster",
#     sec.axis = sec_axis(trans =~.*40, name = "n_Sing_Clust")
#   )
# 
# fator_vet[1:(nrow(fator_vet)/2),] %>% ggplot(aes(x = k)) +
#   geom_line(aes(y = variacao), color = "blue") + 
#   geom_hline(yintercept = -0.10, linetype = "dotted") +
#   geom_hline(yintercept = 0.0) +
#   geom_rect(aes(xmin = k_min, xmax = k_max, ymin = -0.1, ymax = 0), alpha = 1/500, color = "red", fill = "green") +
#   geom_vline(xintercept = c(k_min, k_max), show.legend = TRUE, linetype = "dashed") +
#   geom_line(aes(y = singulares/40), color = "red") + 
#   scale_y_continuous(
#     name = "Variação Fator intra/inter Cluster",
#     sec.axis = sec_axis(trans =~.*40, name = "n_Sing_Clust")
#   )



# rodando K-means


best_k <- 7
set.seed(321)
clust_kmean <- base_k_user[,c(2:ncol(base_k_user))] %>% scale(center = F) %>% hkmeans(k = best_k)

# Calculando os perfis dos clusters

dados_clusters <- clust_kmean$centers


# A partir da análise é possível definir o perfil dos clusters, para poder dar um nome aos mesmos
dados_clusters <- tibble(cluster = rownames(dados_clusters)) %>% bind_cols(as_tibble(dados_clusters))

dados_clusters <- read.csv("data\\base_graf_exp.csv")
dados_clusters <- dados_clusters[2:ncol(dados_clusters)]


(spider <- dados_clusters %>% ggRadar(aes(x = c(n_compras,
                                     t_mean,
                                     mean_prod_cart,
                                     mean_peso_cart,
                                     mean_rec_fat),
                               facet = cluster),
                           interactive = F,
                           size = 1.5,
                           legend.position = "right"
                           ) + 
                            
    )





base_graf2 <- base_k_user %>%
  bind_cols(cluster = clust_kmean$cluster) %>%
  filter(cluster == 5)

base_graf_exp <- base_k_user %>%
  bind_cols(cluster = clust_kmean$cluster)


formato <- list(
  geom_text(aes(x = 5, y = 5+0.1, label = "texto1" ),
            size = 3, color = 'orange', hjust = 0, vjust = 0)+
    labs(title = "Teste", x = "teste_X")
)

base_graf_exp %>% ggplot(aes(x =user_id, y = t_mean)) +
  geom_point() + formato


# Apresentando os dados dos clusters --------------------------------------
base_graf_exp %>% select(-user_id) %>% group_by(cluster) %>% summarise(n_compras = mean(n_compras),
                                                                       t_mean = mean(t_mean),
                                                                       mean_prod_cart = mean(mean_prod_cart),
                                                                       mean_peso_cart = mean(mean_peso_cart),
                                                                       mean_rec_fat = mean(mean_rec_fat))


# # Exportando base para o Leo ----------------------------------------------
# 
# base_graf_exp %>% write.csv("data\\base_graf_exp.csv")
# 
# 
# # Importando Base Leo e filtrando -----------------------------------------
# 
# base_leo <- read.csv("data\\data_L.csv") %>% tibble()
# 
# base_leo <- base_leo %>% select(user_id, cluster) %>% filter(cluster == 3)
# 
# base_leo %>% skim()
# 
# 
# # # Encontrando a Interseção ------------------------------------------------
# 
# intersect <- base_leo %>% inner_join(base_graf2, by = "user_id")
# 
# intersect %>% 
#   select(-c(user_id,cluster.x)) %>% 
#   group_by(cluster.y) %>% 
#   summarise(n_compras = mean(n_compras),
#                t_mean = mean(t_mean),
#                mean_prod_cart = mean(mean_prod_cart),
#                mean_peso_cart = mean(mean_peso_cart),
#                mean_rec_fat = mean(mean_rec_fat))
  


# 
# # Cluster Sérgio - Cluster 5
# # numero de usuários 3851
# base_graf2 %>% nrow()
# 
fat <- 206209/17507

1836*fat


# # Cluster Leo - Cluster 3
# # numero de usuários = 3430
# base_leo %>% nrow()
# 
# # Interseção entre os clusters: 1836
# intersect %>% nrow()
# 1836/3430
# 
# # Total: 206209
# # Amostra: 17507
# # Percentual: 8,5%



# Gráfico de TREEMAP ------------------------------------------------------


  # basic treemap
p <- base_ord_geral_all %>%
          right_join(base_graf2 %>% select(user_id)) %>%
          group_by(aisle, product_name) %>%
          summarise(contagem = n()) %>%
          arrange(desc(contagem)) %>%
          data.frame() %>%
          slice_head(n = 50) %>% 
          treemap(base_ord_geral_all,
                     index=c("aisle","product_name"),
                     vSize="contagem",
                     type="index",
                     palette = "Set2",
                     bg.labels=c("white"),
                     align.labels=list(
                       c("center", "center"), 
                       c("right", "bottom")
                     )
        )

ft %>%
  group_by(cluster, embalagem) %>%
  summarize(receita = sum(receita)) %>%
  ggplot(aes(area = receita, fill = cluster, subgroup = cluster, label = embalagem)) +
  geom_treemap()+
  geom_treemap_subgroup_text(place = "left", grow = T, alpha = 0.3, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  scale_fill_tableau("Tableau 20") +
  labs(title = 'Composição dos clusters pelo total da receita por embalagem') +
  theme_economist() +
  theme(legend.position="right", axis.line = element_blank())


  # make it interactive ("rootname" becomes the title of the plot):
inter <- d3tree2(p, rootname = "Principais Produtos")
# inter
  
# Resultado: falha para rodar o k_means devido ao tamanho da base
# Próximo passo
# criar um sample da base para isso e também ver para Kendall e Pearson (análise e seguir)
# getwd()
# base_k_user <- read.csv("data\\base_graf_exp.csv")
# base_k_user <- base_k_user[2:ncol(base_k_user)]

# Bibliotecas -------------------------------------------------------------

# biblios <- c('tidyverse','dplyr', 'ggplot2', 'lubridate', 'stringr', 
#              'inspectdf', 'skimr', 'naniar', 'visdat', 'tidymodels', 
#              'klaR', 'corrplot', 'NetCluster', 'factoextra', 'maptree', 'treemap', 'DT','patchwork')

biblios <- c('tidyverse', 'stringr', 'janitor', 'inspectdf', 'dplyr', 'skimr', 
             'plotly', 'RcppRoll', 'lubridate', 'factoextra', 'forecats', 'tidymodels')

library(tidymodels)

for (i in biblios){
  if(!require(i, character.only = TRUE)){install.packages(paste0(i)); library(i, character.only = TRUE)}
}

# Importando os dados em .csv, usando o read.csv --------------------------

#path <- "data\\"
path <- "data/"
file_aisles <- "aisles.csv"
base_aisles <- read.csv(paste(path,file_aisles,sep = ""))

file_dept <- "departments.csv"
base_dept <- read.csv(paste(path,file_dept,sep = ""))

file_ord_prior <- "order_products__prior.csv"
base_ord_prior <- read.csv(paste(path,file_ord_prior,sep = "")) %>% glimpse()

file_ord_train <- "order_products__train.csv"
base_ord_train <- read.csv(paste(path,file_ord_train,sep = "")) %>% glimpse()

file_orders <- "orders.csv"
base_orders <- read.csv(paste(path,file_orders,sep = "")) %>% glimpse()

file_products <- "products.csv"
base_products <- read.csv(paste(path,file_products,sep = "")) %>% glimpse()



# Inicio Código -----------------------------------------------------------


# CONCLUSÔES DA ANÁLISE PRÉVIA:
# As bases order_train e order_prior são excludentes, ou seja, os order_id não possuem interseção.
# A base 'order_prior' tem todos os seus order_id encontrados na base 'orders', bem como a base 'order_train'.
# Existem 75k 'order_id', que pertencem a base de testes. Contudo, como essa base de teste não está disponível, podemos remover esses 75k registros.
# AÇÕES
# 1 - Remover 75k registros da base 'orders'.
# 2 - As bases order_train e order_prior, poderão ser mescladas, uma vez que não iremos usar a base para predição.

# Removendo os registros da tabela `orders` que estão categorizados como 'test', uma vez que essas 'order_id' não possuem dados correspondentes nas bases de product_order
base_orders_cl <- base_orders %>% filter(eval_set != 'test')

# Mesclando as bases 'order_prior' e 'order_train'
base_ord_geral <- dplyr::union(base_ord_prior,base_ord_train)

# Fazendo um left join da base de 'base_prod' com a base de base_aisles e base_dept, para trazer os nomes dos corredores e departamentos
base_products_names <- base_products %>% left_join(base_aisles) %>% left_join(base_dept)
base_products_names <- base_products_names[,c(1:2,5:6)]


# Fazendo um left join da base de order_geral com a base_products_names, para trazer dados dos produtos comprados (nome_produto, corredor e departamento)
base_ord_geral_prod <- base_ord_geral %>% left_join(base_products_names)


# Filtro Média Móvel Vivi -------------------------------------------------

base_orders_cl_mm <- base_orders_cl %>% 
  filter(order_number != 1) %>% 
  arrange(user_id, order_number) %>% 
  mutate(order_hour_of_day = as.numeric(order_hour_of_day)) %>% 
  group_by(user_id) %>% 
  mutate(days_ma = roll_mean(days_since_prior_order, 5, fill = NA, na.rm = T)) %>% 
  ungroup() %>% 
  glimpse




# Código de 2020-08-15
           
# filtrando somente os clientes que estão abaixo da mediana
base_orders_cl_mm <- base_orders_cl_mm %>% arrange(user_id,-order_number)

users_churn <- base_orders_cl_mm %>% dplyr::group_by(user_id) %>% summarise(ult_ordem = first(order_number), days_ma_new = nth(days_ma,3), media_days = mean(days_since_prior_order)) %>% filter(days_ma_new == 30 | (is.na(days_ma_new) & media_days >= 25)) %>% glimpse()
users_rec <- base_orders_cl_mm %>% dplyr::group_by(user_id) %>% summarise(ult_ordem = first(order_number), days_ma_new = nth(days_ma,3), media_days = mean(days_since_prior_order)) %>% filter(days_ma_new < 8) %>% glimpse()


base_orders_cl_rec <- base_orders_cl_mm %>% right_join(users_rec)
base_orders_cl_not_rec <- base_orders_cl_mm %>% right_join(users_churn)

base_ord_geral_prod_rec <- base_ord_geral_prod %>% dplyr::filter(order_id %in% base_orders_cl_rec$order_id)
base_ord_geral_prod_not_rec <- base_ord_geral_prod %>% dplyr::filter(order_id %in% base_orders_cl_not_rec$order_id)

# Trazendo a coluna user_id
base_ord_geral_prod_rec2 <- base_ord_geral_prod_rec %>% left_join(base_orders_cl_rec)
base_ord_geral_prod_rec2 <- base_ord_geral_prod_rec2[,c(1:8,10,14)]

base_ord_geral_prod_not_rec2 <- base_ord_geral_prod_not_rec %>% left_join(base_orders_cl_not_rec)
base_ord_geral_prod_not_rec2 <- base_ord_geral_prod_not_rec2[,c(1:8,10,14)]

# Gráfico da média móvel
# base_orders_cl_not_rec2 %>% 
#   na.omit() %>% 
#   ggplot(aes(x = days_ma)) +
#   geom_bar(fill = 'darkgreen') +
#   geom_vline(xintercept = 8, color = 'orange', 
#              linetype = 'dashed') +
#   theme_minimal()


# HIPOTESE
# Compras que tem recorrência, provavelmente é feita, repetindo uma cesta anterior.
# Compras com menor recorrência tem maior variaçao na cesta de compras


# Rodando o modelo para os cem principais recorrentes e os 100 piores recorrentes



# Histograma de Produtos comprados por Ordem ------------------------------
order_n_total <- base_ord_geral_prod_not_rec %>% group_by(order_id) %>% summarise(quant_prod = n(), unid_recompra = sum(reordered))

bin <- order_n_total$quant_prod %>% max()
order_n_total %>% ggplot(aes(x = quant_prod)) +
                    geom_histogram(bins = bin/10) +
                    scale_y_sqrt()
x4 <- function(x) x^4

x_4<- function(x) sqrt(sqrt(x))

x2 <- function(x) x^2

x_2<- function(x) sqrt(x)


# order_n_total %>% ggplot() +
#   geom_histogram(aes(x = quant_prod), bins = bin/10,) +
#   scale_y_continuous(trans = scales::trans_new(name = "sqrt_sqrt",transform = x_4, inverse = x4)) +
#   labs(title = "Histograma de No Produtos comprados") 
# 
# order_n_total %>% ggplot(aes(x = quant_prod)) +
#   geom_freqpoly(bins = bin/10) +
#   scale_y_continuous(trans = scales::trans_new(name = "sqrt_sqrt",transform = x_4, inverse = x4)) +
#   labs(title = "Histograma de No Produtos comprados")

# Produtos Mais Recorrentes ----------------------------------------------------
# em qual posição do carrinho, se localizam o produtos MAIS recorrentes
rec_ord_cart <- base_ord_geral_prod_rec2 %>% group_by(add_to_cart_order) %>% 
                                                  summarise(recorrencias = sum(reordered),
                                                            total = n()) %>% 
                                                  mutate(rec_perc = recorrencias/total) %>% 
                                                  arrange(add_to_cart_order)

rec_ord_cart %>% ggplot(aes(add_to_cart_order, rec_perc)) +
                  geom_col() +
                  labs(title = "Gráfico de ordem_carrinho x percentual de produtos recorrentes")
  

# Produtos Menos Recorrentes ----------------------------------------------------
# em qual posição do carrinho, se localizam o produtos não-recorrentes
nao_rec_ord_cart <- base_ord_geral_prod_not_rec2 %>% group_by(add_to_cart_order) %>% 
  summarise(total = n(),
            nao_recorrencia = total - sum(reordered)) %>% 
  mutate(nao_rec_perc = nao_recorrencia/total) %>% 
  arrange(add_to_cart_order)

nao_rec_ord_cart %>% ggplot(aes(add_to_cart_order, nao_rec_perc)) +
  geom_col() +
  labs(title = "Gráfico de ordem_carrinho x percentual de produtos nao_recorrentes")




# Heatmaps Integradora Intermediária --------------------------------------
# fazer uma análise pelos produtos que entram primeiro na cesta (por produto), nas compras feitas por clientes pouco recorrentes.

# HIPÓTESE:
# Existe uma relação entre a posição do produto no carrinho e a recorrência de compra

# PREMISSA DA ANÁLISE:
# Separar os clientes em 2 catergorias: Clientes muito recorrentes e clientes pouco recorrentes.
# Essa definição inicial é feita com base na variável 'days_since_pior_order'. 
# Primeiro se calcula o valor médio dessa variável, por user_id.
# Uma vez definidas os valores médios por cliente, calculam-se os quartis.
# São definidos como clientes pouco recorrentes, o que estão abaixo da madiana e Clientes recorrentes, aqueles que estão acima da mediana.
# Definido isso, será feita uma análise para cada um dos grupos de modo a buscar as discrepâncias.

# GRÁFICOS:
# Foram criados 2 Heatmaps, um para cada grupo de clientes, onde são apresentados os percentuais de recorrencias de produtos (100 produtos de maior 
# recorrência de compra), nas diferentes posições do carrinho de compras.

# INSIGHT:
# Comparando ambos os gráficos, não se percebem relevantes variações nem nos produtos apresentados, nem tampouco na proporção do produto nas diversas posições.

prod_ord_cart <- base_ord_geral_prod_not_rec2 %>% dplyr::group_by(product_name, add_to_cart_order) %>% 
  summarise(quantidade = n(), 
            recorrencias = sum(reordered)) %>% 
  mutate(rec_perc = recorrencias/quantidade) %>% 
  arrange(-quantidade)

# Definindo a média do número de produtos recorrentes
a <- base_ord_geral_prod_not_rec2$order_id %>% n_distinct() #número de pedidos
b <- base_ord_geral_prod_not_rec2$reordered %>% sum() #numero de produtos
n_prod1 = b/a
(texto1 <- paste("Média Produtos/Ordem = ", round(n_prod1,2), sep = ""))

prod_ord_cart2 <- prod_ord_cart %>% dplyr::group_by(product_name) %>% mutate(perc = recorrencias/sum(recorrencias))

prod_ord_cart2_list <- prod_ord_cart2 %>% group_by(product_name) %>% summarise(recorrencias_total = sum(recorrencias)) %>% arrange(-recorrencias_total)

prod_ord_cart2_list <- prod_ord_cart2_list[1:100,1]

prod_100_n_rec <- prod_ord_cart2 %>% right_join(prod_ord_cart2_list)

prod_100_n_rec %>% ggplot() +
  geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
  scale_fill_gradient2(low = "white", high = "darkgreen", limits = c(0,40))+#,trans = scales::trans_new(name = "quad",transform = x2, inverse = x_2))+
  theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1)) +
  labs(title = "Heatmap de Produtos x Cart_Order para clientes Churn", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
  theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
  geom_hline(yintercept = n_prod1, color = "orange") +
  scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
  geom_text(aes(x = 5, y = n_prod1+0.1, label = texto1 ), size = 3, color = 'orange', hjust = 0, vjust = 0)
  

# hm1 <- prod_100_n_rec %>% ggplot() +
#   geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
#   scale_fill_gradient2() +
#   ylim(0,150) +
#   theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1)) +
#   labs(title = "Heatmap de Produtos x Cart_Order para clientes Não-Recorrentes")
# ggplotly(hm1, tooltip = "perc")

# fazer uma análise pelos produtos que entram primeiro na cesta (por produto), nas compras feitas por clientes MAIS recorrentes.
prod_ord_cart_rec <- base_ord_geral_prod_rec2 %>% dplyr::group_by(product_name, add_to_cart_order) %>% 
  summarise(quantidade = n(), 
            recorrencias = sum(reordered)) %>% 
  mutate(rec_perc = recorrencias/quantidade) %>% 
  arrange(-quantidade)

# Definindo a média do número de produtos recorrentes
a <- base_ord_geral_prod_rec2$order_id %>% n_distinct() #número de pedidos
b <- base_ord_geral_prod_rec2$reordered %>% sum() #numero de produtos
n_prod2 = b/a
(texto2 <- paste("Média Produtos/Ordem = ", round(n_prod2,2), sep = ""))


prod_ord_cart_rec2 <- prod_ord_cart_rec %>% dplyr::group_by(product_name) %>% mutate(perc = recorrencias/sum(recorrencias))

prod_ord_cart_rec2_list <- prod_ord_cart_rec2 %>% group_by(product_name) %>% summarise(recorrencias_total = sum(recorrencias)) %>% arrange(-recorrencias_total)

prod_ord_cart_rec2_list <- prod_ord_cart_rec2_list[1:100,1]

prod_100_rec <- prod_ord_cart_rec2 %>% right_join(prod_ord_cart_rec2_list)

prod_100_rec %>% ggplot() +
  geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
  # scale_fill_gradient2(aes(fill = "darkgreen"))+
  scale_fill_gradient2(low = "white", high = "darkgreen", limits = c(0,40))+ #, trans = scales::trans_new(name = "quad",transform = x2, inverse = x_2))+
  theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1)) +
  labs(title = "Heatmap de Produtos x Cart_Order para clientes Recorrentes", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
  theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
  geom_hline(yintercept = n_prod2, color = "orange") +
  geom_text(aes(x = 5, y = n_prod2+0.1, label = texto2 ), size = 3, color = 'orange', hjust = 0, vjust = 0) +
  scale_y_continuous(limits = c(0,30),expand = c(0,0))

sum(prod_ord_cart_rec2_list$product_name %in% prod_ord_cart2_list$product_name)

view(prod_ord_cart_rec2_list$product_name[!(prod_ord_cart_rec2_list$product_name %in% prod_ord_cart2_list$product_name)])

view(prod_ord_cart2_list$product_name[!(prod_ord_cart2_list$product_name %in% prod_ord_cart_rec2_list$product_name)])

ggplot()+
  geom_la

# hm1 <- prod_100_rec %>% ggplot() +
#   geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
#   scale_fill_gradient2()+
#   ylim(0,150)+
#   theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1)) +
#   labs(title = "Heatmap de Produtos x Cart_Order para clientes Recorrentes")
# ggplotly(hm1, tooltip = "perc")




# Análise de HClust -------------------------------------------------------
# Nova Análise de HClust, onde agora o percentual é feito de maneira diferente. Será feita a contabilização dos percentuais por 
# order de inclusão no carrinho. Ou seja, cada ordem_cart terá um total de produtos que somará 100% e cada produtos terá seu percentual 
# na posição do carrinho.

# Buscando os 100 principais produtos da base geral
base_ord_geral_prod_total <- base_ord_geral_prod %>% left_join(base_orders_cl_mm)
# base_ord_geral_prod_total2 <- base_ord_geral_prod_total[,c(1:8,10,14)]
base_ord_geral_prod_total2 <- base_ord_geral_prod_total

prod_ord_cart_geral <- base_ord_geral_prod_total2 %>% dplyr::group_by(product_name, add_to_cart_order) %>% 
  summarise(quantidade = n(), 
            recorrencias = sum(reordered)) %>% 
  mutate(rec_perc = recorrencias/quantidade) %>% 
  arrange(-quantidade)

prod_ord_cart_geral2 <- prod_ord_cart_geral %>% dplyr::group_by(product_name) %>% mutate(perc = recorrencias/sum(recorrencias))

prod_ord_cart_geral2_list <- prod_ord_cart_geral2 %>% group_by(product_name) %>% summarise(recorrencias_total = sum(recorrencias)) %>% arrange(-recorrencias_total)

prod_ord_cart_geral2_list <- prod_ord_cart_geral2_list[1:50,1]

prod_100_geral <- prod_ord_cart_geral2 %>% right_join(prod_ord_cart_geral2_list)


# Hcluster com clientes CHURN
# ord_cart_prod2 <- prod_ord_cart %>% right_join(prod_ord_cart_geral2_list) %>% dplyr::group_by(add_to_cart_order) %>% mutate(perc = recorrencias/sum(recorrencias))
ord_cart_prod2 <- prod_ord_cart %>% dplyr::group_by(add_to_cart_order) %>% mutate(perc = recorrencias/sum(recorrencias))
mat_similarity_ord <- ord_cart_prod2 %>% dplyr::select(product_name,add_to_cart_order, perc) %>% pivot_wider(names_from = add_to_cart_order, values_from = perc)
mat_similarity_ord <- mat_similarity_ord[1:50,]


# Removendo os NAs
mat_similarity_ord <- mat_similarity_ord %>% replace(is.na(.),0)

# Normalizando os dados
receita <- mat_similarity_ord %>% recipe(product_name ~ .) %>%
  step_normalize(all_numeric(), -all_outcomes())

prep_receita <- prep(receita)

mat_similarity_ord_norm <- juice(prep_receita)[[ncol(juice(prep_receita))]] %>% cbind(juice(prep_receita)[,-ncol(juice(prep_receita))])

# Coletando as colunas com NA ou NaN 
x <- inspect_na(mat_similarity_ord_norm)
col_remove <- x$col_name[x$pcnt == 100]

# Removendo as colunas com NA ou NaN
mat_similarity_ord_norm <- mat_similarity_ord_norm %>% select(-c(col_remove))


# dist_mat <- get_dist(mat_similarity_ord_norm, upper = TRUE, diag = TRUE)
# 
n <- 5
vet_clust <- c(2:((nrow(mat_similarity_ord_norm)-1)/n))
vet_clust <- vet_clust * n
vet_clust2 <- c(c(2:9),vet_clust)
silho <- tibble(k = numeric(), silho_avg = numeric(), negatives = numeric(), singulares = numeric())

for (i in vet_clust2){
  cutted <- hcut(mat_similarity_ord_norm, hc_func = "hclust", hc_method = "ward.D2", k=i, graph = TRUE)
  negativos <- sum(cutted$silinfo$widths$sil_width < 0) / length(cutted$silinfo$widths$sil_width)
  sing <- nrow(as_tibble(cutted$cluster) %>% group_by(value) %>% count() %>% filter(n == 1))
  silho <- silho %>% bind_rows(c(k = i, silho_avg = cutted$silinfo$avg.width, negatives = negativos, singulares = sing))
  print(i)
}

best_k <- silho$k[silho$silho_avg == max(silho$silho_avg)]
best_k_neg <- silho$k[silho$negatives == min(silho$negatives)]

p1 <- silho %>% ggplot(aes(x = k)) +
  geom_line(aes(y = silho_avg), color = "blue") +
  # geom_rect(aes(xmin = 35, xmax = 53, ymin = 0.33, ymax = 0.35), alpha = 1/500, color = "red", fill = "green") +
  # geom_vline(xintercept = c(35, 53), show.legend = TRUE) +
  geom_line(aes(y = singulares/40), color = "red") +
  scale_y_continuous(
    name = "Avg_Silh",
    sec.axis = sec_axis(trans =~.*40, name = "n_Sing_Clust")
  ) +
  geom_vline(xintercept = 5)
p1

k_select <- 5
cutted_ord_not_rec <- hcut(mat_similarity_ord_norm, hc_func = "hclust", hc_method = "ward.D2", k=k_select, graph = TRUE)

cutted_ord_not_rec$labels <- as.character(mat_similarity_ord_norm$.)

fviz_dend(cutted_ord_not_rec, k = k_select, 
          cex = 0.6, 
          type = "rectangle", 
          k_colors = c("darkgreen","darkorange2"),
          labels_track_height = 15,
          # k_colors = c(1:4,6),
          ggtheme = theme_light(),
          main = "Dendrograma de Produtos - Clientes 'Churn'",
          ylim = c(-10,60),
          horiz = TRUE
          )

# cores <- colors()
# 
# cores[cores %>% str_detect("orange")]
# 
# fruit <- c("apple", "banana", "pear", "pineapple")
# str_locate(fruit, "$")
# str_locate(fruit, "a")
# str_locate(fruit, "e")
# str_locate(fruit, c("a", "b", "p", "p"))
# 
# str_locate_all(fruit, "a")
# str_locate_all(fruit, "e")
# str_locate_all(fruit, c("a", "b", "p", "p"))
# 
# fruit <- c("apple", "banana", "pear", "pinapple")
# str_detect(fruit, "a")
# str_detect(fruit, "^a")
# str_detect(fruit, "a$")


# Hcluster com clientes RECORRENTES
# ord_cart_prod_rec2 <- prod_ord_cart_rec %>% right_join(prod_ord_cart_geral2_list) %>% dplyr::group_by(add_to_cart_order) %>% mutate(perc = recorrencias/sum(recorrencias))
ord_cart_prod_rec2 <- prod_ord_cart_rec %>% dplyr::group_by(add_to_cart_order) %>% mutate(perc = recorrencias/sum(recorrencias))
mat_similarity_ord_rec <- ord_cart_prod_rec2 %>% dplyr::select(product_name,add_to_cart_order, perc) %>% pivot_wider(names_from = add_to_cart_order, values_from = perc)
mat_similarity_ord_rec <- mat_similarity_ord_rec[1:50,]


# Removendo os NAs
mat_similarity_ord_rec <- mat_similarity_ord_rec %>% replace(is.na(.),0)

# Normalizando os dados
receita <- mat_similarity_ord_rec %>% recipe(product_name ~ .) %>%
  step_normalize(all_numeric(), -all_outcomes())

prep_receita <- prep(receita)

mat_similarity_ord_rec_norm <- juice(prep_receita)[[ncol(juice(prep_receita))]] %>% cbind(juice(prep_receita)[,-ncol(juice(prep_receita))])

# Coletando as colunas com NA ou NaN 
x <- inspect_na(mat_similarity_ord_rec_norm)
col_remove <- x$col_name[x$pcnt == 100]

# Removendo as colunas com NA ou NaN
mat_similarity_ord_rec_norm <- mat_similarity_ord_rec_norm %>% select(-c(col_remove))




# dist_mat <- get_dist(mat_similarity_ord_rec_norm, upper = TRUE, diag = TRUE)

n <- 5
vet_clust <- c(2:((nrow(mat_similarity_ord_rec_norm)-1)/n))
vet_clust <- vet_clust * n
vet_clust2 <- c(c(2:9),vet_clust)
silho <- tibble(k = numeric(), silho_avg = numeric(), negatives = numeric(), singulares = numeric())

for (i in vet_clust2){
  cutted <- hcut(mat_similarity_ord_rec_norm, hc_func = "hclust", hc_method = "ward.D2", k=i, graph = TRUE)
  negativos <- sum(cutted$silinfo$widths$sil_width < 0) / length(cutted$silinfo$widths$sil_width)
  sing <- nrow(as_tibble(cutted$cluster) %>% group_by(value) %>% count() %>% filter(n == 1))
  silho <- silho %>% bind_rows(c(k = i, silho_avg = cutted$silinfo$avg.width, negatives = negativos, singulares = sing))
  print(i)
}

best_k <- silho$k[silho$silho_avg == max(silho$silho_avg)]
best_k_neg <- silho$k[silho$negatives == min(silho$negatives)]

p1 <- silho %>% ggplot(aes(x = k)) +
  geom_line(aes(y = silho_avg), color = "blue") +
  # geom_rect(aes(xmin = 35, xmax = 53, ymin = 0.33, ymax = 0.35), alpha = 1/500, color = "red", fill = "green") +
  # geom_vline(xintercept = c(35, 53), show.legend = TRUE) +
  geom_line(aes(y = singulares/40), color = "red") +
  scale_y_continuous(
    name = "Avg_Silh",
    sec.axis = sec_axis(trans =~.*40, name = "n_Sing_Clust")
  ) +
  geom_vline(xintercept = 5)
p1

k_select <- 5
cutted_ord_rec <- hcut(mat_similarity_ord_rec_norm, hc_func = "hclust", hc_method = "ward.D2", k=k_select, graph = TRUE)

cutted_ord_rec$labels <- as.character(mat_similarity_ord_rec_norm$.)

fviz_dend(cutted_ord_rec, k = k_select, 
          cex = 0.6, 
          type = "rectangle", 
          k_colors = c("darkgreen","darkorange2"),
          labels_track_height = 20,
          # k_colors = c(1:4,6),
          ggtheme = theme_light(),
          main = "Dendrograma de Produtos - Clientes Recorrentes",
          ylim = c(-20,60),
          horiz = TRUE
)



cutted_ord_rec$cluster

library(dendextend)

dend_not_rec <- as.dendrogram(cutted_ord_not_rec)
dend_rec <- as.dendrogram(cutted_ord_rec)

tang <- dendlist(dend_not_rec, dend_rec)

tang %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  tanglegram(labels_cex = 0.6, 
             margin_inner = 15, 
             k_labels = 4,
             k_branches = 4,
             axes = FALSE, 
             lwd = 2,
             main_left = "Produtos - Clientes Pouco Recorrentes",
             cex_main_left = 1,
             main_right = "Produtos - Clientes Recorrentes",
             cex_main_right = 1,
             dLeaf = 0.1
  )  


dendlist(dend_not_rec, dend_rec) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  tanglegram(labels_cex = 0.6, 
             margin_inner = 15, 
             k_labels = 4,
             k_branches = 4,
             axes = FALSE, 
             lwd = 2,
             main_left = "Produtos - Clientes Pouco Recorrentes",
             cex_main_left = 1,
             main_right = "Produtos - Clientes Recorrentes",
             cex_main_right = 1,
             dLeaf = 0.1
             )  
















# Montando um hclust de produto por order de carrinho para produtos de clientes pouco recorrentes
mat_similarity <- prod_ord_cart2 %>% dplyr::select(product_name,add_to_cart_order, perc) %>% pivot_wider(names_from = add_to_cart_order, values_from = perc)
mat_similarity2 <- mat_similarity[1:100,]


# Montando um hclust de produto por order de carrinho para produtos de clientes MAIS recorrentes
mat_similarity_rec <- prod_ord_cart_rec2 %>% dplyr::select(product_name,add_to_cart_order, perc) %>% pivot_wider(names_from = add_to_cart_order, values_from = perc)
mat_similarity_rec2 <- mat_similarity_rec[1:100,]

# vet_clust <- c(1:((nrow(mat_similarity2)-2)/20))
n <- 5
vet_clust <- c(1:((nrow(mat_similarity2)-1)/n))
vet_clust <- vet_clust * n
vet_clust2 <- c(c(2:10),vet_clust)
silho <- tibble(k = numeric(), silho_avg = numeric(), negatives = numeric(), singulares = numeric())

for (i in vet_clust2){
  cutted <- hcut(mat_similarity2, hc_func = "hclust", hc_method = "complete", k=i, graph = TRUE)
  negativos <- sum(cutted$silinfo$widths$sil_width < 0) / length(cutted$silinfo$widths$sil_width)
  sing <- nrow(as_tibble(cutted$cluster) %>% group_by(value) %>% count() %>% filter(n == 1))
  silho <- silho %>% bind_rows(c(k = i, silho_avg = cutted$silinfo$avg.width, negatives = negativos, singulares = sing))
  print(i)
}

best_k <- silho$k[silho$silho_avg == max(silho$silho_avg)]
best_k_neg <- silho$k[silho$negatives == min(silho$negatives)]

p1 <- silho %>% ggplot(aes(x = k)) +
  geom_line(aes(y = silho_avg), color = "blue") +
  # geom_rect(aes(xmin = 35, xmax = 53, ymin = 0.33, ymax = 0.35), alpha = 1/500, color = "red", fill = "green") +
  # geom_vline(xintercept = c(35, 53), show.legend = TRUE) +
  geom_line(aes(y = singulares/40), color = "red") + 
  scale_y_continuous(
    name = "Avg_Silh",
    sec.axis = sec_axis(trans =~.*40, name = "n_Sing_Clust")
  )
p1

cutted_not_rec <- hcut(mat_similarity2, hc_func = "hclust", hc_method = "complete", k=20, graph = TRUE)



# Removendo os NAs
mat_similarity2 <- mat_similarity2 %>% replace(is.na(.),0)

# Normalizando os dados
receita <- mat_similarity2 %>% recipe(product_name ~ .) %>%
  step_normalize(all_numeric(), -all_outcomes())

prep_receita <- prep(receita)

mat_similarity2_norm <- juice(prep_receita)[[ncol(juice(prep_receita))]] %>% cbind(juice(prep_receita)[,-ncol(juice(prep_receita))])

# Coletando as colunas com NA ou NaN 
x <- inspect_na(mat_similarity2_norm)
col_remove <- x$col_name[x$cnt == 100]

# Removendo as colunas com NA ou NaN
mat_similarity2_norm_sem_na <- mat_similarity2_norm %>% select(-c(col_remove))

dist_mat <- get_dist(mat_similarity2_norm_sem_na, upper = TRUE, diag = TRUE)
print(dist_mat)


cutted_not_rec$labels <- mat_similarity2$product_name


cutted_rec <- hcut(mat_similarity_rec2, hc_func = "hclust", hc_method = "complete", k=20, graph = TRUE)
cutted_rec$labels <- mat_similarity_rec2$product_name

cutted_not_rec$size

fviz_dend(cutted_not_rec, k = 20, 
          cex = 0.7, 
          type = "rectangle", 
          k_colors = c("darkgreen","orange"),
          labels_track_height = 0.8,
          # k_colors = c(1:4,6),
          ggtheme = theme_light(),
          main = "Dendrograma de Produtos - Clientes Não Recorrentes")

fviz_dend(cutted_rec, k = 20, 
          cex = 0.7, 
          type = "rectangle", 
          k_colors = c("darkgreen","orange"),
          labels_track_height = 0.8,
          # k_colors = c(1:4,6),
          ggtheme = theme_light(),
          main = "Dendrograma de Produtos - Clientes Recorrentes")




cutted_not_rec$size
cutted_rec$size














# Heatmap recorrencia: Aisles x Ordem_Carrinho -----------------------------------------

# fazer uma análise pelos produtos que entram primeiro na cesta (por aisle)
ais_ord_cart <- base_ord_geral_prod_not_rec %>% group_by(aisle, add_to_cart_order) %>% 
  summarise(quantidade = n(), 
            recorrencias = sum(reordered)) %>% 
  mutate(rec_perc = recorrencias/quantidade) %>% 
  arrange(-quantidade)

hm1 <- ais_ord_cart %>% ggplot() +
  geom_tile(aes(aisle,add_to_cart_order, fill = rec_perc*100)) +
  scale_fill_gradient2()+
  theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1))
ggplotly(hm1, tooltip = "rec_perc")



# Buscando os principais 100 produtos
lista_produtos <- prod_ord_cart %>% dplyr::group_by(product_name) %>% summarise(recorrencia_media = mean(recorrencias)) %>% dplyr::arrange(-recorrencia_media)

lista_produtos[1:100,1]

hm_prod <- prod_ord_cart %>% dplyr::filter(product_name %in% lista_produtos[1:100,1]) %>% ggplot() +
  geom_tile(aes(product_name,add_to_cart_order, fill = rec_perc*100)) +
  scale_fill_gradient2()+
  theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1))
ggplotly(hm_prod, tooltip = "rec_perc")



# Produtos que entram primeiro na cesta -----------------------------------
# fazer uma análise pelos produtos que entram primeiro na cesta (por nome de produto)
prod_ord_cart <- base_ord_geral_prod %>% group_by(product_name, add_to_cart_order) %>% 
  summarise(quantidade = n(), 
            recorrencias = sum(reordered)) %>% 
  mutate(rec_perc = recorrencias/quantidade) %>% 
  arrange(-quantidade)


# fazer uma análise pelos produtos que entram primeiro na cesta (por aisle)
ais_ord_cart <- base_ord_geral_prod %>% group_by(aisle, add_to_cart_order) %>% 
  summarise(quantidade = n(), 
            recorrencias = sum(reordered)) %>% 
  mutate(rec_perc = recorrencias/quantidade) %>% 
  arrange(-quantidade)


# fazer uma análise pelos produtos que entram primeiro na cesta (por departamento)
dept_ord_cart <- base_ord_geral_prod %>% group_by(department, add_to_cart_order) %>% 
  summarise(quantidade = n(), 
            recorrencias = sum(reordered)) %>% 
  mutate(rec_perc = recorrencias/quantidade) %>% 
  arrange(-quantidade)

# Número máximo de produtos em uma mesma compra
max(prod_ord_cart$add_to_cart_order)
max(base_ord_geral_prod$reordered)


# fazer uma análise pelos produtos que entram por último


# separar por compras de maior numero de produtos

# Seperar por quantidades de produtos comprados em cada compra
# Fazer a relação de quantidade comprada e ordem de inserção na cesta







# Comentários 2020-08-11
# Na verdade, há um alto número de compras com 30 dias desde a última compra, pois o site deixa de registrar após mais de 30 dias

# Na tabela de 'order', verificar que o 'days_since_last_order' = NA, ocorre sempre para a primeira compra. Problema ao buscar o valor máximo, pois NA é maior que qualquer numero.

# Importante considerar que a base não possui somente os últimos 30 dias, porém sim todos as compras, somente não contabilizando intervalos superiores a 30 dias.

# Verificar a soma da diferença entre a o 'days_since_last_order' entre uma compra e a anterior. Se essa soma for 

# Critério para usuário menos recorrente: média móvel (5 compras) de days_since_prior_order > 8 (mediana), não contabilizando a primeira compra pela ocorrencia de NA. (pegar código Vivi)


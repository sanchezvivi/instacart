
# Bibliotecas -------------------------------------------------------------

# biblios <- c('tidyverse','dplyr', 'ggplot2', 'lubridate', 'stringr', 
#              'inspectdf', 'skimr', 'naniar', 'visdat', 'tidymodels', 
#              'klaR', 'corrplot', 'NetCluster', 'factoextra', 'maptree', 'treemap', 'DT','patchwork')

biblios <- c('tidyverse', 'stringr', 'janitor', 'inspectdf', 'dplyr', 'skimr', 'plotly', 'RcppRoll', 'lubridate', 'factoextra', 'forecats')

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

x <- tibble(carac = stringr::str_length(base_aisles$aisle))

x %>% ggplot(aes(carac)) +
  geom_histogram(bins = 20)


# Iniciando Pré-Análises --------------------------------------------------

sum(!(base_orders$order_id %in% base_ord_prior$order_id))

base_orders_rec <- base_orders %>% filter(!is.na(days_since_prior_order))


# Incluindo o produto na base de pedidos anteriores
base_ord_prior %>% left_join(base_products)
base_ord_prior_prod <- base_ord_prior %>% left_join(base_products)
# base_ord_prior_prod <- base_ord_prior_prod[,1:5]

rm(base_ord_)

base_orders_rec_count <- base_orders_rec %>% group_by(user_id) %>% count() %>% transmute(compras = n)

base_orders_rec_count_10 <- base_orders_rec_count %>% filter(compras <= 10)
base_orders_rec_count_10 %>% nrow()

base_orders_rec_count_10_complete <- base_orders_rec_count_10 %>% left_join(base_orders_rec)

base_orders_rec_count_10_complete_prod <- base_orders_rec_count_10_complete %>% left_join(base_ord_prior_prod)

base_orders_rec_count_10_complete_prod_dept <- base_orders_rec_count_10_complete_prod %>% left_join(base_dept)

base_graf1 <- base_orders_rec_count_10_complete_prod_dept %>% group_by(department) %>% count() %>% transmute(quantidade = n) %>% arrange(desc(quantidade))

base_graf1[1:10,] %>% ggplot(aes(x = reorder(department, -quantidade), y = quantidade)) +
                      geom_col(na.rm = TRUE)


x <- base_orders_rec_count %>% filter(compras > 10)

mean(x$compras)

base_orders_rec_count_10_complete$compras %>% mean()


base_orders %>% filter(is.na(days_since_prior_order)) %>% count()


# Analise 2020-08-10 ------------------------------------------------------



## Verificações_Iniciais ---------------------------------------------------


# Verificando se na base_train existem elementos que já estão nas outras bases.
base_orders %>% skim()

base_orders %>% group_by(user_id) %>% summarise(n_pedidos = max(order_number))

n_vezes_mais30 <- base_orders %>% dplyr::filter(days_since_prior_order == 30) %>% group_by(user_id) %>% summarise(n_30 = n())

max(n_vezes_mais30$n_30)

base_orders$days_since_prior_order[base_orders$days_since_prior_order == -1] <- -1

base_orders %>% summary()

base_ord_train %>% skim()

nrow(base_orders)
nrow(base_ord_prior)
nrow(base_ord_train)

sum(base_ord_prior$order_id %in% base_ord_train$order_id)
# Náo há interseção entre os order_id dessas bases

sum(base_ord_prior$order_id %in% base_orders$order_id)
# Há interseção total entre os 'order_id' da tabela 'order_pior' na tabela 'orders'

sum(base_ord_train$order_id %in% base_orders$order_id)
# há interseção total entre os 'order_id' da tabela 'order_train' na tabela 'orders'

orders_in_ord_train <- sum(base_orders$order_id %in% base_ord_train$order_id)

orders_in_ord_prior <- sum(base_orders$order_id %in% base_ord_prior$order_id)

# Ao se buscar os order_id da tabela 'orders' na tabela 'order_prior', encontram-se 3214874 (93,4%).
# Ao se buscar os order_id da tabela 'orders' na tabela 'order_train', encontram-se 131209 (3,84%).
# ou seja, existem 75000 (2,19%) de pedidos da tabela 'orders' que não estão em nenhuma das bases ('order_prior' nem 'order_train'). Esse 'order_id' faltantes, 
# são pertencentes a base 'order_train', que não está disponível.

orders_in_ord_prior/nrow(base_orders)
orders_in_ord_train/nrow(base_orders)

1 - ((orders_in_ord_prior+orders_in_ord_train)/nrow(base_orders))

nrow(base_orders)-(orders_in_ord_prior+orders_in_ord_train)


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


           
# filtrando somente os clientes que estão abaixo da mediana
base_orders_cl_mm <- base_orders_cl_mm %>% arrange(user_id,-order_number)

# users_last_day_ma <- base_orders_cl_mm %>% dplyr::group_by(user_id) %>% summarise(ult_ordem = first(order_number), days_ma = nth(days_ma,3)) %>% glimpse()



base_orders_cl_rec <- base_orders_cl_mm %>% filter(days_ma <8)
base_orders_cl_not_rec <- base_orders_cl_mm %>% filter(days_ma >=8)

base_ord_geral_prod_rec <- base_ord_geral_prod %>% dplyr::filter(order_id %in% base_orders_cl_rec$order_id)
base_ord_geral_prod_not_rec <- base_ord_geral_prod %>% dplyr::filter(order_id %in% base_orders_cl_not_rec$order_id)

# Trazendo a coluna user_id
base_ord_geral_prod_rec2 <- base_ord_geral_prod_rec %>% left_join(base_orders_cl_rec)
base_ord_geral_prod_rec2 <- base_ord_geral_prod_rec2[,c(1:8,10,14)]

base_ord_geral_prod_not_rec2 <- base_ord_geral_prod_not_rec %>% left_join(base_orders_cl_not_rec)
base_ord_geral_prod_not_rec2 <- base_ord_geral_prod_not_rec2[,c(1:8,10,14)]

# Gráfico da média móvel
base_orders_cl_not_rec2 %>% 
  na.omit() %>% 
  ggplot(aes(x = days_ma)) +
  geom_bar(fill = 'darkgreen') +
  geom_vline(xintercept = 8, color = 'orange', 
             linetype = 'dashed') +
  theme_minimal()


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


order_n_total %>% ggplot() +
  geom_histogram(aes(x = quant_prod), bins = bin/10,) +
  scale_y_continuous(trans = scales::trans_new(name = "sqrt_sqrt",transform = x_4, inverse = x4)) +
  labs(title = "Histograma de No Produtos comprados") 

order_n_total %>% ggplot(aes(x = quant_prod)) +
  geom_freqpoly(bins = bin/10) +
  scale_y_continuous(trans = scales::trans_new(name = "sqrt_sqrt",transform = x_4, inverse = x4)) +
  labs(title = "Histograma de No Produtos comprados")

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
  scale_fill_gradient2(low = "white", high = "darkgreen", limits = c(0,40),trans = scales::trans_new(name = "quad",transform = x2, inverse = x_2))+
  theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1)) +
  labs(title = "Heatmap de Produtos x Cart_Order para clientes Não-Recorrentes") +
  theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
  geom_hline(yintercept = n_prod1, color = "orange") +
  scale_y_continuous(limits = c(0,20),expand = c(0,0)) +
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


prod_ord_cart_rec2[prod_ord_cart_rec2$product_name == 'Banana']
prod_ord_cart_rec2 <- prod_ord_cart_rec %>% dplyr::group_by(product_name) %>% mutate(perc = recorrencias/sum(recorrencias))

prod_ord_cart_rec2_list <- prod_ord_cart_rec2 %>% group_by(product_name) %>% summarise(recorrencias_total = sum(recorrencias)) %>% arrange(-recorrencias_total)

prod_ord_cart_rec2_list <- prod_ord_cart_rec2_list[1:100,1]

prod_100_rec <- prod_ord_cart_rec2 %>% right_join(prod_ord_cart_rec2_list)

prod_100_rec %>% ggplot() +
  geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
  # scale_fill_gradient2(aes(fill = "darkgreen"))+
  scale_fill_gradient2(low = "white", high = "darkgreen", limits = c(0,40), trans = scales::trans_new(name = "quad",transform = x2, inverse = x_2))+
  theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1)) +
  labs(title = "Heatmap de Produtos x Cart_Order para clientes Recorrentes") +
  theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
  geom_hline(yintercept = n_prod2, color = "orange") +
  geom_text(aes(x = 5, y = n_prod2+0.1, label = texto2 ), size = 3, color = 'orange', hjust = 0, vjust = 0) +
  scale_y_continuous(limits = c(0,20),expand = c(0,0))



colors()
# hm1 <- prod_100_rec %>% ggplot() +
#   geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
#   scale_fill_gradient2()+
#   ylim(0,150)+
#   theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1)) +
#   labs(title = "Heatmap de Produtos x Cart_Order para clientes Recorrentes")
# ggplotly(hm1, tooltip = "perc")




# Análise de HClust -------------------------------------------------------

# Montando um hclust de produto por order de carrinho para produtos de clientes pouco recorrentes
mat_similarity <- prod_ord_cart2 %>% dplyr::select(product_name,add_to_cart_order, perc) %>% pivot_wider(names_from = add_to_cart_order, values_from = perc)
mat_similarity2 <- mat_similarity[1:200,]

# Montando um hclust de produto por order de carrinho para produtos de clientes MAIS recorrentes
mat_similarity_rec <- prod_ord_cart_rec2 %>% dplyr::select(product_name,add_to_cart_order, perc) %>% pivot_wider(names_from = add_to_cart_order, values_from = perc)
mat_similarity_rec2 <- mat_similarity_rec[1:200,]

# vet_clust <- c(1:((nrow(mat_similarity2)-2)/20))
vet_clust <- c(1:9)
vet_clust <- vet_clust * 20
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


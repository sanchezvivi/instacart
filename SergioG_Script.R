
# Bibliotecas -------------------------------------------------------------

# biblios <- c('tidyverse','dplyr', 'ggplot2', 'lubridate', 'stringr', 
#              'inspectdf', 'skimr', 'naniar', 'visdat', 'tidymodels', 
#              'klaR', 'corrplot', 'NetCluster', 'factoextra', 'maptree', 'treemap', 'DT','patchwork')

biblios <- c('tidyverse', 'stringr', 'janitor', 'inspectdf', 'dplyr', 'skimr')

for (i in biblios){
  if(!require(i, character.only = TRUE)){install.packages(paste0(i)); library(i, character.only = TRUE)}
}

# Importando os dados em .csv, usando o read.csv --------------------------

path <- "data\\"
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

# Verificando se na base_train existem elementos que já estão nas outras bases.
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
base_ord_geral <- union(base_ord_prior,base_ord_train)

# Fazendo um left join da base de 'base_prod' com a base de base_aisles e base_dept, para trazer os nomes dos corredores e departamentos
base_products_names <- base_products %>% left_join(base_aisles) %>% left_join(base_dept)
base_products_names <- base_products_names[,c(1:2,5:6)]


# Fazendo um left join da base de order_geral com a base_products_names, para trazer dados dos produtos comprados (nome_produto, corredor e departamento)
base_ord_geral_prod <- base_ord_geral %>% left_join(base_products_names)




# Histograma de Produtos comprados por Ordem ------------------------------
order_n_total <- base_ord_geral_prod %>% group_by(order_id) %>% summarise(quant_prod = n(), unid_recompra = sum(reordered))

bin <- order_n_total$quant_prod %>% max()
order_n_total %>% ggplot(aes(x = quant_prod)) +
                    geom_histogram(bins = bin/10) +
                    scale_y_sqrt()
x4 <- function(x) x^4

x_4<- function(x) sqrt(sqrt(x))

order_n_total %>% ggplot() +
  geom_histogram(aes(x = quant_prod), bins = bin/10,) +
  scale_y_continuous(trans = scales::trans_new(name = "sqrt_sqrt",transform = x_4, inverse = x4)) +
  labs(title = "Histograma de No Produtos comprados") +
  geom_text(aes(label = ))

order_n_total %>% ggplot(aes(x = quant_prod)) +
  geom_freqpoly(bins = bin/10) +
  scale_y_continuous(trans = scales::trans_new(name = "sqrt_sqrt",transform = x_4, inverse = x4)) +
  labs(title = "Histograma de No Produtos comprados")

# Produtos Mais Recorrentes ----------------------------------------------------
# em qual posição do carrinho, se localizam o produtos MAIS recorrentes
rec_ord_cart <- base_ord_geral_prod %>% group_by(add_to_cart_order) %>% 
                                          summarise(recorrencias = sum(reordered),
                                                    total = n()) %>% 
                                          mutate(rec_perc = recorrencias/total) %>% 
                                          arrange(add_to_cart_order)

rec_ord_cart %>% ggplot(aes(add_to_cart_order, rec_perc)) +
                  geom_col()
  

# Produtos Menos Recorrentes ----------------------------------------------------
# em qual posição do carrinho, se localizam o produtos MENOS recorrentes
rec_ord_cart <- base_ord_geral_prod %>% group_by(add_to_cart_order) %>% 
  summarise(total = n(),
            nao_recorrencia = total - sum(reordered)) %>% 
  mutate(nao_rec_perc = nao_recorrencia/total) %>% 
  arrange(add_to_cart_order)

rec_ord_cart %>% ggplot(aes(add_to_cart_order, nao_rec_perc)) +
  geom_col()



# Heatmap recorrencia: Aisles x Ordem_Carrinho -----------------------------------------

# fazer uma análise pelos produtos que entram primeiro na cesta (por aisle)
ais_ord_cart <- base_ord_geral_prod %>% group_by(aisle, add_to_cart_order) %>% 
  summarise(quantidade = n(), 
            recorrencias = sum(reordered)) %>% 
  mutate(rec_perc = recorrencias/quantidade) %>% 
  arrange(-quantidade)

ais_ord_cart %>% ggplot() +
                  geom_tile(aes(aisle,add_to_cart_order))
# mudar cor e escala do gráfico


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
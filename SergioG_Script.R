
# Bibliotecas -------------------------------------------------------------

# biblios <- c('tidyverse','dplyr', 'ggplot2', 'lubridate', 'stringr', 
#              'inspectdf', 'skimr', 'naniar', 'visdat', 'tidymodels', 
#              'klaR', 'corrplot', 'NetCluster', 'factoextra', 'maptree', 'treemap', 'DT','patchwork')

biblios <- c('tidyverse', 'stringr', 'janitor', 'inspectdf', 'dplyr')

for (i in biblios){
  if(!require(i, character.only = TRUE)){install.packages(?paste0(i)); library(i, character.only = TRUE)}
}
library(dplyr)
# Importando os dados em .csv, usando o read.csv --------------------------

path <- "data\\"
file_aisles <- "aisles.csv"
base_aisles <- read.csv(paste(path,file_aisles,sep = ""))

file_dept <- "departments.csv"
base_dept <- read.csv(paste(path,file_dept,sep = ""))

file_ord_prior <- "order_products__prior.csv"
base_ord_prior <- read.csv(paste(path,file_ord_prior,sep = "")) %>% glimpse()
base_ord_prior_mini <- sample_frac(base_ord_prior, size = 0.01)

file_ord_train <- "order_products__train.csv"
base_ord_train <- read.csv(paste(path,file_ord_train,sep = "")) %>% glimpse()

file_orders <- "orders.csv"
base_orders <- read.csv(paste(path,file_orders,sep = "")) %>% glimpse()
head(base_orders)

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




inspectdf::inspect_num()

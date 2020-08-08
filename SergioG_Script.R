
# Bibliotecas -------------------------------------------------------------

biblios <- c('tidyverse', 'stringr')

for (i in biblios){
  if(!require(i, character.only = TRUE)){install.packages(?paste0(i)); library(i, character.only = TRUE)}
}

# Importando os dados em .csv, usando o read.csv --------------------------

path <- "..\\Data\\instacart_2017_05_01\\"
file_aisles <- "aisles.csv"
base_aisles <- read.csv(paste(path,file_aisles,sep = ""))

file_dept <- "departments.csv"
base_aisles <- read.csv(paste(path,file_dept,sep = ""))

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


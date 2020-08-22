#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

biblios <- c('tidyverse', 'stringr', 'janitor', 'inspectdf', 'dplyr', 'skimr', 
             'plotly', 'RcppRoll', 'lubridate', 'factoextra', 'tidymodels', 'tidytext')

library(tidymodels)
library(tidyverse)

for (i in biblios){
    if(!require(i, character.only = TRUE)){install.packages(paste0(i)); library(i, character.only = TRUE)}
}


theme_set(theme_minimal())
theme_update(text = element_text(family = "Brandon Text"),
             plot.title = element_text(face = "bold"))


source('..\\instacart_palette.R')

# Importando os dados em .csv, usando o read.csv --------------------------

#path <- "data\\"
path <- "..\\data\\"
# file_aisles <- "aisles.csv"
# base_aisles <- read.csv(paste(path,file_aisles,sep = ""))
# 
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
# base_orders_cl <- base_orders %>% filter(eval_set != 'test')
# 
# base_orders_cl %>% group_by(user_id) %>% summarise(cnt = n())
# 
# 
# 
# # Mesclando as bases 'order_prior' e 'order_train'
# base_ord_geral <- dplyr::union(base_ord_prior,base_ord_train)
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
# # Filtro Média Móvel Vivi -------------------------------------------------
# 
# base_orders_cl_mm <- base_orders_cl %>% 
#     filter(order_number != 1) %>% 
#     arrange(user_id, order_number) %>% 
#     mutate(order_hour_of_day = as.numeric(order_hour_of_day)) %>% 
#     group_by(user_id) %>% 
#     mutate(days_ma = roll_mean(days_since_prior_order, 5, fill = NA, na.rm = T)) %>% 
#     ungroup() %>% 
#     glimpse
# 
# # Gráfico da média móvel
# # base_orders_cl_not_rec2 %>%
# #   na.omit() %>%
# #   ggplot(aes(x = days_ma)) +
# #   geom_bar(fill = 'darkgreen') +
# #   geom_vline(xintercept = 8, color = 'orange',
# #              linetype = 'dashed') +
# #   theme_minimal()
# 
# 
# 
# # Filtrando as bases para trazer as ordens dos 2 grupos -------------------
# 
# 
# # filtrando somente os clientes que estão abaixo da mediana
# base_orders_cl_mm <- base_orders_cl_mm %>% arrange(user_id,-order_number)
# 
# users_churn <- base_orders_cl_mm %>% 
#     dplyr::group_by(user_id) %>% 
#     summarise(ult_ordem = first(order_number), days_ma_new = nth(days_ma,3), 
#               media_days = mean(days_since_prior_order)) %>% 
#     filter(days_ma_new == 30 | (is.na(days_ma_new) & media_days >= 25)) %>% glimpse()
# 
# users_rec <- base_orders_cl_mm %>% dplyr::group_by(user_id) %>% 
#     summarise(ult_ordem = first(order_number), days_ma_new = nth(days_ma,3), 
#               media_days = mean(days_since_prior_order)) %>% 
#     filter(days_ma_new < 8) %>% glimpse()
# 
# base_orders_cl_rec <- base_orders_cl_mm %>% right_join(users_rec)
# base_orders_cl_not_rec <- base_orders_cl_mm %>% right_join(users_churn)
# 
# base_ord_geral_prod_rec <- base_ord_geral_prod %>% dplyr::filter(order_id %in% base_orders_cl_rec$order_id)
# base_ord_geral_prod_not_rec <- base_ord_geral_prod %>% dplyr::filter(order_id %in% base_orders_cl_not_rec$order_id)
# 
# 
# 
# # Gráficos Aisles ---------------------------------------------------------
# 
# # # Gráficos de Departamento e corredor
# # base_ord_geral_prod_rec %>% 
# #     group_by(aisle) %>% 
# #     summarise(rec = sum(reordered)/sum(base_ord_geral_prod_rec$reordered)) %>% 
# #     arrange(-rec) %>% 
# #     top_n(20) %>% 
# #     ggplot(aes(reorder(aisle, -rec), rec*100)) +
# #     theme_minimal() +
# #     geom_col(fill = "darkgreen")+
# #     labs(x = "Corredor", y = "Quantidade(%)", title = "Perfil Compra Recorr - Corredores")+
# #     scale_y_continuous(expand = c(0,0), limits = c(0,15)) +
# #     theme(axis.text.x = element_text(angle = 90, color = "darkorange", size = 12))
# # 
# # base_ord_geral_prod_not_rec %>% 
# #     group_by(aisle) %>% 
# #     summarise(rec = sum(reordered)/sum(base_ord_geral_prod_not_rec$reordered)) %>% 
# #     arrange(-rec) %>% 
# #     top_n(20) %>% 
# #     ggplot(aes(reorder(aisle, -rec), rec*100)) +
# #     theme_minimal() +
# #     geom_col(fill = "darkgreen")+
# #     labs(x = "Corredor", y = "Quantidade(%)", title = "Perfil Compra Churn - Corredores")+
# #     scale_y_continuous(expand = c(0,0), limits = c(0,15)) +
# #     theme(axis.text.x = element_text(angle = 90, color = "darkorange", size = 12))
# 
# 
# 
# 
# 
# 
# 
# # Dados de Capa Estáticos -------------------------------------------------
# 
# # Média de compras
# base_orders_cl_rec %>% group_by(user_id) %>% summarise(media_compras = mean(n())) %>% skim()
# 
# base_orders_cl_not_rec %>% group_by(user_id) %>% summarise(media_compras = mean(n())) %>% skim()
# 
# 
# # Dias entre Compras - Média
# 
# base_orders_cl_rec %>% group_by(user_id) %>% summarise(media_compras = mean(days_since_prior_order)) %>% skim()
# 
# base_orders_cl_not_rec %>% group_by(user_id) %>% summarise(media_compras = mean(days_since_prior_order)) %>% skim()
# 
# base_ord_geral_prod_not_rec
# 
# 
# 
# # Trazendo a coluna user_id
# base_ord_geral_prod_rec2 <- base_ord_geral_prod_rec %>% left_join(base_orders_cl_rec)
# base_ord_geral_prod_rec2 <- base_ord_geral_prod_rec2[,c(1:8,10,14)]
# 
# base_ord_geral_prod_not_rec2 <- base_ord_geral_prod_not_rec %>% left_join(base_orders_cl_not_rec)
# base_ord_geral_prod_not_rec2 <- base_ord_geral_prod_not_rec2[,c(1:8,10,14)]
# 
# 
# # HIPOTESE
# # Compras que tem recorrência, provavelmente é feita, repetindo uma cesta anterior.
# # Compras com menor recorrência tem maior variaçao na cesta de compras
# 
# 
# # Rodando o modelo para os cem principais recorrentes e os 100 piores recorrentes
# 
# 
# 
# # Histograma de Produtos comprados por Ordem ------------------------------
# order_n_total <- base_ord_geral_prod_not_rec %>% group_by(order_id) %>% summarise(quant_prod = n(), unid_recompra = sum(reordered))
# 
x4 <- function(x) x^4

x_4<- function(x) sqrt(sqrt(x))

x2 <- function(x) x^2

x_2<- function(x) sqrt(x)
# 
# # # Produtos Mais Recorrentes ----------------------------------------------------
# # # em qual posição do carrinho, se localizam o produtos MAIS recorrentes
# # rec_ord_cart <- base_ord_geral_prod_rec2 %>% group_by(add_to_cart_order) %>% 
# #     summarise(recorrencias = sum(reordered),
# #               total = n()) %>% 
# #     mutate(rec_perc = recorrencias/total) %>% 
# #     arrange(add_to_cart_order)
# # 
# # rec_ord_cart %>% ggplot(aes(add_to_cart_order, rec_perc)) +
# #     geom_col() +
# #     labs(title = "Gráfico de ordem_carrinho x percentual de produtos recorrentes")
# # 
# # 
# # # Produtos Menos Recorrentes ----------------------------------------------------
# # # em qual posição do carrinho, se localizam o produtos não-recorrentes
# # nao_rec_ord_cart <- base_ord_geral_prod_not_rec2 %>% group_by(add_to_cart_order) %>% 
# #     summarise(total = n(),
# #               nao_recorrencia = total - sum(reordered)) %>% 
# #     mutate(nao_rec_perc = nao_recorrencia/total) %>% 
# #     arrange(add_to_cart_order)
# # 
# # nao_rec_ord_cart %>% ggplot(aes(add_to_cart_order, nao_rec_perc)) +
# #     geom_col() +
# #     labs(title = "Gráfico de ordem_carrinho x percentual de produtos nao_recorrentes")
# 
# 
# 
# 
# # Heatmaps Integradora Intermediária --------------------------------------
# # fazer uma análise pelos produtos que entram primeiro na cesta (por produto), nas compras feitas por clientes pouco recorrentes.
# 
# # HIPÓTESE:
# # Existe uma relação entre a posição do produto no carrinho e a recorrência de compra
# 
# # PREMISSA DA ANÁLISE:
# # Separar os clientes em 2 catergorias: Clientes muito recorrentes e clientes pouco recorrentes.
# # Essa definição inicial é feita com base na variável 'days_since_pior_order'. 
# # Primeiro se calcula o valor médio dessa variável, por user_id.
# # Uma vez definidas os valores médios por cliente, calculam-se os quartis.
# # São definidos como clientes pouco recorrentes, o que estão abaixo da madiana e Clientes recorrentes, aqueles que estão acima da mediana.
# # Definido isso, será feita uma análise para cada um dos grupos de modo a buscar as discrepâncias.
# 
# # GRÁFICOS:
# # Foram criados 2 Heatmaps, um para cada grupo de clientes, onde são apresentados os percentuais de recorrencias de produtos (100 produtos de maior 
# # recorrência de compra), nas diferentes posições do carrinho de compras.
# 
# # INSIGHT:
# # Comparando ambos os gráficos, não se percebem relevantes variações nem nos produtos apresentados, nem tampouco na proporção do produto nas diversas posições.
# 
# prod_ord_cart <- base_ord_geral_prod_not_rec2 %>% dplyr::group_by(product_name, add_to_cart_order) %>% 
#     summarise(quantidade = n(), 
#               recorrencias = sum(reordered)) %>% 
#     mutate(rec_perc = recorrencias/quantidade) %>% 
#     arrange(-quantidade)
# 
# # Definindo a média do número de produtos recorrentes
# a <- base_ord_geral_prod_not_rec2$order_id %>% n_distinct() #número de pedidos
# b <- base_ord_geral_prod_not_rec2$reordered %>% sum() #numero de produtos
# n_prod1 = b/a
# texto1 <- paste("Média Produtos/Ordem = ", round(n_prod1,2), sep = "")
# 
# prod_ord_cart2 <- prod_ord_cart %>% dplyr::group_by(product_name) %>% mutate(perc = recorrencias/sum(recorrencias))
# 
# prod_ord_cart2_list <- prod_ord_cart2 %>% group_by(product_name) %>% summarise(recorrencias_total = sum(recorrencias)) %>% arrange(-recorrencias_total)
# 
# 
# # fazer uma análise pelos produtos que entram primeiro na cesta (por produto), nas compras feitas por clientes MAIS recorrentes.
# prod_ord_cart_rec <- base_ord_geral_prod_rec2 %>% dplyr::group_by(product_name, add_to_cart_order) %>% 
#     summarise(quantidade = n(), 
#               recorrencias = sum(reordered)) %>% 
#     mutate(rec_perc = recorrencias/quantidade) %>% 
#     arrange(-quantidade)
# 
# # Definindo a média do número de produtos recorrentes
# a <- base_ord_geral_prod_rec2$order_id %>% n_distinct() #número de pedidos
# b <- base_ord_geral_prod_rec2$reordered %>% sum() #numero de produtos
# n_prod2 = b/a
# texto2 <- paste("Média Produtos/Ordem = ", round(n_prod2,2), sep = "")
# 
# 
# prod_ord_cart_rec2 <- prod_ord_cart_rec %>% dplyr::group_by(product_name) %>% mutate(perc = recorrencias/sum(recorrencias))
# 
# prod_ord_cart_rec2_list <- prod_ord_cart_rec2 %>% group_by(product_name) %>% summarise(recorrencias_total = sum(recorrencias)) %>% arrange(-recorrencias_total)

prod_ord_cart2 <- read.csv(paste(path, "prod_ord_cart2.csv", sep = "")) %>% as.tibble()

prod_ord_cart_rec2 <- read.csv(paste(path, "prod_ord_cart_rec2.csv", sep = "")) %>% as.tibble()

prod_ord_cart2_list <- read.csv(paste(path, "prod_ord_cart2_list.csv", sep = "")) %>% as.tibble()

prod_ord_cart_rec2_list <- read.csv(paste(path, "prod_ord_cart_rec2_list.csv", sep = "")) %>% as.tibble()


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$heatmap1 <- renderPlot({
        prod_ord_cart2_list2 <- prod_ord_cart2_list[1:input$slider_n_prod,1]
        prod_100_n_rec <- prod_ord_cart2 %>% right_join(prod_ord_cart2_list2)
        
        # "1", "x^2", "sqrt2", "x^4", "sqrt4"
        vline_size <- 5
        label_size <- min(7 + (2 * 100/input$slider_n_prod),12)
        n_prod1 <- 3.32
        texto1 <- "Média Produtos/Ordem = 3,32"
        if (input$funcao == "1"){
            prod_100_n_rec %>% ggplot() +
                geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
                scale_fill_gradient2(low = "white", 
                                     high = "darkgreen", 
                                     limits = c(0,40))+#,trans = scales::trans_new(name = "quad",transform = x2, inverse = x_2))+
                theme(axis.text.x = element_text(angle = 90, size = label_size, hjust = 1)) +
                labs(title = "Heatmap de Produtos x Cart_Order para clientes Churn", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
                theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
                geom_hline(yintercept = n_prod1, color = "orange") +
                scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
                geom_text(aes(x = 5, y = n_prod1+0.1, label = texto1 ), size = vline_size, color = 'orange', hjust = 0, vjust = 0)
        } else {
            if (input$funcao == "x^2"){
                prod_100_n_rec %>% ggplot() +
                    geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
                    scale_fill_gradient2(low = "white", 
                                         high = "darkgreen", 
                                         limits = c(0,40),trans = scales::trans_new(name = "quad",transform = x2, inverse = x_2))+
                    theme(axis.text.x = element_text(angle = 90, size = label_size, hjust = 1)) +
                    labs(title = "Heatmap de Produtos x Cart_Order para clientes Churn", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
                    theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
                    geom_hline(yintercept = n_prod1, color = "orange") +
                    scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
                    geom_text(aes(x = 5, y = n_prod1+0.1, label = texto1 ), size = vline_size, color = 'orange', hjust = 0, vjust = 0)
            } else {
                if (input$funcao == "sqrt2"){
                    prod_100_n_rec %>% ggplot() +
                        geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
                        scale_fill_gradient2(low = "white", 
                                             high = "darkgreen", 
                                             limits = c(0,40),trans = scales::trans_new(name = "quad",transform = x_2, inverse = x2))+
                        theme(axis.text.x = element_text(angle = 90, size = label_size, hjust = 1)) +
                        labs(title = "Heatmap de Produtos x Cart_Order para clientes Churn", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
                        theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
                        geom_hline(yintercept = n_prod1, color = "orange") +
                        scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
                        geom_text(aes(x = 5, y = n_prod1+0.1, label = texto1 ), size = vline_size, color = 'orange', hjust = 0, vjust = 0)
                } else {
                    if (input$funcao == "x^4"){
                        prod_100_n_rec %>% ggplot() +
                            geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
                            scale_fill_gradient2(low = "white", 
                                                 high = "darkgreen", 
                                                 limits = c(0,40),trans = scales::trans_new(name = "quad",transform = x4, inverse = x_4))+
                            theme(axis.text.x = element_text(angle = 90, size = label_size, hjust = 1)) +
                            labs(title = "Heatmap de Produtos x Cart_Order para clientes Churn", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
                            theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
                            geom_hline(yintercept = n_prod1, color = "orange") +
                            scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
                            geom_text(aes(x = 5, y = n_prod1+0.1, label = texto1 ), size = vline_size, color = 'orange', hjust = 0, vjust = 0)
                    } else {
                        if (input$funcao == "sqrt4"){
                            prod_100_n_rec %>% ggplot() +
                                geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
                                scale_fill_gradient2(low = "white", 
                                                     high = "darkgreen", 
                                                     limits = c(0,40),trans = scales::trans_new(name = "quad",transform = x_4, inverse = x4))+
                                theme(axis.text.x = element_text(angle = 90, size = label_size, hjust = 1)) +
                                labs(title = "Heatmap de Produtos x Cart_Order para clientes Churn", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
                                theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
                                geom_hline(yintercept = n_prod1, color = "orange") +
                                scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
                                geom_text(aes(x = 5, y = n_prod1+0.1, label = texto1 ), size = vline_size, color = 'orange', hjust = 0, vjust = 0)
                        }
                    }
                }
            }
        }


    })
    output$heatmap2 <- renderPlot({
        prod_ord_cart_rec2_list <- prod_ord_cart_rec2_list[1:input$slider_n_prod,1]
        
        prod_100_rec <- prod_ord_cart_rec2 %>% right_join(prod_ord_cart_rec2_list)
        
        # prod_100_rec %>% ggplot() +
        #     geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
        #     # scale_fill_gradient2(aes(fill = "darkgreen"))+
        #     scale_fill_gradient2(low = "white", high = "darkgreen", limits = c(0,40))+ #, trans = scales::trans_new(name = "quad",transform = x2, inverse = x_2))+
        #     theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1)) +
        #     labs(title = "Heatmap de Produtos x Cart_Order para clientes Recorrentes", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
        #     theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
        #     geom_hline(yintercept = n_prod2, color = "orange") +
        #     geom_text(aes(x = 5, y = n_prod2+0.1, label = texto2 ), size = 3, color = 'orange', hjust = 0, vjust = 0) +
        #     scale_y_continuous(limits = c(0,30),expand = c(0,0))
        vline_size <- 5
        n_prod2 <- 7.13
        label_size <- min(7 + (2 * 100/input$slider_n_prod),12)
        texto2 <- "Média Produtos/Ordem = 7,13"
        if (input$funcao == "1"){
            prod_100_rec %>% ggplot() +
                geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
                scale_fill_gradient2(low = "white", 
                                     high = "darkgreen", 
                                     limits = c(0,40))+#,trans = scales::trans_new(name = "quad",transform = x2, inverse = x_2))+
                theme(axis.text.x = element_text(angle = 90, size = label_size, hjust = 1)) +
                labs(title = "Heatmap de Produtos x Cart_Order para clientes Recorrentes", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
                theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
                geom_hline(yintercept = n_prod2, color = "orange") +
                scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
                geom_text(aes(x = 5, y = n_prod2+0.1, label = texto2 ), size = vline_size, color = 'orange', hjust = 0, vjust = 0)
        } else {
            if (input$funcao == "x^2"){
                prod_100_rec %>% ggplot() +
                    geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
                    scale_fill_gradient2(low = "white", 
                                         high = "darkgreen", 
                                         limits = c(0,40),
                                         trans = scales::trans_new(name = "quad",transform = x2, inverse = x_2))+
                    theme(axis.text.x = element_text(angle = 90, size = label_size, hjust = 1)) +
                    labs(title = "Heatmap de Produtos x Cart_Order para clientes Recorrentes", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
                    theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
                    geom_hline(yintercept = n_prod2, color = "orange") +
                    scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
                    geom_text(aes(x = 5, y = n_prod2+0.1, label = texto2 ), size = vline_size, color = 'orange', hjust = 0, vjust = 0)
            } else {
                if (input$funcao == "sqrt2"){
                    prod_100_rec %>% ggplot() +
                        geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
                        scale_fill_gradient2(low = "white", 
                                             high = "darkgreen", 
                                             limits = c(0,40),trans = scales::trans_new(name = "quad",transform = x_2, inverse = x2))+
                        theme(axis.text.x = element_text(angle = 90, size = label_size, hjust = 1)) +
                        labs(title = "Heatmap de Produtos x Cart_Order para clientes Recorrentes", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
                        theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
                        geom_hline(yintercept = n_prod2, color = "orange") +
                        scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
                        geom_text(aes(x = 5, y = n_prod2+0.1, label = texto2 ), size = vline_size, color = 'orange', hjust = 0, vjust = 0)
                } else {
                    if (input$funcao == "x^4"){
                        prod_100_rec %>% ggplot() +
                            geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
                            scale_fill_gradient2(low = "white", 
                                                 high = "darkgreen", 
                                                 limits = c(0,40),trans = scales::trans_new(name = "quad",transform = x4, inverse = x_4))+
                            theme(axis.text.x = element_text(angle = 90, size = label_size, hjust = 1)) +
                            labs(title = "Heatmap de Produtos x Cart_Order para clientes Recorrentes", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
                            theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
                            geom_hline(yintercept = n_prod2, color = "orange") +
                            scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
                            geom_text(aes(x = 5, y = n_prod2+0.1, label = texto2 ), size = vline_size, color = 'orange', hjust = 0, vjust = 0)
                    } else {
                        if (input$funcao == "sqrt4"){
                            prod_100_rec %>% ggplot() +
                                geom_tile(aes(product_name,add_to_cart_order, fill = perc*100)) +
                                scale_fill_gradient2(low = "white", 
                                                     high = "darkgreen", 
                                                     limits = c(0,40),trans = scales::trans_new(name = "quad",transform = x_4, inverse = x4))+
                                theme(axis.text.x = element_text(angle = 90, size = label_size, hjust = 1)) +
                                labs(title = "Heatmap de Produtos x Cart_Order para clientes Recorrentes", fill = "%", x = "Produto", y = "Ordem_Carrinho") +
                                theme(axis.text.x = element_text(hjust = 1.0, vjust = 0.3)) + 
                                geom_hline(yintercept = n_prod2, color = "orange") +
                                scale_y_continuous(limits = c(0,30),expand = c(0,0)) +
                                geom_text(aes(x = 5, y = n_prod2+0.1, label = texto2 ), size = vline_size, color = 'orange', hjust = 0, vjust = 0)
                        }
                    }
                }
            }
        }
        
        
    })
    # output$bar_graph <- renderPlot({
    #     base_price <- base %>% filter(Sale_Price >= as.integer(input$slider_price[1]) & Sale_Price <= as.integer(input$slider_price[2])) %>% group_by(Neighborhood) %>% count() %>% as.data.frame()
    #     
    #     base_price %>% ggplot(aes(Neighborhood)) + 
    #         geom_col(aes(y = n, fill = Neighborhood)) +
    #         coord_flip()
    # })
    # observeEvent(input$btn_update,{v$data <- input$list_decade2})
    # output$map_out <- renderLeaflet({
    #     min_lat <- min(base$Latitude)
    #     max_lat <- max(base$Latitude)
    #     min_lng <- min(base$Longitude)
    #     max_lng <- max(base$Longitude)
    #     m <- leaflet() %>% 
    #         addTiles() %>% 
    #         fitBounds(lat1 = min_lat, lat2 = max_lat, lng1 = min_lng, lng2 = max_lng)
    #     base_ano_cum <- base %>% filter(Year_Built <= as.integer(v$data)+9)
    #     m %>% addMarkers(lng = base_ano_cum$Longitude, lat = base_ano_cum$Latitude)
    # })
    
})

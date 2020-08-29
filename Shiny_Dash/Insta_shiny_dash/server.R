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


source('..\\..\\instacart_palette.R')

# Importando os dados em .csv, usando o read.csv --------------------------

#path <- "data\\"
path <- "..\\..\\data\\"

x4 <- function(x) x^4

x_4<- function(x) sqrt(sqrt(x))

x2 <- function(x) x^2

x_2<- function(x) sqrt(x)

prod_ord_cart2 <- read.csv(paste(path, "prod_ord_cart2.csv", sep = "")) %>% as.tibble()

prod_ord_cart_rec2 <- read.csv(paste(path, "prod_ord_cart_rec2.csv", sep = "")) %>% as.tibble()

prod_ord_cart2_list <- read.csv(paste(path, "prod_ord_cart2_list.csv", sep = "")) %>% as.tibble()

prod_ord_cart_rec2_list <- read.csv(paste(path, "prod_ord_cart_rec2_list.csv", sep = "")) %>% as.tibble()

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
            prod_100_n_rec %>% # mutate(product_name = reorder_within(product_name, abs(perc), add_to_cart_order)) %>%  
                ggplot() +
                geom_tile(aes(reorder(product_name, desc(recorrencias), FUN = mean), add_to_cart_order, fill = perc*100)) +
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
                    geom_tile(aes(reorder(product_name, desc(recorrencias), FUN = mean),add_to_cart_order, fill = perc*100)) +
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
                        geom_tile(aes(reorder(product_name, desc(recorrencias), FUN = mean),add_to_cart_order, fill = perc*100)) +
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
                            geom_tile(aes(reorder(product_name, desc(recorrencias), FUN = mean),add_to_cart_order, fill = perc*100)) +
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
                                geom_tile(aes(reorder(product_name, desc(recorrencias), FUN = mean),add_to_cart_order, fill = perc*100)) +
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
                geom_tile(aes(reorder(product_name, desc(recorrencias), FUN = mean),add_to_cart_order, fill = perc*100)) +
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
                    geom_tile(aes(reorder(product_name, desc(recorrencias), FUN = mean),add_to_cart_order, fill = perc*100)) +
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
                        geom_tile(aes(reorder(product_name, desc(recorrencias), FUN = mean),add_to_cart_order, fill = perc*100)) +
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
                            geom_tile(aes(reorder(product_name, desc(recorrencias), FUN = mean),add_to_cart_order, fill = perc*100)) +
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
                                geom_tile(aes(reorder(product_name, desc(recorrencias), FUN = mean),add_to_cart_order, fill = perc*100)) +
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



# 
# library(shiny)
# 
# # Define server logic required to draw a histogram
# shinyServer(function(input, output) {
# 
#     output$distPlot <- renderPlot({
# 
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
# 
#     })
# 
# })

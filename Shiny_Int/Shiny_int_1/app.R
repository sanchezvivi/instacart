#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

print(getwd())
source('SergioG_Script5.R')

source('..\\..\\instacart_palette.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Instacart"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("k",
                        "Número de Clusters:",
                        min = 1,
                        max = 10,
                        value = 4)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("spider", dblclick = dblclickOpts(
               id = "plot_dblclick"
           )),
           verbatimTextOutput("txt_out"),
           d3tree3Output("detail")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$spider <- renderPlot({
        # generate bins based on input$bins from ui.R
        clust_kmean <- base_k_user[,c(2:ncol(base_k_user))] %>% scale() %>% hkmeans(k = input$k)
        
        
        # Calculando os perfis dos clusters
        # dados_clusters <- (clust_kmean$centers * (clust_kmean$data %>% attr("scaled:scale"))) + (clust_kmean$data %>% attr("scaled:center"))
        
        dados_clusters <- clust_kmean$centers
        
        # A partir da análise é possível definir o perfil dos clusters, para poder dar um nome aos mesmos
        # dados_clusters <- tibble(cluster = rownames(dados_clusters), nome_cluster = c("Compra_Quant","Churn","Fiel_e_Recorrente","Novo_com_Potencial")) %>% bind_cols(as_tibble(dados_clusters))
        dados_clusters <- tibble(cluster = rownames(dados_clusters)) %>% bind_cols(as_tibble(dados_clusters))
        
        spider <- dados_clusters %>% ggRadar(aes(x = c(n_compras,
                                                       t_mean,
                                                       mean_prod_cart,
                                                       mean_peso_cart,
                                                       mean_rec_fat),
                                                 facet = cluster), 
                                             interactive = F,
                                             size = 1.5,
                                             legend.position = "right"
                                             )
        spider
    })
    output$txt_out <- renderPrint({
        (clust_kmean$centers * (clust_kmean$data %>% attr("scaled:scale"))) 
                            + (clust_kmean$data %>% attr("scaled:center")) %>% 
                                filter(cluster = input$plot_dblclick$panelvar1)
        clust_kmean$size[as.numeric(input$plot_dblclick$panelvar1)]
    })
    # x <- isolate(input$k)
    output$detail <- renderD3tree3({
        base_graf2 <- base_k_user %>%
                            bind_cols(cluster = clust_kmean$cluster) %>%
                            filter(cluster == as.numeric(input$plot_dblclick$panelvar1))
        # base_ord_geral_all %>%
        #     right_join(base_graf2 %>% select(user_id)) %>%
        #     group_by(aisle) %>%
        #     summarise(contagem = n()) %>%
        #     arrange(desc(contagem)) %>%
        #     top_n(10) %>%
        #     ggplot(aes(x = reorder(aisle,desc(contagem)), y = contagem)) +
        #     geom_col(color = "darkorange", fill = "darkgreen")+
        #     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        #     scale_y_log10()
        cores <- unname(instacart_colors)
        base_ord_geral_all %>%
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
                    # palette = "Set2",
                    palette = cores[1:length(cores)],
                    bg.labels=c("white"),
                    align.labels=list(
                        c("center", "center"), 
                        c("right", "bottom")
                    )
            ) %>% d3tree2(rootname = "Principais Produtos")
        # make it interactive ("rootname" becomes the title of the plot):
        # inter <- d3tree2(p, rootname = "Pricipais Produtos")
        # inter
                        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

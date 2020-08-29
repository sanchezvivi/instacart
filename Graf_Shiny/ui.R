#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

n_prod <- c(10:100)

shinyUI(fluidPage(
    tabsetPanel(
        tabPanel("heatmap_graf",
                 # Application title
                 titlePanel("HeatMap Produtos x Posição_Carrinho"),
                 # Sidebar with an input_list for number of bins
                 sidebarLayout(
                     sidebarPanel(
                         verticalLayout(
                               sliderInput("slider_n_prod", 
                                         "Selecione o número de produtos", 
                                         min(n_prod),
                                         max(n_prod), 
                                         max(n_prod),
                                         step = 15
                                ),
                                selectInput("funcao",
                                            "Selecione a função que será utilizada na escala do gráfico",
                                            choices = c("1", "x^2", "sqrt2", "x^4", "sqrt4"),
                                            selected = "1"
                                            )
                         )
                         
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         verticalLayout(
                             plotOutput("heatmap1"),
                             plotOutput("heatmap2")
                         )
                     )
                 )
        )
        # tabPanel("hclust",
        #          # Application title
        #          titlePanel("HClust"),
        #          # Sidebar with an input_list for number of bins
        #          sidebarLayout(
        #              sidebarPanel(
        #                  selectInput("list_decade2", "Selecione o número de clusters", 
        #                              choices = c(2:100),
        #                              selected = 2
        #                  ),
        #                  actionButton("btn_update", "Update"
        #                  )
        #              ),
        #              # Show a plot of the generated distribution
        #              mainPanel(
        #                  leafletOutput("map_out"),
        #              )
        #          )
        # )
    )
)
)

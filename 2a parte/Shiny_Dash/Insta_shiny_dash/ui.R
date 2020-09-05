#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
n_prod <- c(10:100)

ui <- dashboardPage(skin = "green",
            dashboardHeader(title = "Instacar dashboard"),
            dashboardSidebar(
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
            ),
            dashboardBody(
                # Boxes need to be put in a row (or column)
                fluidRow(
                    plotOutput("heatmap1"),
                    plotOutput("heatmap2")
                )
            ),
            tags$head(tags$style
                        (HTML("
                                    .skin-green .main-sidebar {
                                        background-color:  darkgreen;
                                    }
                                    .skin-green .main-header .logo{
                                        background-color:  darkorange;
                                                              }
                              "
                              )
                          )
                      )
)

# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# ))

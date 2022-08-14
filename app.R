#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(ggalt)

babip_comp_sum <- readRDS("app/data/babip_comp_sum.RDS")
babip_comp_sum_p <- readRDS("app/data/babip_comp_sum_p.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(title = h4("xBABIP with Shift vs No Shift")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("game_year",
                        label="Year",
                        choices = list(2018, 2019, 2020, 2021, 2022),
                        selected = 2022)
        ),

        # Show a plot of the generated distribution
        mainPanel("Offensive xBABIP",
                  plotOutput(outputId = "distPlot1"),
                  "xBABIP Against",
                  plotOutput(outputId = "distPlot2")
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot1 <- renderPlot({
      babip_comp_sum %>% filter(game_year == input$game_year) %>%
        ggplot2::ggplot(aes(x=xBABIP, xend=xBABIP.no.shift, y = h_team, group = h_team)) +
        ggalt::geom_dumbbell(aes(color = h_team),colour_x = "#a3c4dc", colour_xend = "#0e668b",size=2,
                             dot_guide = TRUE, dot_guide_size = .25) +
        mlbplotR::scale_color_mlb(type = "primary") +
        mlbplotR::scale_fill_mlb(alpha = .4) +
        ggplot2::labs(x = "xBABIP",
                      y=NULL,
                      title="xBABIP with Shift vs No Shift") +
        ggplot2::theme(
          axis.text.y = mlbplotR::element_mlb_logo(size = .65),
          panel.background = element_rect(fill = "azure"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "darkslategray3")
        ) 
    })
    
    output$distPlot2 <- renderPlot({
      babip_comp_sum_p %>% filter(game_year == input$game_year) %>%
        ggplot2::ggplot(aes(x=xBABIP, xend=xBABIP.no.shift, y = p_team, group = p_team)) +
        ggalt::geom_dumbbell(aes(color = p_team),colour_x = "#a3c4dc", colour_xend = "#0e668b",size=2,
                             dot_guide = TRUE, dot_guide_size = .25) +
        mlbplotR::scale_color_mlb(type = "primary") +
        mlbplotR::scale_fill_mlb(alpha = .4) +
        ggplot2::labs(x = "xBABIP",
                      y=NULL,
                      title="xBABIP with Shift vs No Shift") +
        ggplot2::theme(
          axis.text.y = mlbplotR::element_mlb_logo(size = .65),
          panel.background = element_rect(fill = "azure"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "darkslategray3")
        ) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

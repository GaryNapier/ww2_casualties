
# SOURCES:
# facet_grid() code: https://community.rstudio.com/t/normalising-column-width-whilst-using-facet-wrap-and-coord-flip-in-ggplot2/70617/2
# geom_vline() code: https://stackoverflow.com/questions/54655751/ggplot-add-grid-lines-between-bars-groups
# Replace all NA with 0
# https://www.r-bloggers.com/2022/06/replace-na-with-zero-in-r/#:~:text=T2%20R2%20139-,Using%20the%20dplyr%20package%20in%20R%2C%20you%20can%20use%20the,zero%20for%20any%20NA%20values.&text=0)-,To%20replace%20NA%20values%20in%20a%20particular%20column%20of%20a,replace%20NA%20values%20with%20zero.
# Add image to xaxis https://wilkelab.org/ggtext/
# Add image to title: https://takehomessage.com/2019/12/18/r-package-ggtext/

library(ggplot2)
library(ggtext)
library(XML)
library(readr)
library(stringr)
library(reshape2)
library(rvest)
library(dplyr)
library(png)
library(jpeg)
library(shiny)

options(scipen = 999)

# Paths ----

img_dir <- "../www/"
data_path <- "../data/"

# Files ----

tab_file <- paste0(data_path, "tab.csv")

total_plot_file <- "total_plot.png"
mil_plot_file <- "mil_plot.png"
civ_plot_file <- "civ_plot.png"
pc_plot_file <- "pc_plot.png"

# Load files/data ----

tab <- read.csv(tab_file)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Second World War casualties"),
   
   h4(tags$div(
     "Data source:",
     "https://en.wikipedia.org/wiki/World_War_II_casualties",
     tags$br(), tags$br()
   )),
   
   tags$br(),
   img(src = total_plot_file, width='1200px'),
   tags$br(),
   tags$br(),
   img(src = mil_plot_file, width='1200px'),
   tags$br(),
   tags$br(),
   img(src = civ_plot_file, width='1200px'),
   tags$br(),
   tags$br(),
   img(src = pc_plot_file, width='1200px')
   
   # # Sidebar with a slider input for number of bins
   # sidebarLayout(
   #    sidebarPanel(
   #       sliderInput("bins",
   #                   "Number of bins:",
   #                   min = 1,
   #                   max = 50,
   #                   value = 30)
   #    ), # sidebarPanel

      
      # # Show a plot of the generated distribution
      # mainPanel(
      #    # plotOutput("distPlot")
      # ) # mainPanel
      # ) # sidebarLayout
) # fluidPage


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)































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

point_size <- function(max_n,
                       min_pt_sz = 0.25,
                       max_pt_sz,
                       n){
  x <- 1:max_n
  y <- -((1/max_n)*x^2) + 1
  y <- rescale(y, to = c(min_pt_sz, max_pt_sz))
  lin_df <- data.frame(x = x, y = y)
  lin <- lm(y ~ poly(x, 2), data = lin_df)
  
  # Check with plot
  # plot(x, y)
  # lines(lin$fitted.values, type = "l", lty = 1, col = "blue")
  # Get size value
  sz <- predict(lin, newdata = data.frame(x = n))
  sz
  
}

# Paths ----

img_dir <- "../www/"
data_path <- "../data/"

# Files ----

tab_file <- paste0(data_path, "tab.csv")
# country_manual_lookup_file <- paste0(data_path, "country_manual_lookup.csv")
country_df_file <- paste0(data_path, "country_df.csv")
allied_axis_lookup_file <- paste0(data_path, "allied_axis_lookup.csv")

# Plots
total_plot_file <- "total_plot.png"
mil_plot_file <- "mil_plot.png"
civ_plot_file <- "civ_plot.png"
pc_plot_file <- "pc_plot.png"

# Load files/data ----

tab <- read.csv(tab_file)
# country_manual_lookup <- read.csv(country_manual_lookup_file)
country_df <- read.csv(country_df_file)
allied_axis_lookup <- read.csv(allied_axis_lookup_file)


# Test row
# tab <- rbind(tab, rep(NA, ncol(tab)))
# tab[is.na(tab[, "country"]), "country"] <- "test"
# tab[is.na(tab[, "total"]), "total"] <- 10


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Second World War casualties"),
   
   h4(tags$div(
     "Data source:",
     "https://en.wikipedia.org/wiki/World_War_II_casualties",
     tags$br(), tags$br()
   )),
   
   h4(tags$div(
     "World War II was the deadliest military conflict in history.", 
     tags$br(),
     "An estimated total of 70–85 million people perished, or about 3% of the 2.3 billion (est.) people on Earth in 1940.", 
     tags$br(), 
     "Deaths directly caused by the war (including military and civilian fatalities) are estimated at 50–56 million, with an additional estimated 19–28 million deaths from war-related disease and famine.", 
     tags$br(), 
     "Civilian deaths totaled 50–55 million. Military deaths from all causes totaled 21–25 million, including deaths in captivity of about 5 million prisoners of war.",
     tags$br(), 
     "More than half of the total number of casualties are accounted for by the dead of the Republic of China and of the Soviet Union.",
     style = 'width:1000px', align = "justify",
     tags$br()
   )),
   
   # Bar plots
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
   img(src = pc_plot_file, width='1200px'),
   
   # # Sidebar with a slider input for number of bins
   # sidebarLayout(
   #    sidebarPanel(
   #       sliderInput("bins",
   #                   "Number of bins:",
   #                   min = 1,
   #                   max = 50,
   #                   value = 30)
   #    ), # sidebarPanel

  
   mainPanel(
     
     selectInput("country", "Country:",
                 unique(tab$country)
                 ),
     plotOutput("dots"
                , height = "1000px"
                )
     
   ) # mainPanel

   
   
      # ) # sidebarLayout
) # fluidPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$dots <- renderPlot({
    
    ctry <- input$country
    
    n <- tab[tab[, "country"] == ctry, "total"]
    
    # Code to show flag in plot title
    # https://takehomessage.com/2019/12/18/r-package-ggtext/
    flag_code <- country_df[country_df[, "countries"] == ctry, "flag_file_html_dots"]
    
    flag_code <- gsub("r_scripts/", "", flag_code)

    # Point size as linear proportion of 1:max n (100k), betweeen 1 and 0.25
    # Plot max 100k dots, otherwise will break down - divide by 1000 if over 100k
    max_n <- 100000

    # For large numbers, divide by 1000 and use 1 dot for 1000
    if(n > max_n){
      n <- n/1000
      plot_title <- paste0(flag_code, " ", fmt(n), " dots. 1 dot = 1000 people")
    }else{
      plot_title <- paste0(flag_code, " ", fmt(n), " dots.")
    }

    # Plot dots "as square as possible"
    # For example if plotting 100 dots, dots will be 10 x 10.
    # If 1000, will be the floor of the square root (31 x 31), plus the remainder (39)
    # added to the bottom row as a 31 x 8 matrix.
    sqrt_n <- floor(sqrt(n))
    remainder <- n - (sqrt_n^2)

    # Point sizes

    # Linear function
    # y = mx + c
    min_pt_sz <- 0.25
    max_pt_sz <- 3
    sz <- point_size(max_n, min_pt_sz, max_pt_sz, n)

    # Wrangle data for plot using matrices for 'main' square and 'remainder' square, and converting to data frame:

    # Main square
    mat <- matrix(1, nrow = sqrt_n,
                  ncol = sqrt_n)

    # Remainder square
    mat_rem <- matrix(rep(NA, remainder),
                      ncol = sqrt_n)

    # Fill with 1 up to value of remainder
    i <- 0
    for(row in 1:nrow(mat_rem)){
      for(col in 1:sqrt_n){
        i <- i + 1
        if(i <= remainder){
          mat_rem[row, col] <- 1
        }
      }
    }

    # Reverse remainder matrix rows (does not work if just one row)
    if(nrow(mat_rem) > 1){
      mat_rem <- mat_rem[nrow(mat_rem):1, ]
    }

    # Combine main square matrix with remainder matrix
    mat <- rbind(mat_rem, mat)

    # Put matrix into dataframe
    # df <- data.frame(melt(mat_rem, varnames = c("x", "y"), value.name = "z"))
    df <- data.frame(melt(mat, varnames = c("x", "y"), value.name = "z"))

    # Remove NA values
    df <- df[!is.na(df[, "z"]), ]
    
    ggplot()+
      geom_point(data = df, aes(y, x), size = sz, stroke = 0)+
      # ggtitle(flag_code)+
      labs(title = plot_title)+
      theme(axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_markdown(color = "black", size = 24),
            legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)






























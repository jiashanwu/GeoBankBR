#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(collapsibleTree)

rm(list = ls())

# Define UI for the bank geo app
ui <- fluidPage(
   
   # Application title
   titlePanel("Bank branch geographical distribution in 2019"),
   # Sidebar with a slider input for number of bins
   sidebarLayout(
     
   # This is the input panel
   sidebarPanel(
     # default RSSDID
     numericInput("id", "Please enter a BHC RSSDID:", 1111435),
     # search button to generate outputs
     actionButton("search", "Search"),
     br(),
     strong("Please enter a valid BHC RSSDID. The app cannot handle any exception yet."),
     p("(e.g. 1111435, 1129533, 1231342, 2526755, 2013246, etc)")
   ),

   mainPanel(
  
   # text outputs like names of BHC, bank branches, etc
   p("A few information of this RSSDID:"),
   textOutput("text1"),
   textOutput("text2"),
   textOutput("text3"),
   textOutput("text4"),
   textOutput("text5"),

   # visuals of BHC organizations
   h4("Displaying a few example branches:"),
   collapsibleTreeOutput("ct"),
   br(),
   p("Since some BHC have too many brances, we only display a few of them."),
   
   # visuals of bank branches
   h4("The map distribution of bank branches:"),
   leafletOutput("ll"),
   br(),
   p("Each circle size is proportional to the deposits in branch level.")
   )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # read the data from github
  df <- read.csv('https://raw.githubusercontent.com/jiashanwu/GeoBankBR/master/GeoBankBR2019/Data/full_cleaned.csv')
  
  # use 1111435 as demo
  # dataframe containing the names of BHC, banking institutes and branches
  df1 <- eventReactive(input$search, 
                       {df[df$RSSDHCR == input$id, c('NAMEHCR', 'NAMEFULL', 'NAMEBR')]})
  
  # a few examples of df1, used to generate the tree
  # some BHC have too many branches to include in a tree
  df2 <- eventReactive(input$search, 
                       {head(df[df$RSSDHCR == input$id, c('NAMEHCR', 'NAMEFULL', 'NAMEBR')], 10)})
  
  # longitude of branches
  lng <- eventReactive(input$search, 
                       {df[df$RSSDHCR == input$id, c('SIMS_LONGITUDE')]})
  
  # latitude of branches
  lat <- eventReactive(input$search, 
                       {df[df$RSSDHCR == input$id, c('SIMS_LATITUDE')]})
  
  # names of branches
  brhname <- eventReactive(input$search, 
                       {df[df$RSSDHCR == input$id, c('NAMEBR')]})
  
  # normalize the deposits, the default is radius of 5; maximum is 15
  depnorm <- eventReactive(input$search, 
                           {10 * df[df$RSSDHCR == input$id, c('DEPSUMBR')]/sum(df[df$RSSDHCR == input$id, c('DEPSUMBR')]) + 5})
  
  # name of BHC
  bhcname <- eventReactive(input$search, 
                           {unique(df[df$RSSDHCR == input$id, c('NAMEHCR')])})
  
  # number of banking institutes
  numins <- eventReactive(input$search, 
                            {length(unique(df[df$RSSDHCR == input$id, c('NAMEFULL')]))})
  
  # number of bank branches
  numbr <- eventReactive(input$search, 
                          {length(df[df$RSSDHCR == input$id, c('NAMEBR')])})
  
  # sum of deposits
  sumdep <- eventReactive(input$search, 
                         {sum(df[df$RSSDHCR == input$id, c('DEPSUMBR')])})
  
  
  # table of df1 (not shown)
  output$table <- renderTable(df1())
  
  # leaflet of bank geo
  output$ll <- renderLeaflet({
    
    m <- leaflet() %>% addTiles()
    m <- addCircleMarkers(m, lng = lng(), lat = lat(), label = brhname(), radius = depnorm(), color = 'red')
    
    m
    
  })
  
  # the collappsible tree demo
  output$ct <- renderCollapsibleTree(
    
    collapsibleTree(
    df2(),
    hierarchy = c("NAMEHCR", "NAMEFULL", "NAMEBR"),
    width = 800,
    fontSize = 6,
    zoomable = TRUE
    )
    
  )
  
  # text outpus, brief info the RSSDID
  output$text1 <- renderText({paste("The BHC RSSDID: ", input$id)})
  output$text2 <- renderText({paste("The BHC NAME: ", bhcname())})
  output$text3 <- renderText({paste("The Number of holding banks: ", numins())})
  output$text4 <- renderText({paste("The Number of holding bank branches: ", numbr())})
  output$text5 <- renderText({paste("Total deposits by branches (in dollars): ", sumdep())})
  
}

# Run the application 
shinyApp(ui = ui, server = server)


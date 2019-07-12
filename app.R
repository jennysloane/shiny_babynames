# too many names to have them all listed so have to select certain names manually 
library(shiny)
library(ggplot2)
library(dplyr)
library(here)
babynames <- read.csv("ozbabynames.csv", stringsAsFactors = FALSE)
babynames <- babynames %>%
  filter(name == "Jennifer" | name == "Jenny" | name == "Jenna" | name == "Ella")
#all_states <- unique(babynames$state)
ui <- fluidPage(
  titlePanel("Baby Names in Australia"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1930, max = 2017,
            value = c(1950, 2000), sep = ""),
      radioButtons("stateInput", "State", #radioButtons
            choices = c("New South Wales", "Northern Territory", "Queensland", "South Australia", "Victoria", "Tasmania", "Western Australia"),
            selected = "South Australia"),
      selectInput("namesInput", "Name", #checkboxGroupInput
                  choices = c("Jennifer", "Jenna", "Jenny"), 
                  selected = "Jennifer") 
    #  uiOutput("namesOutput") # use this to get a list of all of the names
    ),
    mainPanel(
      plotOutput("name_trends_plot"), # output placeholder (will make a histogram) and must use renderPlot in the server function
      br(), br(),
      tableOutput("results") # output placeholder (will make a table) and must use renderTable in the server function 
    ) 
  )
)
#test <- babynames %>%
#  filter(name == "Jennifer")
# ggplot(babynames, aes(year, count)) +
#   geom_smooth(se=F) +
#   facet_wrap(~name)

server <- function(input, output) {
  #output$namesOutput <- renderUI({
  #  selectInput("namesInput", "name",
  #              sort(unique(babynames$name)),
  #              selected = "Jennifer")
  #})
  
  filtered <- reactive({
    if (is.null(input$namesInput)) {
      return(NULL)
    } 
    
    babynames %>%
      filter(year >= input$yearInput[1],
             year <= input$yearInput[2],
             state == input$stateInput,
             name == input$namesInput
      ) %>%
      group_by(name, year) %>% 
      summarise(count = sum(count)) 
  })
  
  output$name_trends_plot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(year, count, colour=name)) +
      #geom_smooth(se=F, na.rm=TRUE) +
      geom_point(na.rm=TRUE, size=5) +
      geom_line(na.rm=TRUE, size=2) +
      #facet_wrap(~name) + #scales = "free_y"
      theme_bw() #+ 
      #facet_wrap(~state)
    })
  
  output$results <- renderTable({
    filtered() 
  })
}


# to deploy to shinyapps.io
# rsconnect::deployApp(here())
shinyApp(ui = ui, server = server)

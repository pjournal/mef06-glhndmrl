library(readxl)
library(ggplot2)
library(tidyverse)
library(shiny)

#my_data <- read_excel("/Users/gulhandemirel/Desktop/foreign_students_by_nationality_2021_2022.xlsx")
my_data <- read_excel("foreign_students_by_nationality_2021_2022.xlsx")
my_data <- transform(my_data, E=as.numeric(E), K=as.numeric(K), T=as.numeric(T))


ui <- fluidPage(
  
  # Application title
  titlePanel("Foreign Students By Nationality"),
  sidebarLayout(
    sidebarPanel(
      selectInput('university', 'Select University', choices =my_data$Üniversite.Adı)
    ),
    mainPanel(
      plotOutput('my_data')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$my_data <- renderPlot({
    students <- my_data %>%
      filter(Üniversite.Adı == input$university) %>%
      arrange(desc(T)) %>%
      head(5)
    
    ggplot(students, aes(x=Uyruk, y=T, label=T)) + geom_col(aes(fill = T), colour = "black", position = "dodge") + geom_text(position = position_dodge(0.9), vjust = -0.5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

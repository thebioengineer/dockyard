#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Random Prints"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         textInput("name_input",
                   "Enter your Name:")
      ),

      # Show a plot of the generated distribution
      mainPanel(
         uiOutput("name_output")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$name_output <- renderUI({
     wrapper<-sample(list(p,h1,h2,h3),size = 1)[[1]]
     if(input$name_input==""){
      p(style="color:red;","<- Please enter your name in the text entry box on the left.")
     }else{
      wrapper(paste0("Hi ",input$name_input,", nice to meet you!"))
     }
   })
}

# Run the application
shinyApp(ui = ui, server = server)


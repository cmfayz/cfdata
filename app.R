library(shiny)
library(ggplot2)
library(scales)
ui <- fluidPage(
  
   titlePanel("Planning a Shiny Retirement"),
   
   # Inputs will go in a sidebar
   sidebarPanel(
     
     # Numeric inputs for user to put in their current balance, amount they add each year, and how much will they spend per year in retirement
     numericInput("cbl", "Current Balance in Retirement Accounts:", 50000),
     numericInput("abl", "Annual Contribution to Retirement Accounts:", 5000),
     numericInput("deb", "Estimated Annual Expenses in Retirement:", 12000),
     
     # Slider for current age and age at retirement
     sliderInput("range", "Current Age and Expected Retirement Age:", min = 1, max = 100, value = c(25,65))
     
   ),
   mainPanel(
     
     # Plot of retirement funds by age
     plotOutput("plot"),
     h3(textOutput("maxfunds")),
     h3(textOutput("lowpoint"))
   )
   
)


server <- function(input, output) {
  retirementmoney <- reactive({
    bal<-input$cbl #here I am coding my inputs as vectors
    spend<-input$deb
    contrib<-input$abl
    cage <- input$range[1]
    rage <- input$range[2]
    wkyrs <- rage-cage
    eage <- 100 #for reality's sake, the analysis will not go beyond year 100
    rtime <- eage-rage
    
    for(i in 1:wkyrs) {
      bal <- rbind(bal, bal[i] + contrib) #adding the yearly contribution each working year
    }
    
    for(j in 1:rtime) {
      if (bal[j+wkyrs]>0) {
        bal <- rbind(bal, (bal[j+wkyrs]-spend)) #when in retirement, subtract retirement spending each year
      }
      else {
        eage <- rage + j-1 #looking at each year beyond retirement
        break
      }
      age <- cage:eage #focusing on the time between the current age of the person and the end
    }
    rownames(age) <- c()
    rownames(bal) <- c()
    data <- data.frame(cbind(age, bal))
    data_frame <- setNames(data, c("age","bal")) #storing this information in a dataframe is what allows the plotting to work
  })
 output$plot <- renderPlot({
   gr1<- ggplot(retirementmoney(), aes(x=retirementmoney()$age, y=retirementmoney()$bal))
   gr2<- gr1 + geom_line() + labs (x="Age", y="Retirement Funds Available") + scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) + scale_y_continuous(label=comma) #adding commas to the y axis and mandating a certain number of ticks on the x axis for better comprehension
   print(gr2)
   })
 output$maxfunds <- renderText({ 
   paste("Your maximum balance will be $", max(retirementmoney()$bal)) #getting the max value of the balance
 })
 output$lowpoint <- renderText({ 
   paste("You will run out of retirement funds at age", max(retirementmoney()$age)) #the max value for age is where bal will be 0
 })
 
  }

# Run the application 
shinyApp(ui = ui, server = server)


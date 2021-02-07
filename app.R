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
            titlePanel("Sample size, power and detectable difference for 2 group comparisons"),
        
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel( 
                    radioButtons("hyp", "Hypothesis:",
                             choices = list (
                               "Superiority" =1,
                               "Non-inferiority" = 2), selected = 1
                    ),
                    conditionalPanel("input.hyp == 1",
                        radioButtons("type",  "What do you want to estimate",
                              choices = list(
                                  "Power" = 1,
                                 "Sample size" = 2,
                                 "Detectable difference" = 3), selected = 1
                         ),
                        conditionalPanel("input.type == 1",  # power
                            numericInput(
                            "n1", "Sample size (per group)", value = 100, min = 5, max = 100000),
                            numericInput(
                            "mu11", "Mean group 1",value = 1,min = 0,max = 500),
                            numericInput(
                            "mu21", "Mean group 2", value = 1.5, min = 0,max = 500),
                            numericInput(
                            "sd1", "Standard deviation (common to both groups)", value = 1),
                            numericInput(
                            "alpha1", "Type I error rate", min = 0.000000001, max = 0.2, value = .05)
                        ),
                        conditionalPanel("input.type ==2", #sample size
                            numericInput(
                                "pow2", "Power", value = 0.8, min = 0.1, max = 1),
                            numericInput(
                                "mu12", "Mean group 1", value = 1),
                            numericInput(
                                "mu22", "Mean group 2", value = 1.5),
                            numericInput(
                                "sd2", "Standard deviation (common to both groups)", value = 1),
                            numericInput(
                                "alpha2", "Type 1 error rate", value = 0.05, min=0,max=1)
                        ),
                        conditionalPanel("input.type ==3",
                            numericInput(
                                "n3", "Sample size (per group)", value = 100),
                            numericInput(
                                "pow3", "Power", value = 0.8),
                            numericInput(
                                "sd3", "Standard deviation (common to both groups)", value = 1),
                            numericInput(
                                "alpha3", "Type 1 error rate", value = .05)
                        )
                    ),
                    conditionalPanel("input.hyp == 2",
                            numericInput("n4", "Sample size (per group)", value =100, min = 10, max = 100000),
                            numericInput("nim", "Non-inferiority margin", value = 0.2),
                            numericInput("delta", "Null difference", value = 0),
                            selectInput("dir", "Direction:", c("Lower" = "low",
                                                                    "Higer" = "hi")),
                            numericInput("sd4", "Standard deviation (common to both groups)", value = 1),
                            numericInput("alpha4", "Type I error rate", min = 0.000000001, max = 0.2, value = .05)
                    ),
                    actionButton("do","Submit")
                ),
                
        
                # Show a plot of the generated distribution
                mainPanel(
                   plotOutput("distPlot"),
                  textOutput("txtOutput")
                )
            )
)

    



# Define server logic
server <- function(input, output) {

    observeEvent(input$do,{
        res <- if(input$hyp == 1 & input$type == 1){
                  power.t.test(power= , n=input$n1, delta = (input$mu21-input$mu11), sd=input$sd1, sig.level=input$alpha1)
               }
               else{
                   if(input$hyp == 1 & input$type ==2){
                       power.t.test(n= NULL, delta = (input$mu22-input$mu12), sd=input$sd2, power=input$pow2, sig.level = input$alpha2)
                   }
                   else{
                       if(input$hyp == 1 & input$type ==3){
                           power.t.test(n=input$n3, power=input$pow3,delta=NULL , sd = input$sd3, sig.level = input$alpha3)
                       }
                       else{
                          p<-PowerTOST::power.noninf(alpha = input$alpha4, logscale = FALSE, margin = input$nim, theta0 = input$delta, CV = input$sd4, n = input$n4)
                          list(power = p, n= input$n4, sig.level = input$alpha4)
                          }
                   }
               }
        output$txtOutput = renderText({
          if(input$hyp ==1){
            paste0("A sample size of ", ceiling(res$n), " in each group will give the study ", round(res$power,2)*100," % power to detect a between-group difference of ", round(res$delta,1)," units with a type 1 error rate of ", res$sig.level)
          }
          else{
            paste0("A sample size of ", res$n, " in each group will give ", round(res$p*100,1), "% power to declare the intervention is non-inferior to the control with a type 1 error rate of ", round(res$sig.level*100,2), "%")
          }
        })
        
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

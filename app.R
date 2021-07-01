#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyBS)
library(mathjaxr)

source("RWsimulate.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Theme from shinythemes
    theme = shinytheme("flatly"),

    # Application title
    titlePanel("Rescorla-Wagner Model"),

    # Hide Warnings
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of Simulated Trials:",
                        min = 30,
                        max = 500,
                        value = 100),
            sliderInput("nB",
                        "Trial to Introduce Stimulus B:",
                        min = 0,
                        max = 500,
                        value = 30),            
            sliderInput("g1",
                        "A Learning Rate (gamma):",
                        min = 0,
                        max = .5,
                        step=0.01,
                        value = 0.09),
            sliderInput("g2",
                        "B Learning Rate (gamma):",
                        min = 0,
                        max = .5,
                        step=0.01,
                        value = 0.09)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plot",shinycssloaders::withSpinner(plotOutput("distPlot")), tableOutput('table')),
                tabPanel("Information",shinycssloaders::withSpinner(uiOutput("info")))
            )
        )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    res <- reactive({
        suppressWarnings(sim_blocking(g1=input$g1,g2=input$g2,n=input$n,nB=input$nB))
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        plot_rwsim(res())

    })

    output$info <- renderUI({
        withMathJax(helpText('
        INTRODUCTION
        $$ $$
        The purpose of this web app is to help you visualise how the blocking effect is explained by the Rescorla-Wagner model of associative learning. 
        The simulated data are generated based on the parameters that you choose.
        $$ $$
        THE MODEL
        $$ $$
        The Rescorla-Wagner model (Rescorla & Wagner, 1972) assumes that earning will occur if "what happens on the trial does not match the expectation 
        of the organism" and "the expectation on any given trial is based on the predictive value of all of the stimuli present." (Wilson, 2012, p. 1)
        
        $$  $$
        In the blocking effect paradigm, initial learning of a stimulus, A, blocks later learning of a new stimulus, B. The explanation is that by the 
        time B is introduce, the degree of suprise has been reduced because the outcome can be predicted almost solely on the basis of A alone.
        
        $$ $$
        The app allows you to set the total number of trials, the trial on which stimulus B is introduced, and the learning rates for both A and B.
        
        $$ $$
        References: 
        
        $$ $$
        Rescorla, R. A. & Wagner, A. R. (1972). A theory of Pavlovian conditioning: variations in teh effectives of reinforcement and nonreinforcement. In \\(\\textit{Classical conditioning, II: Current research and theory.}\\), 64-99.
        $$ $$
        Wilson, W. J. (2012). The Rescorla-Wagner, Simplified. https://campus.albion.edu/wjwilson/files/2012/03/RWSimplified.pdf
        '))
    })
}

# Run the application
shinyApp(ui = ui, server = server)

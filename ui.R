library(dplyr)
library(plotly)

fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  titlePanel("Probality true proportion value"),
  
  tabsetPanel(
    type = "tabs",
    
    tabPanel("Proportions", 
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 helpText("The plots and the statistics will be calculated",
                          "as soon as you enter the data below."),
                 
                 radioButtons("data_entry_method", label = h4("How to enter the data"),
                              choices = list("Cases + Noncases" = 1, "N + Proportion" = 2),
                              selected = 1),
                 
                 conditionalPanel("input.data_entry_method == 1",
                                  numericInput("cases", "Cases 1", value = NULL)),
                 conditionalPanel("input.data_entry_method == 1",
                                  numericInput("noncases", "Noncases 1", value = NULL)),
                 conditionalPanel("input.data_entry_method == 1",
                                  numericInput("cases2", "Cases 2 (optional)", value = NULL)),
                 conditionalPanel("input.data_entry_method == 1",
                                  numericInput("noncases2", "Noncases 2 (optional)", value = NULL)),
                 
                 
                 conditionalPanel("input.data_entry_method == 2",
                                  numericInput("n", "N", value = NULL)),
                 conditionalPanel("input.data_entry_method == 2",
                                  numericInput("prop", "Prop", value = NULL)),
                 conditionalPanel("input.data_entry_method == 2",
                                  numericInput("n2", "N 2 (optional)", value = NULL)),
                 conditionalPanel("input.data_entry_method == 2",
                                  numericInput("prop2", "Prop 2 (optional)", value = NULL)),
                 hr(),
                 
                 textOutput("overlap"),
                 
                 hr(),
                 
                 h4("For group 1 we are ...% sure the true proportion is between"),
                 sliderInput("ci", "select %", min = 0, max = 100, value = 95),
                 textOutput("low"),
                 textOutput("midpoint"),
                 textOutput("upper")
                 
               ),
               
               mainPanel(
                 plotlyOutput("beta_density"),
                 hr(),
                 plotlyOutput("beta_distribution"),
                 hr(),
                 h3("More Information"),
                 helpText(readLines("expl_proportions.txt")[1]),
                 helpText(readLines("expl_proportions.txt")[3]))
             )
    ),
    
    tabPanel("Means" ,
             sidebarLayout(
               
               sidebarPanel(
                 helpText("The plots and the statistics will be calculated",
                          "as soon as you enter the data below."),
                 
                 numericInput("mean1", "Mean 1", value = NULL),
                 numericInput("sd1", "Std Dev 1", value = NULL, min = 0),
                 numericInput("normal_n1", "Sample Size 1", value = NULL, min = 0),
                 numericInput("mean2", "Mean 2 (optional)", value = NULL),
                 numericInput("sd2", "Std Dev 2 (optional)", value = NULL, min = 0),
                 numericInput("normal_n2", "Sample Size 2 (optional)", value = NULL, min = 0)
               ),
               
               mainPanel(
                 plotlyOutput("normal_density"),
                 hr(),
                 plotlyOutput("normal_distribution")
               )
             )
    )
    
  )
)

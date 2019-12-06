library(dplyr)
library(plotly)

fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  titlePanel("Do Statistical Tests"),
  
  tabsetPanel(
    type = "tabs",
    
    tabPanel("Intro",
             br(),
             h4("What test to use?"),
             helpText(readLines("explanations/intro_text.txt")[1]),
             hr(),
             
             h5("Proportions"),
             helpText(readLines("explanations/intro_text.txt")[3]),
             h5("What data to bring?"),
             helpText(readLines("explanations/intro_text.txt")[5]),
             hr(),
             
             h5("Means"),
             helpText(readLines("explanations/intro_text.txt")[7]),
             h5("What data to bring?"),
             helpText(readLines("explanations/intro_text.txt")[9]),
             hr(),
             
             h5("Counts"),
             helpText(readLines("explanations/intro_text.txt")[11]),
             h5("What data to bring?"),
             helpText(readLines("explanations/intro_text.txt")[13])
             ),
    
    
    tabPanel("Proportions", 
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 helpText("The plots and the statistics will be calculated",
                          "as soon as you enter the data below."),
                 
                 radioButtons("data_entry_method", label = h4("How to enter the data?"),
                              choices = list("Events + Nonevents" = 1, "Group Size + Proportion" = 2),
                              selected = 1),
                 
                 conditionalPanel("input.data_entry_method == 1",
                                  numericInput("cases", "Total Events", value = NULL)),
                 conditionalPanel("input.data_entry_method == 1",
                                  numericInput("noncases", "Total Nonevents", value = NULL)),
                 
                 conditionalPanel("input.data_entry_method == 2",
                                  numericInput("n", "Group Size", value = NULL)),
                 conditionalPanel("input.data_entry_method == 2",
                                  numericInput("prop", "Proportion", value = NULL)),
                 
                 h5("We are 95% sure the true proportion is between"),
                 textOutput("beta_low"),
                 textOutput("beta_midpoint"),
                 textOutput("beta_upper"),
                 
                 hr(),
                 helpText("If you want to compare to another group, fill in below"),
                 conditionalPanel("input.data_entry_method == 1",
                                  numericInput("cases2", "Events group 2 (optional)", value = NULL)),
                 conditionalPanel("input.data_entry_method == 1",
                                  numericInput("noncases2", "Nonevents group 2 (optional)", value = NULL)),
                 conditionalPanel("input.data_entry_method == 2",
                                  numericInput("n2", "Group Size group 2 (optional)", value = NULL)),
                 conditionalPanel("input.data_entry_method == 2",
                                  numericInput("prop2", "Proportion group 2 (optional)", value = NULL)),
                 textOutput("proportions_overlap")
               ),
               
               mainPanel(
                 plotlyOutput("beta_density"),
                 hr(),
                 plotlyOutput("beta_distribution")
                 )
             )
    ),
    
    tabPanel("Means" ,
             sidebarLayout(
               
               sidebarPanel(
                 helpText("The plots and the statistics will be calculated",
                          "as soon as you enter the data below."),
                 
                 numericInput("mean1", "Mean", value = NULL),
                 numericInput("sd1", "Standard Deviation", value = NULL, min = 0),
                 numericInput("normal_n1", "Group Size", value = NULL, min = 0),
                 
                 h5("We are 95% sure the true mean is between"),
                 textOutput("normal_low"),
                 textOutput("normal_midpoint"),
                 textOutput("normal_upper"),
                 
                 hr(),
                 helpText("If you want to compare to another group, fill in below"),
                 numericInput("mean2", "Mean group 2 (optional)", value = NULL),
                 numericInput("sd2", "Standard Deviation group 2 (optional)", value = NULL, min = 0),
                 numericInput("normal_n2", "Sample Size group 2 (optional)", value = NULL, min = 0),
                 textOutput("means_overlap")
                ),
               
               mainPanel(
                 plotlyOutput("normal_density"),
                 hr(),
                 plotlyOutput("normal_distribution")
               )
             )
    ),
    
    tabPanel("Counts" ,
             sidebarLayout(
               
               sidebarPanel(
                 helpText("The plots and the statistics will be calculated",
                          "as soon as you enter the data below."),
                 
                 numericInput("mean_cnt1", "Average Count", value = NULL, min = 0),
                 numericInput("count_n1", "Number of Counts", value = NULL, min = 0),
                 
                 h5("We are 95% sure the true rate is between"),
                 textOutput("poisson_low"),
                 textOutput("poisson_midpoint"),
                 textOutput("poisson_upper"),
                 
                 hr(),
                 helpText("If you want to compare to another group, fill in below"),
                 numericInput("mean_cnt2", "Average Count groups 2", value = NULL, min = 0),
                 numericInput("count_n2", "Number of Counts group 2", value = NULL, min = 0),
                 textOutput("counts_overlap")
               ),
               
               mainPanel(
                 plotlyOutput("poisson_density"),
                 hr(),
                 plotlyOutput("poisson_distribution")
               )
             )
    ),
    
    tabPanel("1: Why Statistics",
             helpText(readLines("explanations/1_why_stats.txt")[1]),
             helpText(readLines("explanations/1_why_stats.txt")[3]),
             helpText(readLines("explanations/1_why_stats.txt")[5]),
             helpText(readLines("explanations/1_why_stats.txt")[7])
    ),
    
    tabPanel("2: Sample Size",
             helpText(readLines("explanations/2_sample_size.txt")[1]),
             helpText(readLines("explanations/2_sample_size.txt")[3]),
             helpText(readLines("explanations/2_sample_size.txt")[5]),
             helpText(readLines("explanations/2_sample_size.txt")[7])
    ),
    
    tabPanel("3: Representative Sample",
             helpText(readLines("explanations/3_representative_sample.txt")[1]),
             helpText(readLines("explanations/3_representative_sample.txt")[3]),
             helpText(readLines("explanations/3_representative_sample.txt")[5]),
             helpText(readLines("explanations/3_representative_sample.txt")[7]),
             helpText(readLines("explanations/3_representative_sample.txt")[9])
    ),
    
    tabPanel("4: Effect Size",
             helpText(readLines("explanations/4_effect_size.txt")[1]),
             helpText(readLines("explanations/4_effect_size.txt")[3]),
             helpText(readLines("explanations/4_effect_size.txt")[5]),
             helpText(readLines("explanations/4_effect_size.txt")[7])
    ),
    
    tabPanel("5: Practical Tips",
             helpText(readLines("explanations/5_practical_tips.txt")[1]),
             helpText(readLines("explanations/5_practical_tips.txt")[3]),
             helpText(readLines("explanations/5_practical_tips.txt")[5]),
             helpText(readLines("explanations/5_practical_tips.txt")[7]),
             helpText(readLines("explanations/5_practical_tips.txt")[9])
    )
  )
)


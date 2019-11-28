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
                 
                 textOutput("proportions_overlap"),
                 
                 hr(),
                 
                 h4("For group 1 we are ...% sure the true proportion is between"),
                 sliderInput("ci", "select %", min = 0, max = 100, value = 95),
                 textOutput("beta_low"),
                 textOutput("beta_midpoint"),
                 textOutput("beta_upper")
               ),
               
               mainPanel(
                 plotlyOutput("beta_density"),
                 hr(),
                 plotlyOutput("beta_distribution"),
                 hr(),
                 h3("More Information"),
                 helpText(readLines("explanations/expl_proportions.txt")[1]),
                 helpText(readLines("explanations/expl_proportions.txt")[3]))
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
                 numericInput("normal_n2", "Sample Size 2 (optional)", value = NULL, min = 0),
                 
                 hr(),
                 
                 textOutput("means_overlap"),
                 
                 hr(),
                 
                 h4("For group 1 we are ...% sure the true mean is between"),
                 sliderInput("normal_ci", "select %", min = 0, max = 100, value = 95),
                 textOutput("normal_low"),
                 textOutput("normal_midpoint"),
                 textOutput("normal_upper")
               ),
               
               mainPanel(
                 plotlyOutput("normal_density"),
                 hr(),
                 plotlyOutput("normal_distribution"),
                 hr(),
                 h3("More Information"),
                 helpText(readLines("explanations/expl_means.txt")[1]),
                 helpText(readLines("explanations/expl_means.txt")[3])
               )
             )
    ),
    
    tabPanel("Counts" ,
             sidebarLayout(
               
               sidebarPanel(
                 helpText("The plots and the statistics will be calculated",
                          "as soon as you enter the data below."),
                 
                 numericInput("mean_cnt1", "Average Count 1", value = NULL, min = 0),
                 numericInput("count_n1", "Number of Counts 1", value = NULL, min = 0),
                 numericInput("mean_cnt2", "Average Count 2", value = NULL, min = 0),
                 numericInput("count_n2", "Number of Counts 2", value = NULL, min = 0),
                 
                 hr(),
                 
                 textOutput("counts_overlap"),
                 
                 hr(),
                 
                 h4("For group 1 we are ...% sure the true rate is between"),
                 sliderInput("poisson_ci", "select %", min = 0, max = 100, value = 95),
                 textOutput("poisson_low"),
                 textOutput("poisson_midpoint"),
                 textOutput("poisson_upper")
               ),
               
               mainPanel(
                 plotlyOutput("poisson_density"),
                 hr(),
                 plotlyOutput("poisson_distribution"),
                 hr(),
                 h3("More Information"),
                 helpText(readLines("explanations/expl_counts.txt")[1])
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


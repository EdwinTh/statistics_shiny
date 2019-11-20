source("funcs.R")
source("distribution_overlap.R")
function(input, output, session) {

  #############
  ### Tab 1 ###
  #############
  
  output$data_entry <- reactive({
    input$data_entry_method
  })

  beta_input_vals <- reactive({
    if (input$data_entry_method == "1") {
      update_input_values(input$cases, input$noncases, 
                          input$cases2, input$noncases2)
    } else {
      update_input_values(n = input$n, prop = input$prop,
                          n2 = input$n2, prop2 = input$prop2)
    }
  })
  
  beta_pars <- reactive({
    list(alpha1 = beta_input_vals()$cases + 1, 
         beta1  = beta_input_vals()$noncases + 1,
         alpha2 = beta_input_vals()$cases2 + 1,
         beta2  = beta_input_vals()$noncases2 + 1)
  })

  output$beta_density <- renderPlotly(
    plot_posterior(beta_posterior(beta_pars()$alpha1, 
                                  beta_pars()$beta1, 
                                  "density"),
                   beta_posterior(beta_pars()$alpha2, 
                                  beta_pars()$beta2, 
                                  "density")
    )
  )
  
  output$beta_distribution <- renderPlotly(
    plot_posterior(beta_posterior(beta_pars()$alpha1, 
                                  beta_pars()$beta1, 
                                  "distribution"),
                   beta_posterior(beta_pars()$alpha2, 
                                  beta_pars()$beta2, 
                                  "distribution")
    )
  )

  output$low <- renderText(
    calculate_ci(beta_pars()$alpha1, beta_pars()$beta1, input$ci)[1] %>%
      paste("Lower bound:", .)
  )

  output$midpoint <- renderText(
    calculate_ci(beta_pars()$alpha1, beta_pars()$beta1, input$ci)[2] %>%
      paste("Mid point:", .)
  )

  output$upper <- renderText(
    calculate_ci(beta_pars()$alpha1, beta_pars()$beta1, input$ci)[3] %>%
      paste("Upper bound:", .)
  )
  
  output$overlap <- renderText({
    posterior_overlap <- posterior_overlap(
      beta_distribution(beta_pars()$alpha1, beta_pars()$beta1),
      beta_distribution(beta_pars()$alpha2, beta_pars()$beta2)
    )
    paste0("Are we 95% certain the true proportions are different: ", posterior_overlap)
  })
  
  #############
  ### Tab 2 ###
  #############
  
  normal_pars <- reactive({
    post1 <- list(postmean = NULL, postsigma = NULL)
    post2 <- list(postmean = NULL, postsigma = NULL)
    
    if (!is.na(input$normal_n1)) {
      post1 <- bpp::NormalNormalPosterior(input$mean1, input$sd1, input$normal_n1, 0, 100000)
    }
    
    if (!is.na(input$normal_n2)) {
      post2 <- bpp::NormalNormalPosterior(input$mean2, input$sd2, input$normal_n2, 0, 100000)  
    }
    
    list(mu1  = post1$postmean, 
         sig1 = post1$postsigma,
         mu2  = post2$postmean, 
         sig2 = post2$postsigma)
  })
  
  output$normal_density <- renderPlotly({
    plot_posterior(normal_posterior(normal_pars()$mu1, 
                                    normal_pars()$sig1, 
                                    "density"),
                   normal_posterior(normal_pars()$mu2, 
                                    normal_pars()$sig2, 
                                    "density")
    )
  })
  
  output$normal_distribution <- renderPlotly(
    plot_posterior(normal_posterior(normal_pars()$mu1, 
                                    normal_pars()$sig1, 
                                    "distribution"),
                   normal_posterior(normal_pars()$mu2, 
                                    normal_pars()$sig2, 
                                    "distribution")
    )
  )
  
}


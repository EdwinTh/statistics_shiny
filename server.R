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

  conf_bounds_beta <- reactive({
    calculate_ci(beta_pars()$alpha1, beta_pars()$beta1, input$ci)
  })
  
  output$beta_low <- renderText(
    paste("Lower bound:", conf_bounds_beta()[1])
  )

  output$beta_midpoint <- renderText(
    paste("Mid point:", conf_bounds_beta()[2])
  )

  output$beta_upper <- renderText(
    paste("Upper bound:", conf_bounds_beta()[3])
  )
  
  output$proportions_overlap <- renderText({
    post_overlap <- posterior_overlap(
      beta_distribution(beta_pars()$alpha1, beta_pars()$beta1),
      beta_distribution(beta_pars()$alpha2, beta_pars()$beta2)
    )
    paste0("Are we 95% certain the true proportions are different: ", post_overlap)
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
                                    "density"),
                   type = "mean"
    )
  })
  
  normal_distribution_grid1 <- reactive({
    normal_posterior(normal_pars()$mu1, 
                     normal_pars()$sig1, 
                     "distribution")
  })
  
  output$normal_distribution <- renderPlotly(
    plot_posterior(normal_distribution_grid1(),
                   normal_posterior(normal_pars()$mu2, 
                                    normal_pars()$sig2, 
                                    "distribution"),
                   type = "mean"
    )
  )
  
  output$means_overlap <- renderText({
    post_overlap <- posterior_overlap(
      normal_distribution(normal_pars()$mu1, normal_pars()$sig1),
      normal_distribution(normal_pars()$mu2, normal_pars()$sig2)
    )
    paste0("Are we 95% certain the true means are different: ", post_overlap)
  }) 
  
  conf_bounds_normal <- reactive({
    calculate_ci_grid(normal_distribution_grid1(), input$normal_ci)
  })
  
  output$normal_low <- renderText(
    paste("Lower bound:", conf_bounds_normal()[1])
  )
  
  output$normal_midpoint <- renderText(
    paste("Mid point:", conf_bounds_normal()[2])
  )
  
  output$normal_upper <- renderText(
    paste("Upper bound:", conf_bounds_normal()[3])
  )
  
  #############
  ### Tab 3 ###
  #############
  
  poisson_pars <- reactive({
    list(alpha1 = input$mean_cnt1 * input$count_n1 + .001,
         beta1  = input$count_n1 + .001,
         alpha2 = ifelse(is.null(input$count_n2), NULL, input$mean_cnt2 * input$count_n2 + .001),
         beta2  = ifelse(is.null(input$count_n2), NULL,input$count_n2 + .001))
  })
  
  output$poisson_density <- renderPlotly({
    plot_posterior(poisson_posterior(poisson_pars()$alpha1, 
                                     poisson_pars()$beta1, 
                                    "density"),
                   poisson_posterior(poisson_pars()$alpha2, 
                                     poisson_pars()$beta2, 
                                     "density"),
                   type = "rate"
    )
  })
  
  output$poisson_distribution <- renderPlotly({
    plot_posterior(poisson_posterior(poisson_pars()$alpha1, 
                                     poisson_pars()$beta1, 
                                     "distribution"),
                   poisson_posterior(poisson_pars()$alpha2, 
                                     poisson_pars()$beta2, 
                                     "distribution"),
                   type = "rate"
    )
  })
  
  output$counts_overlap <- renderText({
    post_overlap <- posterior_overlap(
      poisson_distribution(poisson_pars()$alpha1, poisson_pars()$beta1),
      poisson_distribution(poisson_pars()$alpha2, poisson_pars()$beta2)
    )
    paste0("Are we 95% certain the true rates are different: ", post_overlap)
  }) 
  
  
  conf_bounds_poisson <- reactive({
    calculate_ci_poisson(poisson_pars()$alpha1, poisson_pars()$beta1, input$poisson_ci)
  })
  
  output$poisson_low <- renderText(
    paste("Lower bound:", conf_bounds_poisson()[1])
  )
  
  output$poisson_midpoint <- renderText(
    paste("Mid point:", conf_bounds_poisson()[2])
  )
  
  output$poisson_upper <- renderText(
    paste("Upper bound:", conf_bounds_poisson()[3])
  )
  
}


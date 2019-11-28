library(dplyr)
library(plotly)

update_input_values <- function(cases     = NULL,
                                noncases  = NULL,
                                cases2    = NULL,
                                noncases2 = NULL,
                                n         = NULL,
                                prop      = NULL,
                                n2        = NULL,
                                prop2     = NULL) {
  if (!is.null(cases) & !is.null(noncases)) {
    n    <- cases + noncases
    prop <- cases / n
  }
  
  if (!is.null(cases2) & !is.null(noncases2)) {
    n2    <- cases2 + noncases2
    prop2 <- cases2 / n2
  }
  
  if (!is.null(n) & !is.null(prop)) {
    cases    <- round(n * prop)
    noncases <- n - cases
  }
  
  if (!is.null(n2) & !is.null(prop2)) {
    cases2    <- round(n2 * prop2)
    noncases2 <- n2 - cases2
  }
  
  list(cases     = cases,
       noncases  = noncases,
       cases2    = cases2,
       noncases2 = noncases2,
       n         = n,
       prop      = prop,
       n2        = n2,
       prop2     = prop2)
}

beta_posterior <- function(alpha,
                           beta,
                           func) {
  
  if (is.null(beta)) return(NULL)
  
  x    <- seq(0, 1, .001)
  p <- if (func == "density") {
    dbeta(x, alpha, beta)
  } else {
    pbeta(x, alpha, beta)
  }
  
  tibble(x = x,
         p = p)
}

normal_posterior <- function(mean, sd, func) {
  if (is.null(mean) | is.null(sd)) return(NULL)
  low  <- qnorm(.0001, mean, sd)
  high <- qnorm(.9999, mean, sd)
  x <- seq(low, high, length.out = 1000)
  p <- if (func == "density") {
    dnorm(x, mean, sd)
  } else {
    pnorm(x, mean, sd)
  }
  
  tibble(x = x,
         p = p)
}

poisson_posterior <- function(alpha, beta, func) {
  if (is.na(alpha) | is.na(beta)) return(NULL)
  low  <- qgamma(.0001, alpha, beta)
  high <- qgamma(.9999, alpha, beta)
  x <- seq(low, high, length.out = 1000)
  p <- if (func == "density") {
    dgamma(x, alpha, beta)
  } else {
    pgamma(x, alpha, beta)
  }
  
  dplyr::tibble(x = x, p = p)
}

plot_posterior <- function(x,
                           x2   = NULL,
                           type = c("proportion", "mean", "rate")) {
  
  type <- match.arg(type)
  x_range <- range(c(x$x, x2$x))
  
  f <- list(
    family = "Proxima Nova",
    size = 18,
    color = "#0071B3"
  )
  
  x_lab <- list(
    title = glue::glue("{capitalise(type)} Value"),
    titlefont = f,
    range = c(x_range[1], x_range[2])
  )
  
  if (tail(x$p, 1) == max(x$p)) {
    title_text <- glue::glue("Probability true {type} is smaller than x")
    y_title    <- "Probability"
  } else {
    title_text <- glue::glue("Probability true {type} is exactly x")
    y_title    <- "Density"
  }
  
  y_lab <- list(
    title = y_title,
    titlefont = f
  )
  
  title_font <-list(
    family = "Proxima Nova",
    size = 18,
    color = "#0071B3"
  )
  
  cust_layout <- function(x) {
    plotly::layout(x,
                   title = title_text,
                   titlefont = title_font,
                   xaxis = x_lab,
                   yaxis = y_lab)
  }
  
  if (is.null(x2)){
    pl <- plotly::plot_ly(x, x = ~get('x'), y = ~get('p'),
                    type = 'scatter', mode = 'lines',
                    colors = "blue")
  } else {
    x <- bind_rows(x %>% mutate(grp = "1"), x2 %>% mutate(grp = "2"))
    pl <- plotly::plot_ly(x, x = ~get('x'), y = ~get('p'),
                          type = 'scatter', mode = 'lines', color = ~get("grp"),
                          colors = c("#F7A100", "#0071B3"))
  }
  cust_layout(pl)
}

calculate_ci_grid <- function(grid, width) {
  low  <- .5 - ((width / 100) / 2)
  high <- .5 + ((width / 100) / 2)
  purrr::map_dbl(c(low, 0.5, high),
                 ~find_closest(.x, grid)) %>% 
    round(3)
}

find_closest <- function(quant, grid) {
  grid$x[which.min(abs(quant - grid$p))]
}

calculate_ci <- function(alpha, beta, width) {
  low  <- .5 - ((width / 100) / 2)
  high <- .5 + ((width / 100) / 2)
  qbeta(c(low, 0.5, high), alpha, beta) %>%
    round(3)
}

calculate_ci_poisson <- function(alpha, beta, width) {
  low  <- .5 - ((width / 100) / 2)
  high <- .5 + ((width / 100) / 2)
  qgamma(c(low, 0.5, high), alpha, beta) %>%
    round(3)
}

capitalise <- function(word) {
  paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
}


posterior_overlap <- function(dens1, 
                              dens2,
                              ci = .95) {
  stopifnot(is.function(dens1), is.function(dens2))
  dens1_borders <- determine_borders(dens1, ci)
  dens2_borders <- determine_borders(dens2, ci)
  no_overlap(dens1_borders, dens2_borders)
}

beta_distribution <- function(alpha, beta) {
  function(x) qbeta(x, alpha, beta)
}

normal_distribution <- function(mu, sigma) {
  function(x) qnorm(x, mean = mu, sd = sigma)
}

poisson_distribution <- function(alpha, beta) {
  function(x) qgamma(x, alpha, beta)
}

determine_borders <- function(dens, ci = .95) {
  dens(c(.5 - ci/2, .5 + ci/2))
}

no_overlap <- function(bords1, bords2) {
  dif <- max(c(bords1[1], bords2[1])) > min(c(bords1[2], bords2[2]))
  ifelse(dif, "Yes", "No")
}


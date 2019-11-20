# currently works for beta only
# needs to be expanded if we want to 
# include other probability distributions
posterior_overlap <- function(dens1, 
                              dens2,
                              ci = .95) {
  stopifnot(is.function(dens1), is.function(dens2))
  dens1_borders <- determine_borders(dens1, ci)
  dens2_borders <- determine_borders(dens2, ci)
  no_overlap(dens1_borders, dens2_borders)
}

determine_order <- function(dens1, dens2) {
  (dens2(.5) > dens1(.5)) + 1
}

beta_distribution <- function(alpha, beta) {
  function(x) qbeta(x, alpha, beta)
}

determine_borders <- function(dens, ci) {
  dens(c(.5 - ci/2, .5 + ci/2))
}

no_overlap <- function(bords1, bords2) {
  dif <- max(c(bords1[1], bords2[1])) > min(c(bords1[2], bords2[2]))
  ifelse(dif, "Yes", "No")
}


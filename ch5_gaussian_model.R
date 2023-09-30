data(Howell1)

height <- function() Howell1
height.adult <- function() height() %>% filter(age >= 18)

height.precis <- function() height() %>% precis()

height.density.plot <- function() height.adult() %>% dens()

height.priors <- function() {
  mean_population_height <- 178
  sd_population_height <- 20
  dnorm(
    height.adult()$height,
    mean_population_height,
    sd_population_height
  )
}

plot.height.priors <- function() curve(dnrom(x, 178, 20), from = 100, to = 250)

plot.prior.predictive <- function() {
  sample_mu <- rnorm(1e4, 178, 20)
  sample_sigma <- runif(1e4, 0, 50)
  prior_h <- rnorm(1e4, sample_mu, sample_sigma)
  dens(prior_h) 
}

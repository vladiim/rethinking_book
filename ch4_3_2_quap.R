# Quadratic approximation
# Making inferences about the shape of the posterior
# lies at the Maximum A Posteriori est (MAP)

m4.3 <- function() height.adult.modelFit()

height.adult.modelDefinition <- function() {
  # mean 178
  # stdiv high = 20, low = 0.1

  alist(
    height ~ dnorm( mu, sigma ), # Likelihood
    mu     ~ a + b*( weight - mean(height.adult()$weight)),  # Mean prior
    a ~ dnorm( 178, 20 ),
    b ~ dlnorm(0, 1),
    sigma  ~ dunif( 0, 50 )      # Standard deviation prior
  )
}

height.adult.modelFit <- function(d = height.adult()) {
  height.adult.modelDefinition() %>% quap(d)
}

height.adult.model.posteriorDist <- function() {
  height.adult.modelFit() %>% precis()
}

# Variance - covariance matrix
# Multi-dimensional glue for quadratic approximation
# How each parameter relates to each other
height.adult.varCovar <- function() {
  height.adult.model.posteriorDist() %>% vcov()
}

height.adult.genSamples <- function() {
  height.adult.modelFit() %>% extract.samples(, n = 1e4)
}

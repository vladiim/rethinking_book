# Quadratic approximation
# Making inferences about the shape of the posterior
# lies at the Maximum A Posteriori est (MAP)

height.adult.modelDefinition <- function() {
  # stdiv high = 20, low = 0.1

  alist(
    height ~ dnorm( mu, sigma ),
    mu     ~ dnorm( 178, 0.1 ),
    sigma  ~ dunif( 0, 50 )
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

# Quadratic approximation
# Making inferences about the shape of the posterior
# lies at the Maximum A Posteriori est (MAP)

height.adult.modelDefinition <- function() {
  alist(
    height ~ dnorm( mu, sigma ),
    mu     ~ dnorm( 178, 20 ),
    sigma  ~ dunif( 0, 50 )
  )
}

height.adult.modelFit <- function(d = height.adult()) {
  height.adult.modelDefinition() %>% quap(d)
}

height.adult.model.posteriorDist <- function() {
  precis(height.adult.modelFit)
}

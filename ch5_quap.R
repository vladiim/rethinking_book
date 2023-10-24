# Quadratic approximation
# Making inferences about the shape of the posterior
# lies at the Maximum A Posteriori est (MAP)

height.adult.modelDefinition <- function(stdiv = 20) {
  alist(
    height ~ dnorm( mu, sigma ),
    mu     ~ dnorm( 178, stdiv ),
    sigma  ~ dunif( 0, 50 )
  )
}

height.adult.modelFit <- function(d = height.adult(), stdiv = 20) {
  height.adult.modelDefinition(stdiv) %>%
    quap(d)
}

height.adult.model.posteriorDist <- function() {
  precis(height.adult.modelFit())
}

height.adult.model.posteriorDist.lowSD <- function() {
  precis(height.adult.modelFit(height.adult(), 0.1))
}

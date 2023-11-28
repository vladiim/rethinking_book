#### Model average height posterior approximaion

heightWeight.posterior <- function() {
  # Equation: https://capture.dropbox.com/kov9DZoJFl5eBpKo
  d <- height.adult() 

  # Average weight, x-bar
  xbar <- mean(d$weight)

  fit <- quap(alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - mean(weight)),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d)
  fit
}

### Understand the model

heightWeight.posterior.statTable <- function() heightWeight.posterior() %>% precis()

heightWeight.posterior.coVariation <- function() heightWeight.posterior() %>% vcov(3)

heightWeight.posterior.plot <- function() {
  # Shows how well the linear model fits the data
  d <- height.adult() 
  plot(height ~ weight, data = d, col = rangi2)
  posterior <- extract.samples(heightWeight.posterior())
  a_map <- mean(posterior$a)
  b_map <- mean(posterior$b)
  curve(a_map + b_map * (x - mean(d$weight)), add = TRUE)
}

### Understand the mu height distribution for every weight value

heightWeight.muDist <- function() {
  # gets a dist for every individual, not every weight
  # link(heightWeight.posterior())

  # define all the possible weights
  weight.seq <- seq(from = 25, to = 70, by = 1)
  link(heightWeight.posterior(), data = data.frame(weight = weight.seq))
}

### Understand the distribution for a specific attribute, e.g. weight = 50kg, what is the height distribution?

heightWeight.attr.mu <- function(weight = 50) {
  d <- height.adult() 
  posterior <- extract.samples(heightWeight.posterior())
  posterior$a + posterior$b * (weight - mean(d$weight))
}

heightWeight.attr.mu.plot <- function(weight = 50) {
  heightWeight.attr.mu(weight) %>%
    dens(col = rangi2, lwd = 2, xlab = paste0("mu|weight=", weight))
}

heightWeight.attr.mu.probabilityInterval <- function(weight = 50, interval = 0.9) {
  heightWeight.attr.mu(weight) %>% PI(prob = interval) 
}


#### Display average heights

heightWeight.simulation.goodModel <- function() {
  # +ve log normal relationship b/w weight & height
#  b <- rlnorm(1e4, 0, 1) # view with - dens(b, xlim = c(0,5), adj = 0.1)
  b <- rlnorm(100, 0, 1)
  heightWeight.simulation(b)
}


heightWeight.simulation.badModal <- function() {
  # People are -ve hieght and taller than the tallest man
  # Also - average height increases with weight
  b <- rnorm(100, 0, 10)
  heightWeight.simulation(b)
}

heightWeight.simulation <- function(b) {
  set.seed(2971)
  d <- height.adult() 
  N <- 100
  a <- rnorm(N, 178, 20)
  plot(NULL,
    xlim = range(d$weight),
    ylim = c(-100, 400),
    xlab = "weight",
    ylab = "height"
  )
  abline(h = 0, lty = 2) # no one can be shorter than 0
  abline(h = 272, lty =1, lwd = 0.5) # tallest man ever 272 cm
  mtext("b ~ dnorm(0, 10)")
  xbar <- mean(d$weight)
  for (i in 1:N) {
    curve(a[i] + b[i] * (x - xbar),
      from = min(d$weight),
      to = max(d$weight),
      add = TRUE,
      col = col.alpha("black",0.2)
    ) 
  }
}

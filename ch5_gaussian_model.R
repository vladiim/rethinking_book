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

height.grid.posterior.brute.force <- function() {
  mu <- seq(from=150, to=160, length.out=100)
  sigma <- seq(from=7, to=9, length.out=100)
  expand.grid(mu = mu, sigma = sigma) %>%
  mutate(
    ll = sapply(1:nrow(.), function(i) {
      dnorm(height.adult()$height, mu[i], sigma[i], log = TRUE) %>%
      sum()
    }),
    prod = ll + dnorm(mu, 178, 20, TRUE) + dunif(sigma, 0, 50, TRUE),
    prob = exp(prod - max(prod))
  )
}

height.grid.posterior.plots <- function() {
 # height.grid.posterior.brute.force() %>% contour_xyz()
  height.grid.posterior.brute.force() %>% image_xyz()
}

height.grid.posterior.sample.posterior <- function(
  d = height.grid.posterior.brute.force(),
  size = 1e4
  ) {
  sample.rows <- sample(1:nrow(d), size = size, replace = TRUE, prob = d$prob)

  data.frame(
    mu = d$mu[ sample.rows ],
    sigma = d$sigma[ sample.rows ]
  )
}

height.grid.posterior.sample.posterior.plot <- function(d = height.grid.posterior.sample.posterior()) {
  #plot(s$mu, s$sigma, cex = 0.5, pch = 15, col = col.alpha(rangi2, 0.1))
  plot(d$mu, d$sigma, cex = 0.5, pch = 15, col = col.alpha(rangi2, 0.1))
}

height.grid.posterior.sample.posterior.describe <- function(d = height.grid.posterior.sample.posterior()) {
  par(mfrow = c(1, 2))
  dens(d$mu)
  dens(d$sigma)
}

height.grid.posterior.sample.posterior.PI <- function(d = height.grid.posterior.sample.posterior()) {
  PI(d$mu)
  PI(d$sigma)
}

height.grid.smaller.sample <- function(d = height.adult()) {
  d3 <- sample(d$height, size=20)
  mu.list <- seq( from=150, to=170, length.out=200)
  sigma.list <- seq( from=4, to=20, length.out=200)
  post2 <- expand.grid( mu=mu.list, sigma=sigma.list)
  post2$LL <- sapply( 1:nrow(post2), function(i)  sum( dnorm( d3, mean=post2$mu[i], sd=post2$sigma[i],  log=TRUE)))
  post2$prod <- post2$LL + dnorm( post2$mu, 178, 20, TRUE) +  dunif( post2$sigma, 0, 50, TRUE)
  post2$prob <- exp( post2$prod - max(post2$prod))
  sample2.rows <- sample( 1:nrow(post2), size=1e4, replace=TRUE,  prob=post2$prob)
  sample2.mu <- post2$mu[ sample2.rows]
  sample2.sigma <- post2$sigma[ sample2.rows]
  plot( sample2.mu, sample2.sigma, cex=0.5,  col=col.alpha(rangi2,0.1),  xlab="mu", ylab="sigma", pch=16)
}

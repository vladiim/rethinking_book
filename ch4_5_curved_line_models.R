# Two types
# B-Splines (better)
# Polynomial regression

# Polynomal regression
# Model: https://capture.dropbox.com/IQOV80oewVxTqcHL
m4.5 <- function() polynomial.quap()
mp4.5.stats <- function() mp4.5() %>% precis()

height.standardised <- function() {
  d <- height()

  # Standartise data when doing polynomials by modifying µ
  d$weight_s <- ( d$weight - mean(d$weight) ) / sd(d$weight)
  d$weight_s2 <- d$weight_s^2
  d$weight_s3 <- d$weight_s^3
  d
}

# Polynomial model - much better than linear but worse than cubic regression
polynomial.quap <- function() {
  d <- height.standardised()
  quap(alist(
    height ~ dnorm(mu, sigma),
    mu <- a + ( b1 * weight_s ) + ( b2 * weight_s2 ),
    a ~  dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d)
}

plot.poly.fits <- function() plot.polynomial.fits(m4.5())

plot.polynomial.fits <- function(m = m4.5()) {
  weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
  pred_dat <- list(weight_s = weight.seq, weight_s2 = weight.seq ^ 2)
  mu <- link(m, data = pred_dat)
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI, prob = 0.89)
  sim.height <- sim(m, data = pred_dat)
  height.PI <- apply(sim.height, 2, PI, prob = 0.89)
  plot(height ~ weight_s, data = height.standardised(), col = col.alpha(rangi2, 0.5))
  lines(weight.seq, mu.mean)
  shade(mu.PI, weight.seq)
  shade(height.PI, weight.seq)
}

# Cubic regression - best fit
m4.6 <- function() cubic.quap()
plot.cubic.fits <- function() plot.polynomial.fits(m4.6())

cubic.quap <- function() {
  d <- height.standardised()
  quap(alist(
    height ~ dnorm(mu, sigma),
    mu <- a + ( b1 * weight_s ) + ( b2 * weight_s2 ) + ( b3 * weight_s3 ),
    a ~  dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    b3 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d)
}

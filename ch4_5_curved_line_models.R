# Two types
# B-Splines (better)
# Polynomial regression

# Polynomal regression
# Model: https://capture.dropbox.com/IQOV80oewVxTqcHL
m4.5 <- function() polynomial.quap()
mp4.5.stats <- mp4.5() %>% precis()

polynomial.quap <- function() {
  d <- height()

  # Standartise data when doing polynomials by modifying µ
  d$weight_s <- ( d$weight - mean(d$weight) ) / sd(d$weight)
  d$weight_s2 <- d$weight_s^2

  quap(alist(
    height ~ dnorm(mu, sigma),
    mu <- a + ( b1 * weight_s ) + ( b2 * weight_s2 ),
    a ~  dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d)
}

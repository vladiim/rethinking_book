heightWeight.simulation <- function() {
  set.seed(2971)
  d <- height.adult() 
  N <- 100
  a <- rnorm(N, 178, 20)
  b <- rnorm(N, 0, 10)
  plot(NULL,
    xlim = range(d$weight),
    ylim = c(-100, 400),
    xlab = "weight",
    ylab = "height"
  )
  abline(h = 0, lty = 2)
  ablin(h = 272, lty =1, lwd = 0.5)
  mtext("b ~ dnorm(0, 10)")
  xbar <- mean(d$weight)
  for (i in 1:N) {
    lines(
      d$weight,
      a[i] + b[i] * (d$weight - xbar),
      col = col.alpha("black", 0.1)
    )
  }
  points(d$weight, d$height)
}

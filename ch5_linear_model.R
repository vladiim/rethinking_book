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
for (i in 1:N) curve( a[i] + b[i]*(x - xbar),  from=min(d2$weight), to=max(d2$weight), add=TRUE,  col=col.alpha("black",0.2)) 
}

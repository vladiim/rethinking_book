sim.heights <- function() {
  data <- list(weight = weight.seq()) 
  m4 <- m4.3()
  sim(m4.3(), data = data)
}

height.PI <- function() {
  apply(sim.heights(), 2, PI, prob = 0.89)
}

# Plot
plot.raw.predIntervals <- function() {
  # Raw data
  plot(height ~ weight, height.adult(), col=col.alpha(rangi2, 0.5))

  weight.seq <- weight.seq()
  mu <- heightWeight.muDist()
  mu.mean <- apply(mu, 2, mean)

  # MAP line
  lines(weight.seq, mu.mean)

  # HPDI region
  #shade(mu.HPDI, weight.seq)

  # PI region for simulated heights
  height.pi <- height.PI()
  shade(height.pi, weight.seq)
}

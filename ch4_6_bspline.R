d.bspline <- cherry_blossoms
d.bspline2 <- d.bspline[ complete.cases(d.bspline$doy), ]

NUM_KNOTS <- 15

# B-spline model
knot.list <- function() {
  quantile(
    d.bspline2$year,
    probs = seq(0, 1, length.out = NUM_KNOTS)
  )
}

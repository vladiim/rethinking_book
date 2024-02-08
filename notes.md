# Generating predictions and intervals from the posterior of a fit model

1. Use `link` to generate distributions of posterior valus for µ. The default behaviour of `link` is to use the original data, so you have to pass it a list of new horizontal axis values you want to plot posterior predictions across. e.g. func `heightWeight.muDist()`.

2. Use summary functions like `mean` or `PI` to find averages and lower/upper bounds of µ for each value of the predictor variable. e.g. `heightWeight.muDist.summary()` 

3. Use plotting functions like `lines` or `shade` to draw the lines and intervals or do further numerical calculations with them. e.g. `heightWeight.attr.mu.plot()`

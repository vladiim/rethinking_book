# Bayse theorum
# p(earth | land ) = p(land | earth) * p( earth ) / p(land)
p_earth <- 0.5
p_mars <- 0.5
p_land_earth <- 0.3
p_land_mars <- 1.0

(p_land_earth * p_earth) / ((p_land_earth * p_earth)/ (p_earth * p_mars))

# c("b/b", "b/w", "w/w")

# Grid approximation
W <- 3
L <- 0
grid <- seq(from=0, to=1, length.out=100)
prior <- rep(1, 100)
prior <- ifelse(grid < 0.5, 0, 1)
prior <- exp(-5 * abs(grid - 0.5))
obs <- 5
samples <- 7
likelihood <- dbinom(obs, size = samples, prob = grid)
unstandardised_posterior <- likelihood * prior
posterior <- unstandardised_posterior / sum(unstandardised_posterior)
#plot(grid, posterior) # centralises to 60% 6 / 9


# library(rethinking)
w <- 6
l <- 3


#curve( d, from=0, to=1)  #quadratic approximation
# curve( dnorm( x, 0.67, 0.16), lty=2, add=TRUE) 

n_samples <- 1000  
p <- rep( NA, n_samples)  
p[1] <- 0.5  
W <- 6  
L <- 3  
for (i in 2:n_samples) {
  p_new <- rnorm( 1, p[i-1], 0.1)  
  if (p_new < 0) p_new <- abs( p_new) 
  if (p_new > 1) p_new <- 2 - p_new  
  q0 <- dbinom( W, W+L, p[i-1])  
  q1 <- dbinom( W, W+L, p_new)  
  p[i] <- ifelse( runif(1) < q1/q0, p_new, p[i-1]) 
}
# The values in p are samples from the posterior distribution. To compare to the analytical posterior:  R code  2.9  
# dens( p, xlim=c(0,1))  
# curve( dbeta( x, W+1, L+1), lty=2, add=TRUE) 

# chapter 3 prob vampire
pos_vamp <- 0.95
prob_vamp <- 0.001
pos_human <- 0.01
prob_human <- 1 - prob_vamp
prob_pos <- (pos_vamp * prob_vamp) + (pos_human * prob_human)

prob_test_vamp <- (pos_vamp * prob_vamp) / prob_pos

# panda ch 2 question
spa_twins <- 0.1
spb_twins <- 0.2
prob_species <- 0.5
prob_twins <- (spa_twins * prob_species) + (spb_twins * prob_species)
prob_spa <- (spa_twins * prob_species) / prob_twins

# generate prior
pgrid <- seq(from=0, to=1, length.out=10000)
probp<-rep(1,10000)
probdata<-dbinom(6,size=9,prob=pgrid)
rawposterior<-probdata*pgrid
posterior<-rawposterior/sum(rawposterior)
samples<-sample(pgrid,prob=posterior,size=1e4,replace=T)

# proportion of posterior < 50%
lower_bound <- sum(samples < 0.5) / 1e4

# 90% confidence
conf_intervals <- quantile(samples, c(0.1, 0.9))

# calculate loss over every descision
loss <- sapply(pgrid, function(x) sum(posterior * abs(x - pgrid)))

# minimise loss
min_loss <- pgrid[ which.min(loss)]

# Given 70% of globe is water and 2 tosses, what is the likelihood of getting water 0 times, 1 time or 2 times? i.e. 0:2
prob_water <- dbinom(0:2, size=2, prob=0.7)
# now simulate 10 samples
water_10_samples <- rbinom(10, size=2, prob=0.7)
# varify with large samples
water_10k <- rbinom(1e5, size=2, prob=0.7)
results <- table(water_10k) / 1e5

# generate diff samples
w_samples <- rbinom(1e5, size=9, prob=0.7)
# simplehist(w_samples)

# 3.5 practice
n<-1e4
p_grid <- seq(from=0, to=1, length.out=n)
prior <- rep(1, n)
likelihood <- dbinom(8, size=15, prob=p_grid)
raw_posterior <- likelihood * prior
posterior <- raw_posterior / sum(raw_posterior)
set.seed(1)
samples <- sample(p_grid, prob=p_grid, size=n, replace=T)

# Calculate the estimated probabilty of gettting a result given priors
prior_prob <- 0.7
size_of_est <- 10
est_result <- 5 # e.g. 5/10 positives given 0.7 prior prob
est <- rbinom(est_result, size = size_of_est, prob=prior_prob)

# CH 3 hard
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0, 0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0, 1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0, 1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1, 0,0,0,1,1,1,0,0,0,0) 
births <- c(birth2, birth1)
perc_boy <- sum(births) / length(births)
n <- 1e4
pgrid <- seq(from=0, to=1, length.out=n)
prior <- rep(1, n)
likelihood <- dbinom(sum(births), size=length(births), prob=pgrid)
raw_posterior <- likelihood * prior
posterior <- raw_posterior / sum(raw_posterior)

# CHAPTER 4

# 10k people flipling a coin 16 times and walking left or right
pos <- replicate( 1000, sum( runif(16,-1,1)))

# calculate Growth Rate 1 (no growth) to 1.1 (10%)
growth_rate <- prod( 1 + runif(12,0,0.1)) 
gen_growth <- replicate( 10000, prod( 1 + runif(12,0,0.1)))
# proportion_growth <- dens( growth, norm.comp=TRUE) # dens needs the rethinking package

# Gaussian dist - variance matters
# the smaller the adlditive step, the better the gaussian approximation
big <- replicate( 10000, prod( 1 + runif(12,0,0.5)))  
small <- replicate( 10000, prod( 1 + runif(12,0,0.01))) 
big_at_log_is_the_same <- replicate( 10000, log(prod(1 + runif(12,0,0.5)))) 

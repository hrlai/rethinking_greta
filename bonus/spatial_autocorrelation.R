# example of a linear regression model with spatially-correlated residuals
# see https://github.com/greta-dev/greta/issues/218

# make fake spatial data
set.seed(2018-08-23)

n <- 100
k <- 3
coords <- matrix(runif(n * 2), ncol = 2)
X <- matrix(rnorm(n * k), ncol = k)

true_intercept <- rnorm(1, 0, 1)
true_beta <- rnorm(k, 0, 0.2)
true_range <- 0.8
true_mu <- true_intercept + X %*% true_beta
true_Sigma <- exp(-0.5 * as.matrix(dist(coords)) / true_range)

y <- MASS::mvrnorm(1, true_mu, true_Sigma)

# plot(coords, asp = 1, cex = exp(scale(y)))

# using the dev branch
# devtools::install_github("greta-dev/greta@dev")
library(greta)
#> 
#> Attaching package: 'greta'
#> The following objects are masked from 'package:stats':
#> 
#>     binomial, poisson
#> The following objects are masked from 'package:base':
#> 
#>     %*%, backsolve, beta, colMeans, colSums, diag, eigen,
#>     forwardsolve, gamma, rowMeans, rowSums, sweep, tapply

# parameters
range <- normal(0, 0.5, truncation = c(0, Inf))
beta <- normal(0, 10, k)
intercept <- normal(0, 3)

# spatial random effect
distance <- as.matrix(dist(coords))
Sigma <- exp (-0.5 * distance / range)

# mean
mu <- intercept + X %*% beta

# this is one realisation of a multivariate normal, so it needs to be a row
y_row <- t(y)
distribution(y_row) <- multivariate_normal(t(mu), Sigma)

m <- model(intercept, beta, range)

library(future)
plan(multisession)
draws <- mcmc(m,
              chains = 2,
              warmup = 2000,
              one_by_one = TRUE)  # handle cholesky errors
#> 
#> running 2 chains in parallel, each on up to 2 cores
#>

plot(draws)

# looks good, but we could do with some more samples for 'range'
coda::effectiveSize(draws)
#> intercept beta[1,1] beta[2,1] beta[3,1]     range 
#> 1822.9392 1203.1398 1206.9114 1253.6127  826.3138
draws <- extra_samples(draws,
                       n_samples = 1000,
                       one_by_one = TRUE)
#> 
#> running 2 chains in parallel, each on up to 2 cores
#> 
plot(draws)

coda::effectiveSize(draws)
#> intercept beta[1,1] beta[2,1] beta[3,1]     range 
#>  3688.805  2331.398  1933.940  2278.731  1701.291

# check posterior estimates
summary(draws)$statistics
#>                 Mean         SD     Naive SE Time-series SE
#> intercept -0.8342330 0.77911704 0.0123189221   0.0128257061
#> beta[1,1] -0.5279881 0.02053475 0.0003246829   0.0004258921
#> beta[2,1] -0.3297013 0.01533758 0.0002425084   0.0003500329
#> beta[3,1]  0.1286363 0.01677290 0.0002652028   0.0003817559
#> range      0.8705338 0.12348462 0.0019524633   0.0029941229
c(true_intercept, true_beta, true_range)
#> [1] -0.4742740 -0.5280850 -0.3082669  0.1422844  0.8000000

# plot correlated residuals (spatial correlation component)
resid <- y - mu
resid_draws <- calculate(resid, values = draws)
resid_mean <- summary(resid_draws)$statistics[, "Mean"]
plot(coords, asp = 1, cex = exp(scale(resid_mean)))


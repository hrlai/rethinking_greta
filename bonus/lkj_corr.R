library(greta)

# model matrix
modmat <- model.matrix(~ Sepal.Width, iris) 
# index of species
jj <- as.numeric(iris$Species)

M <- ncol(modmat) # number of varying coefficients
N <- max(jj) # number of species

# prior on the standard deviation of the varying coefficient
tau <- exponential(0.5, dim = M)

# prior on the correlation between the varying coefficient
Omega <- lkj_correlation(3, M)

# optimization of the varying coefficient sampling through
# cholesky factorization and whitening
Omega_U <- chol(Omega)
Sigma_U <- sweep(Omega_U, 2, tau, "*")
z <- normal(0, 1, dim = c(N, M)) 
ab <- z %*% Sigma_U # equivalent to: ab ~ multi_normal(0, Sigma_U)

# the linear predictor
mu <- rowSums(ab[jj,] * modmat)

# the residual variance
sigma_e <- cauchy(0, 3, truncation = c(0, Inf))

#model
y <- iris$Sepal.Length
distribution(y) <- normal(mu, sigma_e)

mod <- model(ab, sigma_e, Sigma_U, Omega)

draws <- mcmc(mod)

summary(draws)

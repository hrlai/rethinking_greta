library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]


library(greta)

# data
height <- d2$height

# Model 4.1
# variables & priors
mu <- normal(178, 20)
sigma <- uniform(0, 50)

# observation model
distribution(height) <- normal(mu, sigma)

m4.1 <- model(mu, sigma)

# find the MAP estimate using 
opt_m4.1 <- opt(m4.1, optimiser = bfgs(), hessian = TRUE)
opt_m4.1$par


# Model 4.2
# variables & priors
mu <- normal(178, 0.1)
sigma <- uniform(0, 50)

# observation model
distribution(height) <- normal(mu, sigma)

m4.2 <- model(mu, sigma)

# find the MAP estimate using 
opt_m4.2 <- opt(m4.1, optimiser = bfgs(), hessian = TRUE)
opt_m4.2$par


# Height ~ Weight
# data
height <- d2$height
weight <- d2$weight - mean(d2$weight)  # centering following p. 96

# variables & priors
alpha <- normal(178, 20)
beta <- normal(0, 10)
sigma <- uniform(0, 50)

# likelihood 
mu <- alpha + beta * weight

# observation model
distribution(height) <- normal(mu, sigma)

m4.3 <- model(alpha, beta, sigma)

# find the MAP estimate using 
opt_m4.3 <- opt(m4.3, optimiser = bfgs(), hessian = TRUE)
opt_m4.3$par

with(d2, plot(weight, height, col = "steelblue3"))
curve(opt_m4.3$par$alpha + opt_m4.3$par$beta*(x - mean(x)), 
      from = min(d2$weight), to = max(d2$weight),
      add = TRUE)

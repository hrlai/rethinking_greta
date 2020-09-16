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
height <- as_data(d2$height)
weight <- as_data(d2$weight - mean(d2$weight))  # centering following p. 96

# variables & priors
alpha <- normal(178, 20)
beta <- normal(0, 10)
sigma <- uniform(0, 50)

# likelihood 
mu <- alpha + beta * weight

# observation model
distribution(height) <- normal(mu, sigma)

m4.3 <- model(alpha, beta, sigma)

# simulate linear slopes from m4.3 with prior predictive simulation
N <- 100
m4.3_sims <- simulate(m4.3, nsim = N, seed = 2971)
plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) ,
      xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0, 10)" )
for (i in seq_len(N)) {
    curve(m4.3_sims$alpha[i, , 1] + m4.3_sims$beta[i, , 1]*(x - mean(x)),
          from = min(d2$weight), to = max(d2$weight),
          add = TRUE, col = col.alpha("black", 0.2))
}

# change beta's prior from normal to lognormal
height <- as_data(d2$height)
weight <- as_data(d2$weight - mean(d2$weight))  # centering following p. 96
alpha <- normal(178, 20)
beta <- lognormal(0, 1)
sigma <- uniform(0, 50)
mu <- alpha + beta * weight
distribution(height) <- normal(mu, sigma)
m4.3 <- model(alpha, beta, sigma)
# simulate linear slopes from m4.3 with prior predictive simulation
m4.3_sims <- simulate(m4.3, nsim = N, seed = 2971)
plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) ,
      xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dlnorm(0, 1)" )
for (i in seq_len(N)) {
    curve(m4.3_sims$alpha[i, , 1] + m4.3_sims$beta[i, , 1]*(x - mean(x)),
          from = min(d2$weight), to = max(d2$weight),
          add = TRUE, col = col.alpha("black", 0.2))
}

# find the MAP estimate using 
opt_m4.3 <- opt(m4.3, optimiser = bfgs(), hessian = TRUE)
opt_m4.3$par

with(d2, plot(weight, height, col = rangi2))
curve(opt_m4.3$par$alpha + opt_m4.3$par$beta*(x - mean(x)), 
      from = min(d2$weight), to = max(d2$weight),
      add = TRUE)

# fit model with different sample sizes
N <- 10
dN <- d2[ 1:N , ]
height <- as_data(d2$height)
weight <- as_data(d2$weight - mean(d2$weight))  # centering following p. 96
alpha <- normal(178, 20)
beta <- lognormal(0, 1)
sigma <- uniform(0, 50)
mu <- alpha + beta * weight
distribution(height) <- normal(mu, sigma)
m4.3 <- model(alpha, beta, sigma)
opt_m4.3 <- opt(m4.3, optimiser = bfgs(), hessian = TRUE)

height_new <- greta_array(dim = 100)
weight_new <- 
    seq(
        min(d2$weight - mean(d2$weight)),
        max(d2$weight - mean(d2$weight)),
        length.out = 100
    )
mu_new <- alpha + beta * weight_new
distribution(height_new) <- normal(mu_new, sigma)

height_post_sim <-
    calculate(
        height_new,
        values = list(
            alpha = opt_m4.3$par$alpha,
            beta = opt_m4.3$par$beta,
            sigma = opt_m4.3$par$sigma
        ),
        nsim = 20
    )
plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))
for (i in seq_len(N)) {
    lines(weight_new + mean(d2$weight), height_post_sim$height_new[i, , ])
}

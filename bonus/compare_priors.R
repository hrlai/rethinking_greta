library(greta)

# commonly used priors on sigma
sigma <- 
    list(
        exponential(1),
        normal(0, 1, truncation = c(0, Inf)),
        # student(3, 0, 10, truncation = c(0, Inf)),
        lognormal(0, 1),  
        cauchy(0, 1, truncation = c(0, Inf)),
        cauchy(0, 2, truncation = c(0, Inf))
    )

sigma_sims <- 
    lapply(sigma, function(x) {
        calculate(x, nsim = 10000)
    })

xlim_max <- 10
par(mfrow = c(5, 1), mar = c(4.5,4,0,0))
lapply(sigma_sims, function(x) {
    plot(density(x$x[,,1], from = 0, to = xlim_max), 
         xlab = "sigma", ylab = "Density", main = "",
         xlim = c(0, xlim_max), ylim = c(0, 1),
         bty = "n", col = "red")
})

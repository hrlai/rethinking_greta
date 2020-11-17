library(greta)

# see https://greta-stats.org/articles/example_models.html#common-bayesian-priors
horseshoe <- function (tau = 1, dim = NULL) {
    lambda <- cauchy(0, 1, truncation = c(0, Inf), dim = dim)
    sd <- tau ^ 2 * lambda ^ 2
    return(sd)
}

regularized_horseshoe <- function (tau = 1,  c = 1, dim = NULL) {
    stopifnot(c > 0)
    lambda <- cauchy(0, 1, truncation = c(0, Inf), dim = dim)
    lambda_tilde <- (c^2 * lambda^2) / (c^2 + tau^2 * lambda^2)
    sd <- tau ^ 2 * lambda_tilde ^ 2
    return(sd)
}


# commonly used priors on sd
sd <- 
    list(
        normal10  = normal(0, 10, truncation = c(0, Inf)),
        normal1   = normal(0, 1, truncation = c(0, Inf)),
        # student(3, 0, 10, truncation = c(0, Inf)),  # brms
        lognormal = lognormal(0, 1),  
        cauchy1   = cauchy(0, 1, truncation = c(0, Inf)),
        cauchy2   = cauchy(0, 2, truncation = c(0, Inf)),
        exp1      = exponential(1),
        ridge     = inverse_gamma(1, 1),  
        lasso     = exponential(0.5 * gamma(1, 1)**2),  
        horseshoe = horseshoe(),
        finnish_horseshoe = regularized_horseshoe()
    )

# simulate coef
coef <- lapply(sd, function(sd) {
    normal(0, sd)
})

# simulate coefs
coef_sims <- lapply(coef, function(x) {
    calculate(x, nsim = 10000)
})

plot_lims <- c(-5, 5)
plot(density(unlist(coef_sims$normal10), 
             from = plot_lims[1], to = plot_lims[2]),
     xlim = plot_lims, ylim = c(0, 0.5),
     main = "", xlab = "X", col = "grey",
     las = 1, bty = "n")
lines(density(unlist(coef_sims$normal1), adjust = 2,
             from = plot_lims[1], to = plot_lims[2]))

plot(density(unlist(coef_sims$normal10), 
             from = plot_lims[1], to = plot_lims[2]),
     xlim = plot_lims, ylim = c(0, 0.5),
     main = "", xlab = "X", col = "grey",
     las = 1, bty = "n")
lines(density(unlist(coef_sims$lognormal), adjust = 2,
              from = plot_lims[1], to = plot_lims[2]))

plot(density(unlist(coef_sims$normal10), 
             from = plot_lims[1], to = plot_lims[2]),
     xlim = plot_lims, ylim = c(0, 0.5),
     main = "", xlab = "X", col = "grey",
     las = 1, bty = "n")
lines(density(unlist(coef_sims$cauchy1), adjust = 2,
              from = plot_lims[1], to = plot_lims[2]))
lines(density(unlist(coef_sims$cauchy2), adjust = 2,
              from = plot_lims[1], to = plot_lims[2]))

plot(density(unlist(coef_sims$normal10), 
             from = plot_lims[1], to = plot_lims[2]),
     xlim = plot_lims, ylim = c(0, 0.5),
     main = "", xlab = "X", col = "grey",
     las = 1, bty = "n")
lines(density(unlist(coef_sims$exp1), adjust = 2,
              from = plot_lims[1], to = plot_lims[2]))

plot(density(unlist(coef_sims$normal10), 
             from = plot_lims[1], to = plot_lims[2]),
     xlim = plot_lims, ylim = c(0, 0.5),
     main = "", xlab = "X", col = "grey",
     las = 1, bty = "n")
lines(density(unlist(coef_sims$ridge), adjust = 2,
              from = plot_lims[1], to = plot_lims[2]))
lines(density(unlist(coef_sims$lasso), adjust = 2,
              from = plot_lims[1], to = plot_lims[2]))

plot(density(unlist(coef_sims$normal10), 
             from = plot_lims[1], to = plot_lims[2]),
     xlim = plot_lims, ylim = c(0, 0.5),
     main = "", xlab = "X", col = "grey",
     las = 1, bty = "n")
lines(density(unlist(coef_sims$horseshoe), adjust = 2,
              from = plot_lims[1], to = plot_lims[2]), col = "red")
lines(density(unlist(coef_sims$finnish_horseshoe), adjust = 4,
              from = plot_lims[1], to = plot_lims[2]), col = "blue")

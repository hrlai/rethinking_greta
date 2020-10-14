# Chapter 5

library(rethinking)
library(greta)

# calculate SE of params
# following https://github.com/greta-dev/greta/issues/226
se_hessian <- function(opt) {
    out <-
        lapply(opt$hessian, function(h) {
            sqrt(diag(solve(h)))
        })
    return(out)
}


data(WaffleDivorce)
d <- WaffleDivorce
# standardize variables
d$A <- scale( d$MedianAgeMarriage )
d$D <- scale( d$Divorce )

D <- as_data(d$D)
A <- as_data(d$A)

alpha <- normal(0, 0.2)
beta <- normal(0, 0.5)
sigma <- exponential(1)

mu <- alpha + beta * A

distribution(D) <- normal(mu, sigma)

m5.1 <- model(alpha, beta, sigma)

opt5.1 <- opt(m5.1, hessian = TRUE)
opt5.1$se <- se_hessian(opt5.1)
opt5.1

# prior predictions
m5.1_sims <- calculate(alpha, beta, nsim = 50)
plot( NULL , xlim=c(-2,2) , ylim=c(-2,2) )
for (i in 1:50) {
    abline(m5.1_sims$alpha[i,,1], m5.1_sims$beta[i,,1], col=col.alpha("black",0.4))
}

# posterior predictions
plot( D ~ A , data=d , col=rangi2 )
abline(opt5.1$par$alpha , opt5.1$par$beta , lwd=2 )




# DAG play around ---------------------------------------------------------
library(dagitty)
DMA_dag2 <- dagitty("dag{D <- A -> M}")
impliedConditionalIndependencies(DMA_dag2)
drawdag(DMA_dag2)




# R code 5.18 -------------------------------------------------------------

N <- 100
x_real <- rnorm(N)
x_spur <- rnorm(N, x_real)
y <- rnorm(N, x_real)
d <- data.frame(y, x_real, x_spur)
pairs(d)
summary(lm(y ~ x_spur + x_real, d))

# single variable linear regression
gen_data <- function(N, b1, b0=0, xm=0, xsd=1) {
    obs <- rep(1:N)
    x <- rnorm(N, xm, xsd)
    y <- rnorm(N, b0+b1*x, (1-b1^2))
    return(data.frame(obs, y, x))
}

power_lm1 <- function(N, b1, b0=0, xm=0, xsd=1, n.sims=5000) {
    output <- data.frame(sim=numeric(n.sims), xm=numeric(n.sims),
        xsd=numeric(n.sims), ym=numeric(n.sims), ysd=numeric(n.sims),
        est=numeric(n.sims), se=numeric(n.sims), p=numeric(n.sims),
        sig=logical(n.sims))
    for (s in 1:n.sims) {
        sim <- gen_data(N, b1, b0, xm, xsd)
        model <- lm(y ~ x, data=sim)

        output[s, 'sim'] <- s
        output[s, 'xm'] <- mean(sim$x)
        output[s, 'xsd'] <- sd(sim$x)
        output[s, 'ym'] <- mean(sim$y)
        output[s, 'ysd'] <- sd(sim$y)
        output[s, 'est'] <- coef(summary(model))['x', 'Estimate']
        output[s, 'se'] <- coef(summary(model))['x', 'Std. Error']
        output[s, 'p'] <- coef(summary(model))['x', 'Pr(>|t|)']
        output[s, 'sig'] <- output[s, 'est'] > 0 & output[s, 'p'] <= .05
    }
    print(paste('Estimated power:', mean(output$sig)))
    return(output)
}

set.seed(100)
system.time(power <- power_lm1(N=343, b1=.15, b0=1, n.sims=10000))





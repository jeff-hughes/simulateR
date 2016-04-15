# single variable linear regression
gen_data <- function(N, coef=0, mean=0, sd=1) {
    obs <- rep(1:N)

    if (typeof(coef) == 'list') {
        if ('b0' %in% names(coef)) {
            b0 <- coef[['b0']]
        } else {
            b0 <- 0
        }

        if ('b1' %in% names(coef)) {
            b1 <- coef[['b1']]
        } else {
            b1 <- 0
        }
    } else if (is.numeric(coef)) {
        b0 <- coef
        b1 <- coef
    } else {
        b0 <- 0
        b1 <- 0
    }

    if (typeof(mean) == 'list') {
        if ('x' %in% names(mean)) {
            xm <- mean[['x']]
        } else {
            xm <- 0
        }
    } else if (is.numeric(mean)) {
        xm <- mean
    } else {
        xm <- 0
    }

    if (typeof(sd) == 'list') {
        if ('x' %in% names(sd)) {
            xsd <- sd[['x']]
        } else {
            xsd <- 1
        }
    } else if (is.numeric(sd)) {
        xsd <- sd
    } else {
        xsd <- 1
    }

    x <- rnorm(N, xm, xsd)
    y <- rnorm(N, b0+b1*x, (1-b1^2))
    return(data.frame(obs, y, x))
}

power_lm1 <- function(N, coef=0, mean=0, sd=1, n.sims=5000, seed=runif(1)*1000) {
    output <- matrix(nrow=n.sims, ncol=9, dimnames=list(NULL,
        c('sim', 'x mean', 'x sd', 'y mean', 'y sd', 'estimate', 'se', 'p', 'sig')))
    set.seed(seed)
    for (s in 1:n.sims) {
        sim <- gen_data(N, coef, mean, sd)
        model <- lm(y ~ x, data=sim)

        output[s, 'sim'] <- s
        output[s, 'x mean'] <- mean(sim$x)
        output[s, 'x sd'] <- sd(sim$x)
        output[s, 'y mean'] <- mean(sim$y)
        output[s, 'y sd'] <- sd(sim$y)
        output[s, 'estimate'] <- coef(summary(model))['x', 'Estimate']
        output[s, 'se'] <- coef(summary(model))['x', 'Std. Error']
        output[s, 'p'] <- coef(summary(model))['x', 'Pr(>|t|)']
        output[s, 'sig'] <- output[s, 'estimate'] > 0 & output[s, 'p'] <= .05
    }
    print(paste('Estimated power:', mean(output[, 'sig'])))

    output <- as.data.frame(output)
    output$sig <- as.logical(output$sig)
    return(output)
}

system.time(power <- power_lm1(N=343, coef=list(b1=.15), n.sims=5000, seed=100))
mean(power[['x mean']])
mean(power[['x sd']])
mean(power[['y mean']])
mean(power[['y sd']])
mean(power[['estimate']])
mean(power[['se']])
mean(power[['p']])
mean(power[['sig']])







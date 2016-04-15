pwr_sim <- function(func, params, n.sims=5000, ...) {
    grid <- expand.grid(params, KEEP.OUT.ATTRS=FALSE)  # cross each param value with others
    grid_output <- grid
    names(grid_output) <- paste0(names(grid_output), '.test')

    output <- NULL
    for (set in 1:nrow(grid)) {
        for (s in 1:n.sims) {
            result <- c(sim=s, unlist(grid_output[set, ]),
                do.call(func, args=c(as.list(grid[set, ]), ...)))

            # colnames get dropped when combined if only one column, so insert them back
            if (ncol(grid_output) == 1) {
                names(result)[2] <- colnames(grid_output)[1]
            }

            if (is.null(output)) {
                output <- result
            } else {
                output <- rbind(output, result)
            }
            row.names(output) <- NULL
        }
    }
    return(as.data.frame(output))
}


# test
lm_test <- function(N, b1, b0=0, xm=0, xsd=1) {
    obs <- rep(1:N)
    x <- rnorm(N, xm, xsd)
    y <- rnorm(N, b0 + b1*x, (1-b1^2))
    data <- data.frame(obs, y, x)
    model <- lm(y ~ x, data)

    est <- coef(summary(model))['x', 'Estimate']
    se <- coef(summary(model))['x', 'Std. Error']
    p <- coef(summary(model))['x', 'Pr(>|t|)']

    return(c(xm=mean(x), xsd=sd(x), ym=mean(y), ysd=sd(y), est=est, se=se, p=p,
        sig=est > 0 & p <= .05))
}

system.time(power <- pwr_sim(lm_test, params=list(N=c(100, 200)), n.sims=10, b1=.15))






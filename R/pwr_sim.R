#' Simulate statistical power.
#'
#' \code{pwr_sim} simulates data by generating samples according to a user-
#' defined function. These data can then be used to examine statistical power.
#' This function is intentionally kept general and flexible, to allow for a wide
#' variety of statistical models.
#'
#' @param func A user-defined function that generates data and performs a
#'   statistical test on that data.
#' @param params A list of parameters to be passed to \code{func}. Each of these
#'   parameters can be a vector of multiple values, and \code{pwr_sim} will test
#'   each combination of these values.
#' @param n.sims Number of simulations (per combination of params).
#' @param ... Additional arguments to be passed to \code{func}. If you do not
#'   need to vary certain parameters in your model, you can pass them to
#'   \code{func} here.
#' @return Returns a data frame with one row per simulation. \code{func} must
#'   return a (named) vector with the results you wish to capture; these vectors
#'   will then be combined for the final output.
#' @seealso \code{\link{boot}}
#' @examples
#' lm_test <- function(N, b0, b1) {
#' x <- rnorm(N, 0, 1)
#' y <- rnorm(N, b0 + b1*x, sqrt(1 - b1^2))
#' data <- data.frame(y, x)
#' model <- lm(y ~ x, data)
#'
#' # capture output from model summary
#' est <- coef(summary(model))['x', 'Estimate']
#' se <- coef(summary(model))['x', 'Std. Error']
#' p <- coef(summary(model))['x', 'Pr(>|t|)']
#'
#' return(c(xm=mean(x), xsd=sd(x), ym=mean(y), ysd=sd(y), est=est, se=se, p=p,
#'     sig=est > 0 & p <= .05))
#' }
#'
#' # test power for two sample sizes: N=200 and N=300, with 5000 sims for each
#' power <- pwr_sim(lm_test, params=list(N=c(200, 300)), n.sims=5000, b0=0, b1=.15)
#' @export
pwr_sim <- function(func, params, n.sims=5000, ...) {
    # cross each param value with every other one, to create all combinations
    grid <- expand.grid(params, KEEP.OUT.ATTRS=FALSE)
    grid_output <- grid
    names(grid_output) <- paste0(names(grid_output), '.test')

    cat('Running simulations...\n')
    totalSims <- nrow(grid) * n.sims
    progressBar <- txtProgressBar(min=0, max=totalSims, style=3)

    output <- NULL  # variable to fill with final output
    for (set in 1:nrow(grid)) {
        for (s in 1:n.sims) {
            result <- c(sim=s, unlist(grid_output[set, ]),
                do.call(func, args=c(as.list(grid[set, ]), ...)))
                # single line of output, from one simulation

            # colnames get dropped when combined if only one column, so insert
            # them back
            if (ncol(grid_output) == 1) {
                names(result)[2] <- colnames(grid_output)[1]
            }

            if (is.null(output)) {
                output <- result
            } else {
                output <- rbind(output, result)
            }
            row.names(output) <- NULL

            # update the progress bar
            if (s %% 20 == 0) {
                setTxtProgressBar(progressBar, (set-1)*n.sims + s)
            }
        }
    }
    return(as.data.frame(output))
}


#' Calculate error variance given model coefficients.
#'
#' \code{lm_error_var} will calculate the required error variance for a linear
#' model, given specified model coefficients, to create variance for your
#' dependent variable of approximately 1.
#'
#' @param ... Pass along all model coefficients, excluding the intercept. These
#'   can be named or unnamed
#' @return Returns the required error variance so that the variance of your
#'   dependent variable is approximately 1.
#' @examples
#' lm_error_var(b1=.15, b2=.3)  # returns error variance of 0.8875
#' @export
lm_error_var <- function(...) {
    dots <- list(...)
    exp_var <- 0
    for (i in 1:length(dots)) {
        exp_var <- exp_var + dots[[i]]^2
    }
    return(1-exp_var)
}


# test
# lm_test <- function(N, b1, b0=0, xm=0, xsd=1) {
#     obs <- rep(1:N)
#     x <- rnorm(N, xm, xsd)
#     y <- rnorm(N, b0 + b1*x, sqrt(lm_error_var(b1)))
#     data <- data.frame(obs, y, x)
#     model <- lm(y ~ x, data)
#
#     est <- coef(summary(model))['x', 'Estimate']
#     se <- coef(summary(model))['x', 'Std. Error']
#     p <- coef(summary(model))['x', 'Pr(>|t|)']
#
#     return(c(xm=mean(x), xsd=sd(x), ym=mean(y), ysd=sd(y), est=est, se=se, p=p,
#         sig=est > 0 & p <= .05))
# }
#
# system.time(power <- pwr_sim(lm_test, params=list(N=c(100, 200)), n.sims=500, b1=.15))






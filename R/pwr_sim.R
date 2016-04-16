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

    allResults <- NULL  # variable to fill with final output
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

            if (is.null(allResults)) {
                allResults <- result
            } else {
                allResults <- rbind(allResults, result)
            }
            row.names(allResults) <- NULL

            # update the progress bar
            if (s %% 20 == 0) {
                setTxtProgressBar(progressBar, (set-1)*n.sims + s)
            }
        }
    }

    allResults <- as.data.frame(allResults)

    output <- list(results=allResults, tests=grid, n.sims=n.sims)
    class(output) <- 'pwr_sim'
    return(output)
}





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
#' @param boot Whether or not to use bootstrapped data to pass along to your
#'   simulation.
#' @param bootParams If \code{boot=TRUE}, then use \code{bootParams} to pass
#'   along a named list of arguments to the \code{\link{boot}} function. The
#'   statistic and R parameters will be filled automatically, but at minimum you
#'   will need to pass along data. Information about parallel processing will
#'   also be passed along automatically.
#' @param parallel The type of parallel operation to be used (if any).
#' @param ncpus Integer: the number of processes to be used in parallel
#'   operation.
#' @param cl An optional \code{parallel} or \code{snow} cluster for use if
#'   \code{parallel = 'snow'}. If not supplied, a cluster on the local machine
#'   is created for the duration of the simulations.
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
pwr_sim <- function(func, params=NULL, n.sims=5000, boot=FALSE, bootParams=NULL,
    parallel=c('no', 'multicore', 'snow'), ncpus=1, cl=NULL, ...) {

    # cross each param value with every other one, to create all combinations
    grid <- expand.grid(params, KEEP.OUT.ATTRS=FALSE)
    grid_output <- grid
    names(grid_output) <- paste0(names(grid_output), '.test')

    totalSims <- nrow(grid) * n.sims
    cat(paste0('Running ',
        prettyNum(totalSims, big.mark=',', scientific=FALSE),
        ' simulations...\n'))

    progressBar <- NULL
    if (!boot && totalSims >= 100) {
        progressBar <- txtProgressBar(min=0, max=totalSims, style=3, width=60)
    }

    allResults <- NULL  # variable to fill with final output
    for (set in 1:nrow(grid)) {
        # bootstrap data
        if (boot && 'data' %in% names(bootParams)) {
            boot_output <- do.call(boot::boot, args=c(list(statistic=func, R=n.sims),
                bootParams, as.list(grid[set, , drop=FALSE]), ...))

            boot_names <- names(boot_output$t0)
            boot_data <- boot_output$t
            colnames(boot_data) <- boot_names

            extendGrid <- matrix(rep(unlist(grid_output[set, , drop=FALSE]),
                each=n.sims), nrow=n.sims)
            colnames(extendGrid) <- names(grid_output)
            result <- cbind(sim=1:n.sims, extendGrid, boot_data)

            if (is.null(allResults)) {
                allResults <- result
            } else {
                allResults <- rbind(allResults, result)
            }
            row.names(allResults) <- NULL

        # simulate data
        } else {
            for (s in 1:n.sims) {
                result <- c(sim=s, unlist(grid_output[set, , drop=FALSE]),
                    do.call(func, args=c(as.list(grid[set, , drop=FALSE]), ...)))
                    # single line of output, from one simulation

                if (is.null(allResults)) {
                    allResults <- result
                } else {
                    allResults <- rbind(allResults, result)
                }
                row.names(allResults) <- NULL

                # update the progress bar
                if (!is.null(progressBar) && s %% 20 == 0) {
                    setTxtProgressBar(progressBar, (set-1)*n.sims + s)
                }
            }
        }
    }

    allResults <- as.data.frame(allResults)

    output <- list(results=allResults, tests=grid, n.sims=n.sims)
    class(output) <- 'pwr_sim'
    return(output)
}





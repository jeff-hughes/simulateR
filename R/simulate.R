#' Simulate data and statistical models.
#'
#' \code{simulate} simulates data by generating samples according to a user-
#' defined function. These data can then be used to examine whatever statistic
#' you wish. This function is intentionally kept general and flexible, to allow
#' for a wide variety of applications.
#'
#' @param func A user-defined function that generates data and performs a
#'   statistical test on that data.
#' @param params A list of parameters to be passed to \code{func}. Each of these
#'   parameters can be a vector of multiple values, and \code{simulate} will
#'   test each combination of these values.
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
#' power_sim <- simulate(lm_test, params=list(N=c(200, 300)), n.sims=5000, b0=0, b1=.15)
#' @export
simulate <- function(func, params=NULL, n.sims=5000, boot=FALSE, bootParams=NULL,
    parallel=c('no', 'multicore', 'snow'), ncpus=1, cl=NULL, ...) {

    # cross each param value with every other one, to create all combinations
    if (!is.null(params)) {
        grid <- expand.grid(params, KEEP.OUT.ATTRS=FALSE)
        grid_output <- grid
        names(grid_output) <- paste0(names(grid_output), '.test')
        nSets <- nrow(grid)
    } else {
        grid <- data.frame()  # empty
        grid_output <- data.frame()
        nSets <- 1
    }

    # figure out which parallel method to use
    parallel <- match.arg(parallel)
    have_mc <- FALSE
    have_snow <- FALSE
    if (parallel != 'no' && ncpus > 1) {
        if (requireNamespace('parallel', quietly=TRUE)) {
            if (parallel == 'multicore') {
                have_mc <- .Platform$OS.type != 'windows'
                if (!have_mc) {
                    warning('Multicore is not supported on Windows. Try parallel = "snow" instead. Proceeding with simulations serially.')
                }
            } else if (parallel == 'snow') {
                have_snow <- TRUE
            }
        } else {
            warning("Loading package 'parallel' failed. Proceeding with simulations serially.")
        }
        if (!have_mc && !have_snow) {
            ncpus <- 1
        }
    }

    # set up cluster if necessary
    if (ncpus > 1 && have_snow) {
        if (is.null(cl)) {
            clust <- parallel::makePSOCKcluster(rep('localhost',
                ncpus))
            if (RNGkind()[1] == "L'Ecuyer-CMRG") {
                parallel::clusterSetRNGStream(clust)
            }
        } else {
            clust <- cl
        }
    }

    cat(paste0('Running ',
        prettyNum(nSets * n.sims, big.mark=',', scientific=FALSE),
        ' simulations...\n'))

    allResults <- NULL  # variable to fill with final output
    for (set in 1:nSets) {
        # bootstrap data
        if (boot && 'data' %in% names(bootParams)) {
            boot_output <- do.call(boot::boot, args=c(list(statistic=func,
                R=n.sims, parallel=parallel, ncpus=ncpus, cl=cl), bootParams,
                as.list(grid[set, , drop=FALSE]), ...))

            col_names <- names(boot_output$t0)
            output <- boot_output$t
            colnames(output) <- col_names

        # simulate data
        } else {
            if (ncpus > 1 &&
                (have_mc || have_snow)) {

                if (have_mc) {
                    output <- do.call(parallel::mclapply,
                        args=c(list(X=1:n.sims, FUN=func, mc.cores=ncpus),
                            as.list(grid[set, , drop=FALSE]), ...))
                } else if (have_snow) {
                    output <- do.call(parallel::parLapply,
                        args=c(list(cl=clust, X=1:n.sims, fun=func),
                            as.list(grid[set, , drop=FALSE]), ...))
                }
            } else {
                output <- do.call(lapply, args=c(list(X=1:n.sims, FUN=func),
                    as.list(grid[set, , drop=FALSE]), ...))
            }

            # convert list to data frame
            col_names <- names(output[[1]])
            output <- do.call(rbind.data.frame, output)
            colnames(output) <- col_names
        }

        if (!is.null(params)) {
            extendGrid <- matrix(rep(unlist(grid_output[set, , drop=FALSE]),
                each=n.sims), nrow=n.sims)
            colnames(extendGrid) <- names(grid_output)
            result <- cbind(sim=1:n.sims, extendGrid, output)
        } else {
            result <- cbind(sim=1:n.sims, output)
        }

        if (is.null(allResults)) {
            allResults <- result
        } else {
            allResults <- rbind(allResults, result)
        }
        row.names(allResults) <- NULL
    }

    # stop cluster if we created it
    if (ncpus > 1 && have_snow && is.null(cl)) {
        parallel::stopCluster(clust)
    }

    allResults <- as.data.frame(allResults)

    output <- list(results=allResults, tests=grid, n.sims=n.sims)
    class(output) <- 'simulation'
    return(output)
}





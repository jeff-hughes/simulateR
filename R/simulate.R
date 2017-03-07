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
#' @param output Specifies how the function provides the ultimate results of the
#'   simulations: can return a "list", a "dataframe", or a "vector". Note that
#'   the output from the supplied function must be able to be coerced into this
#'   output type.
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
#' @param beep Include a numeric value or character vector indicating the sound
#'   you wish to play once the simulation is done running. Requires the "beepr"
#'   package, and information about supported values is available in the
#'   documentation for that package.
#' @param ... Additional arguments to be passed to \code{func}. If you do not
#'   need to vary certain parameters in your model, you can pass them to
#'   \code{func} here.
#' @return Returns a list (by default) with one element per simulation. If
#'   \code{output} is specified as "dataframe", then \code{func} must
#'   return a (named) vector with the results you wish to capture; if
#'   \code{output} is specified as "vector", then \code{func} must return a
#'   one-element vector.
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
simulate <- function(func, params=NULL, n.sims=5000,
    output=c('list', 'dataframe', 'vector'), boot=FALSE, bootParams=NULL,
    parallel=c('no', 'multicore', 'snow'), ncpus=1, cl=NULL, beep=NULL, ...) {

    dots <- list(...)
    outputType <- match.arg(output)

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
                c(as.list(grid[set, , drop=FALSE]), dots)))

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
                            c(as.list(grid[set, , drop=FALSE]), dots)))
                } else if (have_snow) {
                    output <- do.call(parallel::parLapply,
                        args=c(list(cl=clust, X=1:n.sims, fun=func),
                            c(as.list(grid[set, , drop=FALSE]), dots)))
                }
            } else {
                output <- do.call(lapply, args=c(list(X=1:n.sims, FUN=func),
                    c(as.list(grid[set, , drop=FALSE]), dots)))
            }

            # convert output to data frame/vector if requested
            if (outputType == 'dataframe') {
                col_names <- names(output[[1]])
                output <- do.call(rbind.data.frame, output)
                colnames(output) <- col_names
            } else if (outputType == 'vector') {
                output <- unlist(output)
            }
        }

        if (outputType == 'dataframe') {
            rowsEachSim <- nrow(output) / n.sims
            if (!is.null(params)) {
                extendGrid <- matrix(rep(unlist(grid_output[set, , drop=FALSE]),
                    each=n.sims), nrow=n.sims)
                colnames(extendGrid) <- names(grid_output)

                result <- cbind(sim=rep(1:n.sims, each=rowsEachSim), extendGrid, output)
            } else {
                result <- cbind(sim=rep(1:n.sims, each=rowsEachSim), output)
            }

            if (is.null(allResults)) {
                allResults <- result
            } else {
                allResults <- rbind(allResults, result)
            }
            row.names(allResults) <- NULL
        } else {
            if (is.null(allResults)) {
                allResults <- output
            } else {
                allResults <- c(allResults, output)
            }
        }


    }

    # stop cluster if we created it
    if (ncpus > 1 && have_snow && is.null(cl)) {
        parallel::stopCluster(clust)
    }

    allResults <- as.data.frame(allResults)

    output <- list(results=allResults, tests=grid, n.sims=n.sims)
    class(output) <- 'simulation'

    # Ding! Fries are done
    if (!is.null(beep)) {
        if (requireNamespace('beepr', quietly=TRUE)) {
            beepr::beep(beep)
        }
    }

    return(output)
}





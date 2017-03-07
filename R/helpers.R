#' Return results of a simulation.
#'
#' \code{results} is a generic function that extracts the raw data from a
#' simulation.
#'
#' @param sim A simulation object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{results} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{results.simulation}}
#' @export
results <- function(sim, ...) UseMethod('results')


#' Return results of a simulation.
#'
#' \code{results.simulation} returns the raw data from a simulation.
#'
#' @param sim A simulation object of type 'simulation'.
#' @return Returns a data frame with all the data returned from each simulation.
#' @export
results.simulation <- function(sim) {
    return(sim$results)
}


#' Return the tests performed by a simulation.
#'
#' \code{tests} is a generic function that extracts information about the set of
#' specific tests (parameter values) for a simulation.
#'
#' @param sim A simulation object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{tests} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{tests.simulation}}
#' @export
tests <- function(sim, ...) UseMethod('tests')


#' Return the tests performed by a simulation.
#'
#' \code{tests.simulation} extracts information about the set of specific tests
#' (parameter values) for a simulation.
#'
#' @param sim A simulation object of type 'simulation'.
#' @return Returns a data frame with one row for each set of simulations that
#'   was performed.
#' @export
tests.simulation <- function(sim) {
    return(sim$tests)
}


#' Return the number of simulations performed by a simulation.
#'
#' \code{n.sims} is a generic function that extracts information about the
#' number of simulations (per specific test) performed by a simulation.
#'
#' @param sim A simulation object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{n.sims} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{n.sims.simulation}}
#' @export
n.sims <- function(sim, ...) UseMethod('n.sims')


#' Return the number of simulations performed by a simulation.
#'
#' \code{n.sims.simulation} extracts information about the number of simulations
#' (per specific test) performed by a simulation.
#'
#' @param sim A simulation object of type 'simulation'.
#' @return Returns the number of simulations done in each test.
#' @export
n.sims.simulation <- function(sim) {
    return(sim$n.sims)
}


#' Return the timing information of a simulation.
#'
#' \code{timing} is a generic function that extracts the raw data from a
#' simulation.
#'
#' @param sim A simulation object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{timing} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{timing.simulation}}
#' @export
timing <- function(sim, ...) UseMethod('timing')


#' Return the timing information of a simulation.
#'
#' \code{timing.simulation} returns the raw data from a simulation.
#'
#' @param sim A simulation object of type 'simulation'.
#' @return Returns an object of class "proc_time" with information about how
#'   long the simulation process took.
#' @export
timing.simulation <- function(sim) {
    return(sim$timing)
}


#' Calculate error variance given model coefficients.
#'
#' \code{lm_error_var} will calculate the required error variance for a linear
#' model, given specified model coefficients, to create variance for your
#' dependent variable of approximately 'var'.
#'
#' \strong{Note:} This function assumes that \emph{all predictors are
#' independent} (i.e., uncorrelated).
#'
#' @param var The variance you wish your dependent variable to be.
#' @param ... Pass along all model coefficients, excluding the intercept. These
#'   can be named or unnamed.
#' @return Returns the required error variance so that the variance of your
#'   dependent variable is approximately 'var'.
#' @examples
#' lm_error_var(var=1, .15, .3)  # returns error variance of 0.8875
#' @export
lm_error_var <- function(var=1, ...) {
    dots <- list(...)
    exp_var <- 0
    for (i in 1:length(dots)) {
        exp_var <- exp_var + dots[[i]]^2
    }
    return(var - exp_var)
}






#' Return results of a power simulation.
#'
#' \code{results} is a generic function that extracts the raw data from a power
#' simulation.
#'
#' @param sim A power simulation object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{results} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{results.pwr_sim}}
#' @export
results <- function(sim, ...) UseMethod('results')


#' Return results of a power simulation.
#'
#' \code{results.pwr_sim} returns the raw data from a power simulation.
#'
#' @param sim A power simulation object of type 'pwr_sim'.
#' @return Returns a data frame with all the data returned from each simulation.
#' @export
results.pwr_sim <- function(sim) {
    return(sim$results)
}


#' Return the tests performed by a power simulation.
#'
#' \code{tests} is a generic function that extracts information about the set of
#' specific tests (parameter values) for a power simulation.
#'
#' @param sim A power simulation object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{tests} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{tests.pwr_sim}}
#' @export
tests <- function(sim, ...) UseMethod('tests')


#' Return the tests performed by a power simulation.
#'
#' \code{results.pwr_sim} extracts information about the set of specific tests
#' (parameter values) for a power simulation.
#'
#' @param sim A power simulation object of type 'pwr_sim'.
#' @return Returns a data frame with one row for each set of simulations that
#'   was performed.
#' @export
tests.pwr_sim <- function(sim) {
    return(sim$tests)
}


#' Return the number of simulations performed by a power simulation.
#'
#' \code{n.sims} is a generic function that extracts information about the
#' number of simulations (per specific test) performed by a power simulation.
#'
#' @param sim A power simulation object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{n.sims} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{n.sims.pwr_sim}}
#' @export
n.sims <- function(sim, ...) UseMethod('n.sims')


#' Return the number of simulations performed by a power simulation.
#'
#' \code{n.sims.pwr_sim} extracts information about the number of simulations
#' (per specific test) performed by a power simulation.
#'
#' @param sim A power simulation object of type 'pwr_sim'.
#' @return Returns the number of simulations done in each test.
#' @export
n.sims.pwr_sim <- function(sim) {
    return(sim$n.sims)
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

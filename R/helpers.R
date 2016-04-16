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

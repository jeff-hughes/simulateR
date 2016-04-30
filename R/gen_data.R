#' Generate data through a factor matrix and effects matrix.
#'
#' \code{gen_data} will generate sample data based on a factor structure and
#' effects structure specified by the user.
#'
#' @param factor_struct A matrix describing the measurement model of latent
#'   factors (columns) as measured by observed variables (rows).
#' @param effects_struct A matrix describing the variances and covariances of
#'   the latent variables in the model.
#' @param nCases Number of sample cases to generate.
#' @param trueScores Whether or not to include the data for each variable as
#'   measured without error. If set to TRUE, the resulting data frame will
#'   include all the variables in the model twice: once with measurement error,
#'   and once without.
#' @return Returns a data frame with \code{nCases} rows and columns for each
#'   observed and latent variable. These variables will approximately accord
#'   with the factor structure and effects structure that was specified, within
#'   sampling error.
#' @examples
#' # two uncorrelated predictors, one criterion, with measurement error in all
#' # variables
#' beta1 <- .5
#' beta2 <- .6
#' y_resid_var <- sqrt(1 - (beta1^2 + beta2^2))
#' fmodel <- matrix(
#'     c(.8, 0, 0,   # x1
#'       0, .6, 0,   # x2
#'       0, 0, .5),  # y
#'     nrow=3, ncol=3, byrow=TRUE, dimnames=list(
#'     c('x1', 'x2', 'y'), c('x1', 'x2', 'y')))
#'     # in this case, observed and latent variables are the same
#' effects <- matrix(
#'     c(1, 0, beta1,
#'       0, 1, beta2,
#'       0, 0, y_resid_var),
#'     nrow=3, ncol=3, byrow=TRUE, dimnames=list(
#'     c('x1', 'x2', 'y'), c('x1', 'x2', 'y')))
#'
#' sample_data <- gen_data(fmodel, effects, nCases=1000)
#' round(var(sample_data), 2)
#' round(cor(sample_data), 2)
#' summary(lm(y ~ x1 + x2, data=sample_data))
#'     # note that beta coefficients are much smaller, due to measurement error
#' @export
gen_data <- function(factor_struct, effects_struct, nCases=1000,
    trueScores=FALSE) {

    nVars <- dim(factor_struct)[1]  # problem size determined by input to the function
    nLatent <- dim(factor_struct)[2]

    tmodel <- t(factor_struct)  # transpose of model

    communality <- diag(factor_struct %*% tmodel)
        # find how much to weight true scores and errors given the measurement model
    uniqueness <- 1 - communality
    error_weight <- diag(sqrt(uniqueness))  # how much to weight the errors

    latent_scores <- matrix(rnorm(nCases * nLatent), nCases)
        # generate true scores for the latent variables
    latent_scores <- latent_scores %*% effects_struct
        # ensure true scores reflect structural relations between the factors

    true_scores <- latent_scores %*% tmodel
    error <- matrix(rnorm(nCases * nVars), nCases)
        # generate normal error
    wtd_error <- error %*% error_weight
    observed_scores <- true_scores + wtd_error

    if (trueScores) {
        colnames(true_scores) <- paste0(colnames(observed_scores), '_true')
        return(data.frame(observed_scores, true_scores))
    } else {
        return(data.frame(observed_scores))
    }
}






#' Test Longitudinal Measurement Invariance
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param data Dataframe.
#'   The function assumes that the data is in the wide format
#'   and the variables are named as follows:
#'   `paste0("y", time_point, "_", item_number)`.
#'   For example, for the item 1 from the first time point,
#'   the variable name should be `y1_1`.
#' @param time_points Numeric vector of discrete time points.
#' @param factor_loadings List with length equal to the number of factors.
#'   Each element of the list is the item number of items
#'   for the specific factor.
#' @param covariances Logical.
#'   If `covariance = TRUE`,
#'   model the covariances of the measurement error.
#' @param model_add_configural Additional specification
#'   added to the lavaan model syntax
#'   for the configural invariance model.
#' @param model_add_weak Additional specification
#'   added to the lavaan model syntax
#'   for the weak invariance model.
#' @param model_add_strong Additional specification
#'   added to the lavaan model syntax
#'   for the strong invariance model.
#' @param model_add_strict Additional specification
#'   added to the lavaan model syntax
#'   for the strict invariance model.
#' @param ... Additional arguments to pass to [lavaan::cfa()].
#' @return Returns an object of class `longmi` which is
#'   a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{List of function arguments.}
#'     \item{fit}{Fitted models.}
#'     \item{measures}{Fit measures.}
#'     \item{fun}{Function used ("Invariance").}
#'   }
#' @examples
#' data("osbornesudick1972", package = "longMI")
#' mi <- Invariance(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' print(mi)
#' summary(mi)
#' @export
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance invariance
Invariance <- function(data,
                       time_points,
                       factor_loadings,
                       covariances = FALSE,
                       model_add_configural = NULL,
                       model_add_weak = NULL,
                       model_add_strong = NULL,
                       model_add_strict = NULL,
                       ...) {
  models <- c(
    "configural",
    "weak",
    "strong",
    "strict"
  )
  configural <- .Fit(
    type = 0,
    data = data,
    time_points = time_points,
    factor_loadings = factor_loadings,
    covariances = covariances,
    model_add = model_add_configural,
    ...
  )
  weak <- .Fit(
    type = 1,
    data = data,
    time_points = time_points,
    factor_loadings = factor_loadings,
    covariances = covariances,
    model_add = model_add_weak,
    ...
  )
  strong <- .Fit(
    type = 2,
    data = data,
    time_points = time_points,
    factor_loadings = factor_loadings,
    covariances = covariances,
    model_add = model_add_strong,
    ...
  )
  strict <- .Fit(
    type = 3,
    data = data,
    time_points = time_points,
    factor_loadings = factor_loadings,
    covariances = covariances,
    model_add = model_add_strict,
    ...
  )
  fit <- list(
    configural = configural,
    weak = weak,
    strong = strong,
    strict = strict
  )
  fit_measures <- lapply(
    X = fit,
    FUN = lavaan::inspect,
    what = "fit.measures"
  )
  names(fit_measures) <- models
  fit_measures <- do.call(
    what = "cbind",
    args = fit_measures
  )
  out <- list(
    call = match.call(),
    args = list(
      data = data,
      time_points = time_points,
      factor_loadings = factor_loadings,
      covariances = covariances,
      model_add_configural = model_add_configural,
      model_add_weak = model_add_weak,
      model_add_strong = model_add_strong,
      model_add_strict = model_add_strict,
      cfa_args = ...
    ),
    fit = fit,
    measures = fit_measures,
    fun = "Invariance"
  )
  class(out) <- c(
    "longmi",
    class(out)
  )
  return(
    out
  )
}

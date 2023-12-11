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
#' @param model_add_configural Additional specification added to the lavaan model syntax
#'   for the configural invariance model.
#' @param model_add_weak Additional specification added to the lavaan model syntax
#'   for the weak invariance model.
#' @param model_add_strong Additional specification added to the lavaan model syntax
#'   for the strong invariance model.
#' @param model_add_strict Additional specification added to the lavaan model syntax
#'   for the strict invariance model.
#' @param ... Additional arguments to pass to [lavaan::cfa()].
#' @examples
#' data("osbornesudick1972", package = "longMI")
#' mi <- Invariance(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' names(mi)
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
  pairs <- as.data.frame(
    utils::combn(
      x = models,
      m = 2
    )
  )
  diff <- lapply(
    X = pairs,
    FUN = function(x,
                   fit) {
      return(
        lavaan::lavTestLRT(
          fit[[x[1]]],
          fit[[x[2]]],
          model.names = c(x[1], x[2])
        )
      )
    },
    fit = fit
  )
  diff_names <- lapply(
    X = pairs,
    FUN = function(x) {
      return(
        paste(x[1], "against", x[2])
      )
    }
  )
  dim(diff_names) <- NULL
  names(diff) <- diff_names
  return(
    list(
      fit = fit,
      fit_measures = fit_measures,
      difference = diff
    )
  )
}

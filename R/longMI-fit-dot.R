#' Fit Invariance Model
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param type Integer.
#'   If `type = 0`, fit configural invariance model.
#'   If `type = 1`, fit weak invariance model.
#'   If `type = 2`, fit strong invariance model.
#'   If `type = 3`, fit strict invariance model.
#' @param data Dataframe.
#'   The function assumes that the data is in the wide format
#'   and the variables are named as follows:
#'   paste0("y", time_point, "_", item_number).
#'   For example, for the item 1 from the first time point,
#'   the variable name should be `y1_1`.
#' @param time_points Numeric vector of discrete time points.
#' @param factor_loadings List with length equal to the number of factors.
#'   Each element of the list is the item number of items
#'   for the specific factor.
#' @param covariances Logical.
#'   If `covariance = TRUE`, model the covariances of the measurement error.
#' @param model_add Additional specification added to the lavaan model syntax.
#' @param ... Additional arguments to pass to [lavaan::cfa()].
#' @examples
#' data("osbornesudick1972", package = "longMI")
#' fit <- .Fit(
#'   type = 0,
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' library(lavaan)
#' summary(fit)
#' @noRd
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance fit
.Fit <- function(type,
                 data,
                 time_points,
                 factor_loadings,
                 covariances = FALSE,
                 model_add = NULL,
                 ...) {
  if (type == 0) {
    # configural
    loadings_constrained <- FALSE
    intercepts_constrained <- FALSE
    variances_constrained <- FALSE
  }
  if (type == 1) {
    # weak
    loadings_constrained <- TRUE
    intercepts_constrained <- FALSE
    variances_constrained <- FALSE
  }
  if (type == 2) {
    # strong
    loadings_constrained <- TRUE
    intercepts_constrained <- TRUE
    variances_constrained <- FALSE
  }
  if (type == 3) {
    # strict
    loadings_constrained <- TRUE
    intercepts_constrained <- TRUE
    variances_constrained <- TRUE
  }
  items <- sort(
    unique(
      unlist(
        factor_loadings
      )
    )
  )
  first_item <- sapply(
    X = factor_loadings,
    FUN = function(x) {
      return(x[1])
    }
  )
  loadings <- .Loadings(
    time_points = time_points,
    factor_loadings = factor_loadings,
    equal = loadings_constrained
  )
  intercepts <- .Intercepts(
    time_points = time_points,
    items = items,
    first_item = first_item,
    equal = intercepts_constrained
  )
  variances <- .Variances(
    time_points = time_points,
    items = items,
    first_item = first_item,
    equal = variances_constrained
  )
  if (covariances) {
    covariances <- .Covariances(
      time_points = time_points,
      items = items,
      equal = variances_constrained
    )
  } else {
    covariances <- ""
  }
  factors <- .Factors(
    time_points = time_points,
    factors = length(factor_loadings)
  )
  if (is.null(model_add)) {
    model_add <- ""
  }
  model <- paste0(
    loadings,
    intercepts,
    variances,
    factors,
    covariances,
    "\n",
    model_add,
    collapse = "\n"
  )
  return(
    lavaan::cfa(
      model = model,
      data = data,
      ...
    )
  )
}

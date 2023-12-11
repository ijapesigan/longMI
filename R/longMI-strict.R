#' Strict Invariance Model
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams Invariance
#' @examples
#' data("osbornesudick1972", package = "longMI")
#' strict_fit <- Strict(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' library(lavaan)
#' summary(strict_fit)
#' @export
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance strict
Strict <- function(data,
                   time_points,
                   factor_loadings,
                   covariances = FALSE,
                   model_add = NULL,
                   ...) {
  return(
    .Fit(
      type = 3,
      data = data,
      time_points = time_points,
      factor_loadings = factor_loadings,
      covariances = covariances,
      model_add = model_add,
      ...
    )
  )
}

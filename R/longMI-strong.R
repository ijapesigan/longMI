#' Strong Invariance Model
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams Configural
#' @examples
#' data("osbornesudick1972", package = "longMI")
#' strong_fit <- Strong(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' library(lavaan)
#' summary(strong_fit)
#' @export
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance strong
Strong <- function(data,
                   time_points,
                   factor_loadings,
                   covariances = FALSE,
                   model_add = NULL,
                   ...) {
  return(
    .Fit(
      type = 2,
      data = data,
      time_points = time_points,
      factor_loadings = factor_loadings,
      covariances = covariances,
      model_add = model_add,
      ...
    )
  )
}

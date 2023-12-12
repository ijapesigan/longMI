#' Configural Invariance Model
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams Invariance
#' @param model_add Additional specification added to the lavaan model syntax.
#' @return Returns a fitted `lavaan` object.
#' @examples
#' data("osbornesudick1972", package = "longMI")
#' configural_fit <- Configural(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' library(lavaan)
#' summary(configural_fit)
#' @export
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance configural
Configural <- function(data,
                       time_points,
                       factor_loadings,
                       covariances = FALSE,
                       model_add = NULL,
                       ...) {
  return(
    .Fit(
      type = 0,
      data = data,
      time_points = time_points,
      factor_loadings = factor_loadings,
      covariances = covariances,
      model_add = model_add,
      ...
    )
  )
}

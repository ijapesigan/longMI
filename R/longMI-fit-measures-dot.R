#' Fit Measures
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `longmi`
#'   that is, the output of the [longMI::Invariance()]
#'   or the [longMI::Comparison()] functions.
#' @param measures Character vector of fit measures.
#' @return Returns a matrix of fit measures
#' @examples
#' data("osbornesudick1972", package = "longMI")
#' mi <- Invariance(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' .FitMeasures(mi)
#' @noRd
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance fit
.FitMeasures <- function(object,
                         measures = c(
                           "chisq",
                           "df",
                           "pvalue",
                           "cfi",
                           "tli",
                           "rmsea",
                           "srmr",
                           "aic",
                           "bic"
                         )) {
  fit <- object$fit
  fit <- fit[
    !sapply(
      X = fit,
      FUN = is.null
    )
  ]
  out <- lapply(
    X = fit,
    FUN = lavaan::fitMeasures,
    fit.measures = measures
  )
  out <- do.call(
    what = "rbind",
    args = out
  )
  rownames(out) <- names(fit)
  return(out)
}

#' Print Method for an Object of Class `longmi`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   selected fit measures.
#'
#' @param x Object of class `longmi`.
#' @param ... additional arguments.
#' @param digits Digits to print.
#'
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
#'
#' @keywords methods
#' @export
print.longmi <- function(x,
                         digits = 4,
                         ...) {
  models <- colnames(x$fit_measures)
  chisq <- x$fit_measures["chisq", ]
  df <- x$fit_measures["df", ]
  pvalue <- x$fit_measures["pvalue", ]
  cfi <- x$fit_measures["cfi", ]
  tli <- x$fit_measures["tli", ]
  rmsea <- x$fit_measures["rmsea", ]
  srmr <- x$fit_measures["srmr", ]
  aic <- x$fit_measures["aic", ]
  bic <- x$fit_measures["bic", ]
  out <- cbind(
    chisq = chisq,
    df = df,
    pvalue = pvalue,
    cfi = cfi,
    tli = tli,
    rmsea = rmsea,
    srmr = srmr,
    aic = aic,
    bic = bic
  )
  rownames(out) <- models
  base::print(
    round(
      out,
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `longmi`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a list of
#'   likelihood ratio tests.
#'
#' @param object Object of class `longmi`.
#' @param ... additional arguments.
#'
#' @examples
#' data("osbornesudick1972", package = "longMI")
#' mi <- Invariance(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' summary(mi)
#'
#' @keywords methods
#' @export
summary.longmi <- function(object,
                           ...) {
  return(
    object$difference
  )
}

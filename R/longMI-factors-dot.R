#' Latent Factors
#'
#' Specify the means and the variances of the latent factors
#' in the `lavaan` model syntax.
#'
#' @details The function assumes that the data is in the wide format
#'   and the variables are named as follows:
#'   `paste0("y", time_point, "_", item_number)`.
#'   For example, for the item 1 from the first time point,
#'   the variable name should be `y1_1`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param time_points Numeric vector of discrete time points.
#' @param factors Positive integer.
#'   Number of factors.
#' @examples
#' y <- .Factors(
#'   time_points = c(1, 2, 4, 5),
#'   factors = 5
#' )
#' fn <- tempfile()
#' fileConn <- file(fn)
#' writeLines(text = y, con = fn)
#' close(fileConn)
#' readLines(fn)
#' @noRd
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance factors
.Factors <- function(time_points,
                     factors) {
  nt <- length(time_points)
  factor_means <- factor_variances <- matrix(
    data = NA,
    nrow = factors,
    ncol = nt
  )
  for (i in seq_len(factors)) {
    for (j in seq_len(nt)) {
      if (j == 1) {
        factor_means[i, j] <- paste0(
          "f",
          i,
          "t",
          time_points[j],
          " ~ 0 * 1"
        )
        factor_variances[i, j] <- paste0(
          "f",
          i,
          "t",
          time_points[j],
          " ~~ 1 * ",
          "f",
          i,
          "t",
          time_points[j]
        )
      } else {
        factor_means[i, j] <- paste0(
          "f",
          i,
          "t",
          time_points[j],
          " ~ 1"
        )
        factor_variances[i, j] <- paste0(
          "f",
          i,
          "t",
          time_points[j],
          " ~~ ",
          "f",
          i,
          "t",
          time_points[j]
        )
      }
    }
  }
  dim(factor_means) <- NULL
  dim(factor_variances) <- NULL
  factor <- c(factor_means, factor_variances)
  factor <- paste0(
    "\n",
    factor,
    collapse = "\n"
  )
  return(factor)
}

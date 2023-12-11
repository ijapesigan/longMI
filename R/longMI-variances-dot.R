#' Measurement Error Variances
#'
#' Specify the measurement error variances in the `lavaan` model syntax.
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
#' @param items Numeric vector of item numbers.
#' @param first_item Numeric vector of the items numbers
#'   of first items per factor.
#' @param equal Logical.
#'   If `equal = TRUE`,
#'   measurement error variances are constrained to be equal across time.
#'   If `equal = FALSE`,
#'   measurement error variances are free to vary.
#' @examples
#' # equal = FALSE
#' y <- .Variances(
#'   time_points = c(1, 2, 4, 5),
#'   items = 1:25,
#'   first_item = c(1, 6, 11, 16, 21),
#'   equal = FALSE
#' )
#' fn <- tempfile()
#' fileConn <- file(fn)
#' writeLines(text = y, con = fn)
#' close(fileConn)
#' readLines(fn)
#'
#' # equal = TRUE
#' y <- .Variances(
#'   time_points = c(1, 2, 4, 5),
#'   items = 1:25,
#'   first_item = c(1, 6, 11, 16, 21),
#'   equal = TRUE
#' )
#' fn <- tempfile()
#' fileConn <- file(fn)
#' writeLines(text = y, con = fn)
#' close(fileConn)
#' readLines(fn)
#' @noRd
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance variances
.Variances <- function(time_points,
                       items,
                       first_item,
                       equal) {
  nt <- length(time_points)
  ni <- length(items)
  variances <- matrix(
    data = NA,
    nrow = ni,
    ncol = nt
  )
  for (i in seq_len(ni)) {
    for (j in seq_len(nt)) {
      if (equal) {
        variances[i, j] <- paste0(
          "y",
          time_points[j],
          "_",
          items[i],
          " ~~ ",
          "u",
          items[i],
          " * ",
          "y",
          time_points[j],
          "_",
          items[i]
        )
      } else {
        variances[i, j] <- paste0(
          "y",
          time_points[j],
          "_",
          items[i],
          " ~~ ",
          "y",
          time_points[j],
          "_",
          items[i]
        )
      }
    }
  }
  dim(variances) <- NULL
  variances <- paste0(
    "\n",
    variances,
    collapse = "\n"
  )
  return(variances)
}

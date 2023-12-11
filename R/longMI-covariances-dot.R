#' Measurement Error Covariances
#'
#' Specify the measurement error covariances in the `lavaan` model syntax.
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
#' @param equal Logical.
#'   If `equal = TRUE`,
#'   item measurement error covariances are constrained to be equal across time.
#'   If `equal = FALSE`,
#'   item measurement error covariances are free to vary.
#' @examples
#' # equal = FALSE
#' y <- .Covariances(
#'   time_points = c(1, 2, 4, 5),
#'   items = 1:25,
#'   equal = FALSE
#' )
#' fn <- tempfile()
#' fileConn <- file(fn)
#' writeLines(text = y, con = fn)
#' close(fileConn)
#' readLines(fn)
#'
#' # equal = TRUE
#' y <- .Covariances(
#'   time_points = c(1, 2, 4, 5),
#'   items = 1:25,
#'   equal = TRUE
#' )
#' fn <- tempfile()
#' fileConn <- file(fn)
#' writeLines(text = y, con = fn)
#' close(fileConn)
#' readLines(fn)
#' @noRd
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance covariances
.Covariances <- function(time_points,
                         items,
                         equal) {
  ni <- length(items)
  time_pairs <- utils::combn(
    x = time_points,
    m = 2
  )
  n_pairs <- dim(time_pairs)[2]
  covariances <- matrix(
    data = NA,
    nrow = ni,
    ncol = n_pairs
  )
  for (i in seq_len(ni)) {
    for (j in seq_len(n_pairs)) {
      if (equal) {
        covariances[i, j] <- paste0(
          "y",
          time_pairs[1, j],
          "_",
          items[i],
          " ~~ ",
          "c",
          items[i],
          " * ",
          "y",
          time_pairs[2, j],
          "_",
          items[i]
        )
      } else {
        covariances[i, j] <- paste0(
          "y",
          time_pairs[1, j],
          "_",
          items[i],
          " ~~ ",
          "y",
          time_pairs[2, j],
          "_",
          items[i]
        )
      }
    }
  }
  dim(covariances) <- NULL
  covariances <- paste0(
    "\n",
    covariances,
    collapse = "\n"
  )
  return(covariances)
}

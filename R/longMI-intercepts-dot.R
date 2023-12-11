#' Measurement Model Intercepts
#'
#' Specify the measurement model intercepts in the `lavaan` model syntax.
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
#'   intercepts are constrained to be equal across time.
#'   If `equal = FALSE`,
#'   intercepts for `first_item` are constrained to be equal across time
#'   (for identification) and the rest of the intercepts are free to vary.
#' @examples
#' # equal = FALSE
#' y <- .Intercepts(
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
#' y <- .Intercepts(
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
#' @keywords manSASInvariance intercepts
.Intercepts <- function(time_points,
                        items,
                        first_item,
                        equal) {
  nt <- length(time_points)
  ni <- length(items)
  intercepts <- matrix(
    data = NA,
    nrow = ni,
    ncol = nt
  )
  for (i in seq_len(ni)) {
    for (j in seq_len(nt)) {
      constant <- paste0(
        "i",
        i,
        " * 1"
      )
      if (!equal) {
        if (!(i %in% first_item)) {
          constant <- 1
        }
      }
      intercepts[i, j] <- paste0(
        "y",
        time_points[j],
        "_",
        items[i],
        " ~ ",
        constant
      )
    }
  }
  dim(intercepts) <- NULL
  intercepts <- paste0(
    "\n",
    intercepts,
    collapse = "\n"
  )
  return(intercepts)
}

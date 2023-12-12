#' Factor Loadings
#'
#' Specify the factor loadings in the `lavaan` model syntax.
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
#' @param factor_loadings List with length equal to the number of factors.
#'   Each element of the list is the item number
#'   of items for the specific factor.
#' @param equal Logical.
#'   If `equal = TRUE`, factor loadings are constrained to be equal across time.
#'   If `equal = FALSE`, factor loadings for the first item per factor
#'   are constrained to be equal across time
#'   (for identification) and the rest of the factor loadings are free to vary.
#' @examples
#' # equal = FALSE
#' y <- .Loadings(
#'   time_points = c(1, 2, 4, 5),
#'   factor_loadings = list(
#'     1:5,
#'     6:10,
#'     11:15,
#'     16:20,
#'     21:25
#'   ),
#'   equal = FALSE
#' )
#' fn <- tempfile()
#' fileConn <- file(fn)
#' writeLines(text = y, con = fn)
#' close(fileConn)
#' readLines(fn)
#'
#' # equal = TRUE
#' y <- .Loadings(
#'   time_points = c(1, 2, 4, 5),
#'   factor_loadings = list(
#'     1:5,
#'     6:10,
#'     11:15,
#'     16:20,
#'     21:25
#'   ),
#'   equal = TRUE
#' )
#' fn <- tempfile()
#' fileConn <- file(fn)
#' writeLines(text = y, con = fn)
#' close(fileConn)
#' readLines(fn)
#' @noRd
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance loadings
.Loadings <- function(time_points,
                      factor_loadings,
                      equal) {
  items <- sort(
    unique(
      unlist(factor_loadings)
    )
  )
  nf <- length(factor_loadings)
  nt <- length(time_points)
  ni <- length(items)
  factors <- seq_len(nf)
  loadings <- array(
    data = NA,
    dim = c(ni, nf, nt)
  )
  for (i in seq_len(nt)) {
    for (j in seq_len(nf)) {
      items_factor <- factor_loadings[[j]]
      first_item <- items_factor[1]
      for (k in seq_len(ni)) {
        if (items[k] %in% items_factor) {
          if (equal) {
            if (k == first_item) {
              loadings[k, j, i] <- paste0(
                "f",
                factors[j],
                "t",
                time_points[i],
                " =~ ",
                "NA",
                " * ",
                "y",
                time_points[i],
                "_",
                items[k],
                " + ",
                "l",
                factors[j],
                items[k],
                " * ",
                "y",
                time_points[i],
                "_",
                items[k]
              )
            } else {
              loadings[k, j, i] <- paste0(
                "f",
                factors[j],
                "t",
                time_points[i],
                " =~ ",
                "l",
                factors[j],
                items[k],
                " * ",
                "y",
                time_points[i],
                "_",
                items[k]
              )
            }
          } else {
            if (k == first_item) {
              loadings[k, j, i] <- paste0(
                "f",
                factors[j],
                "t",
                time_points[i],
                " =~ ",
                "NA",
                " * ",
                "y",
                time_points[i],
                "_",
                items[k],
                " + ",
                "l",
                factors[j],
                items[k],
                " * ",
                "y",
                time_points[i],
                "_",
                items[k]
              )
            } else {
              loadings[k, j, i] <- paste0(
                "f",
                factors[j],
                "t",
                time_points[i],
                " =~ ",
                "y",
                time_points[i],
                "_",
                items[k]
              )
            }
          }
        }
      }
    }
  }
  dim(loadings) <- NULL
  loadings <- loadings[stats::complete.cases(loadings)]
  loadings <- paste0(
    "\n",
    loadings,
    collapse = "\n"
  )
  return(loadings)
}

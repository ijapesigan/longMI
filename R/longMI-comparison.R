#' Comparison Measurement Invariance Models
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param configural Fitted configural invariance model.
#' @param weak Fitted weak invariance model.
#' @param strong Fitted strong invariance model.
#' @param strict Fitted strict invariance model.
#' @param ... Additional arguments to pass to [lavaan::lavTestLRT()].
#' @examples
#' data("osbornesudick1972", package = "longMI")
#' configural_fit <- Configural(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' weak_fit <- Weak(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' strong_fit <- Strong(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' strict_fit <- Strict(
#'   data = osbornesudick1972,
#'   time_points = c(1, 6),
#'   factor_loadings = list(
#'     c(1, 2, 3, 4)
#'   )
#' )
#' mi <- Comparison(
#'   configural = configural_fit,
#'   weak = weak_fit,
#'   strong = strong_fit,
#'   strict = strict_fit
#' )
#' print(mi)
#' summary(mi)
#' @export
#' @family Longitudinal Measurement Invariance Functions
#' @keywords manSASInvariance comparison
Comparison <- function(configural = NULL,
                       weak = NULL,
                       strong = NULL,
                       strict = NULL,
                       ...) {
  models <- rep(x = NA, times = 4)
  fit <- c(
    configural = configural,
    weak = weak,
    strong = strong,
    strict = strict
  )
  names(models) <- c(
    "configural",
    "weak",
    "strong",
    "strict"
  )
  if (!is.null(configural)) {
    models["configural"] <- 1
  }
  if (!is.null(weak)) {
    models["weak"] <- 1
  }
  if (!is.null(strong)) {
    models["strong"] <- 1
  }
  if (!is.null(strict)) {
    models["strict"] <- 1
  }
  models <- models[stats::complete.cases(models)]
  if (length(models) < 2) {
    stop(
      "Provide at least two models as arguments."
    )
  }
  pairs <- as.data.frame(
    utils::combn(
      x = names(models),
      m = 2
    )
  )
  diff <- lapply(
    X = pairs,
    FUN = function(x,
                   fit,
                   ...) {
      return(
        lavaan::lavTestLRT(
          fit[[x[1]]],
          fit[[x[2]]],
          ...,
          model.names = c(x[1], x[2])
        )
      )
    },
    fit = fit,
    ...
  )
  diff_names <- lapply(
    X = pairs,
    FUN = function(x) {
      return(
        paste(x[1], "against", x[2])
      )
    }
  )
  dim(diff_names) <- NULL
  names(diff) <- diff_names

  fit_measures <- lapply(
    X = names(models),
    FUN = function(x) {
      lavaan::inspect(
        object = fit[[x]],
        what = "fit.measures"
      )
    }
  )
  names(fit_measures) <- names(models)
  fit_measures <- do.call(
    what = "cbind",
    args = fit_measures
  )
  out <- list(
    fit = fit,
    fit_measures = fit_measures,
    difference = diff
  )
  class(out) <- c(
    "longmi",
    class(out)
  )
  return(
    out
  )
}

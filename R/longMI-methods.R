#' Print Method for an Object of Class `longmi`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   selected fit measures.
#'
#' @param x Object of class `longmi`
#'   that is, the output of the [longMI::Invariance()]
#'   or the [longMI::Comparison()] functions.
#' @param measures Vector of fit measures.
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
                         ),
                         digits = 4,
                         ...) {
  cat("Call:\n")
  base::print(x$call)
  cat("\n")
  base::print(
    round(
      .FitMeasures(
        object = x,
        measures = measures
      ),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `longmi`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a list of
#'   the summary of the fitted models.
#'
#' @param object Object of class `longmi`
#'   that is, the output of the [longMI::Invariance()]
#'   or the [longMI::Comparison()] functions.
#' @param measures Vector of fit measures.
#' @param ... additional arguments to pass to the summary function
#'   in `lavaan`
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
                           ),
                           ...) {
  cat("Call:\n")
  base::print(object$call)
  measures <- .FitMeasures(
    object = object,
    measures = measures
  )
  cat("\nFit Measures\n")
  base::print(
    round(
      x = measures,
      digits = 4
    )
  )
  cat("\n")
  fit <- object$fit
  fit <- fit[!sapply(X = fit, FUN = is.null)]
  models <- names(fit)
  varnames <- paste(
    toupper(models),
    "INVARIANCE MODEL"
  )
  out <- lapply(
    X = seq_len(length(fit)),
    FUN = function(i) {
      cat(paste0("\n\n", varnames[i], "\n\n"))
      out <- lavaan::summary(fit[[i]])
      print(out)
      return(out)
    }
  )
  invisible(
    list(
      measures = measures,
      summary = out
    )
  )
}

#' Model Comparison Method for an Object of Class `longmi`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a data frame of chi-square difference test results.
#'
#' @param object Object of class `longmi`
#'   that is, the output of the [longMI::Invariance()]
#'   or the [longMI::Comparison()] functions.
#' @param ... Additional arguments to pass to [lavaan::lavTestLRT()].
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
#' anova(mi)
#'
#' @keywords methods
#' @export
anova.longmi <- function(object,
                         ...) {
  cat("Call:\n")
  base::print(object$call)
  fit <- object$fit
  fit <- fit[!sapply(X = fit, FUN = is.null)]
  models <- names(fit)
  pairs <- as.data.frame(
    utils::combn(
      x = models,
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
  out <- do.call(
    what = "rbind",
    args = diff
  )
  varnames <- gsub(
    pattern = "V",
    replacement = "",
    x = rownames(out)
  )
  rownames(out) <- varnames
  print(out)
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
  invisible(diff)
}

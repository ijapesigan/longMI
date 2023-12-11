## ---- test
lapply(
  X = 1,
  FUN = function(i,
                 tol,
                 text) {
    message(text)
    if (!exists("osbornesudick1972")) {
      try(
        data(
          "osbornesudick1972",
          package = "longMI"
        ),
        silent = TRUE
      )
    }
    time_points <- c(1, 6)
    factor_loadings <- list(
      c(1, 2, 3, 4)
    )
    configural_fit <- Configural(
      data = osbornesudick1972,
      time_points = time_points,
      factor_loadings = factor_loadings,
      mimic = "mplus"
    )
    lavaan::summary(configural_fit)
    weak_fit <- Weak(
      data = osbornesudick1972,
      time_points = time_points,
      factor_loadings = factor_loadings,
      mimic = "mplus"
    )
    lavaan::summary(weak_fit)
    strong_fit <- Strong(
      data = osbornesudick1972,
      time_points = time_points,
      factor_loadings = factor_loadings,
      mimic = "mplus"
    )
    lavaan::summary(strong_fit)
    strict_fit <- Strict(
      data = osbornesudick1972,
      time_points = time_points,
      factor_loadings = factor_loadings,
      mimic = "mplus"
    )
    lavaan::summary(strict_fit)
    mi_comparison <- Comparison(
      configural = configural_fit,
      weak = weak_fit,
      strong = strong_fit,
      strict = strict_fit
    )
    npar_mi_comparison <- mi_comparison$fit_measures["npar", ]
    chisq_mi_comparison <- mi_comparison$fit_measures["chisq", ]
    mi_invariance <- Invariance(
      data = osbornesudick1972,
      time_points = time_points,
      factor_loadings = factor_loadings
    )
    npar_mi_invariance <- mi_invariance$fit_measures["npar", ]
    chisq_mi_invariance <- mi_invariance$fit_measures["chisq", ]
    testthat::test_that(
      paste(text, "npar comparison"),
      {
        testthat::expect_true(
          all(
            abs(
              as.integer(npar_mi_comparison) - c(25, 22, 19, 15)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "npar invariance"),
      {
        testthat::expect_true(
          all(
            abs(
              as.integer(npar_mi_invariance) - c(25, 22, 19, 15)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "chisq comparison"),
      {
        testthat::expect_true(
          all(
            abs(
              round(
                chisq_mi_comparison,
                digits = 3
              ) - c(25.968, 41.897, 53.723, 134.559)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "chisq invariance"),
      {
        testthat::expect_true(
          all(
            abs(
              round(
                chisq_mi_invariance,
                digits = 3
              ) - c(25.968, 41.897, 53.723, 134.559)
            ) <= tol
          )
        )
      }
    )
    configural_fit <- Configural(
      data = osbornesudick1972,
      time_points = time_points,
      factor_loadings = factor_loadings,
      covariances = TRUE,
      model_add = "", # coverage
      mimic = "mplus"
    )
    measures <- lavaan::inspect(
      object = configural_fit,
      what = "fit.measures"
    )
    testthat::test_that(
      paste(text, "npar covariances"),
      {
        testthat::expect_true(
          all(
            abs(
              as.integer(measures["npar"]) - 29
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "chisq covariances"),
      {
        testthat::expect_true(
          all(
            abs(
              round(measures["chisq"], digits = 3) - 24.882
            ) <= tol
          )
        )
      }
    )
  },
  tol = 0.001,
  text = "test"
)

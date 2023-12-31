% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/longMI-strong.R
\name{Strong}
\alias{Strong}
\title{Strong Invariance Model}
\usage{
Strong(
  data,
  time_points,
  factor_loadings,
  covariances = FALSE,
  model_add = NULL,
  ...
)
}
\arguments{
\item{data}{Dataframe.
The function assumes that the data is in the wide format
and the variables are named as follows:
\code{paste0("y", time_point, "_", item_number)}.
For example, for the item 1 from the first time point,
the variable name should be \code{y1_1}.}

\item{time_points}{Numeric vector of discrete time points.}

\item{factor_loadings}{List with length equal to the number of factors.
Each element of the list is the item number of items
for the specific factor.}

\item{covariances}{Logical.
If \code{covariance = TRUE},
model the covariances of the measurement error.}

\item{model_add}{Additional specification added to the lavaan model syntax.}

\item{...}{Additional arguments to pass to \code{\link[lavaan:cfa]{lavaan::cfa()}}.}
}
\value{
Returns a fitted \code{lavaan} object.
}
\description{
Strong Invariance Model
}
\examples{
data("osbornesudick1972", package = "longMI")
strong_fit <- Strong(
  data = osbornesudick1972,
  time_points = c(1, 6),
  factor_loadings = list(
    c(1, 2, 3, 4)
  )
)
library(lavaan)
summary(strong_fit)
}
\seealso{
Other Longitudinal Measurement Invariance Functions: 
\code{\link{Comparison}()},
\code{\link{Configural}()},
\code{\link{Invariance}()},
\code{\link{Strict}()},
\code{\link{Weak}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Longitudinal Measurement Invariance Functions}
\keyword{manSASInvariance}
\keyword{strong}
